library(here)
library(Synth)
library(dplyr)
library(stargazer)
library(ggplot2)
library(nlme)
library(haven)
library(xtable)
library(arsenal)
library(labelled)

setwd(here("/PS5"))

tsdat <- read_dta("traffic_safety_ps5.dta")

#### (a)
### i: Compare pre-treatment period log traffic fatalities of TU and controls

# Create a new column for log vehicle fatalities
tsdat <- tsdat %>% mutate(log_fatalities_pc = log(fatalities/population))

# See the corresponding State labels
tsdat <- tsdat %>% group_by(year) %>%
  mutate(state_name = ifelse(state == 99, "TU", names(val_labels(state))[state])) %>% ungroup()

# Use an intermediary dataframe to create dataframe of only control states
tsdat2 <- tsdat %>% group_by(state) %>% filter(year <= 1986 & primary != 0)
tsdat_ctrl <- anti_join(tsdat, tsdat2, by ="state")


# Summarise the mean log fatalities for TU and controls
TU_mean_lfat <- tsdat %>% filter(state == 99 & year <1986) %>% summarise(mean(log_fatalities_pc))
ctrl_mean_lfat <- tsdat_ctrl %>% filter(year < 1986) %>% summarise(mean(log_fatalities_pc))

# Create table of the two
tab1 <- cbind(ctrl_mean_lfat, TU_mean_lfat)
colnames(tab1) <- c("Control States", "Treatment")
row.names(tab1) <- "Mean Log Fatalities Per Capita"
tab1
#xtable(tab1) #latex output

## Create visualization of pre-trends
# Create a column in the full dataframe that indicates whether control or TU
yearly_means_ctrl <- tsdat_ctrl %>% group_by(year) %>% summarise(mean(log_fatalities_pc))
colnames(yearly_means_ctrl) <- c("year", "log_fatalities_pc")


# ggplot
ggplot(data = subset(tsdat, state == "99"), aes(x=year, y=log_fatalities_pc, color = "TU")) + 
  geom_point() + geom_line()  +
  geom_line(data = yearly_means_ctrl, aes(color = "Control")) + 
  geom_point(data = yearly_means_ctrl, color = "red") +
  xlim(1981,1985) + 
  xlab("Year") + ylab("Log Traffic Fatatlities Per Capita") +
  labs(colour = "Group") +
  ggtitle("Comparison of Mean Log Fatalities Rates for TU and Control in Pre-Treatment Period (<1986)") + 
  theme_minimal()

### ii: Look at MSE of log fatalities of TU and each control state across 5 yrs
yearly_lf_TU <- tsdat %>% filter(year < 1986 & state ==99) %>% select(log_fatalities_pc)
yearly_lf_TU <- as.matrix(yearly_lf_TU)

MSE_ctrl <- tsdat_ctrl %>% filter(year < 1986) %>% select(state, year, log_fatalities_pc) %>% 
  group_by(state) %>% mutate(sqe = (log_fatalities_pc-yearly_lf_TU)^2) %>% summarise(mean(sqe)) %>%
  arrange(`mean(sqe)`)
head(MSE_ctrl, 2)
#xtable(head(MSE_ctrl,1))

## The smallest MSE belongs to state 8 -- FL
# Look at covariate balance between state 8 and TU
balance_8_TU <- tsdat %>% filter(year < 1986 & state ==8) %>% rbind(subset(tsdat, state ==99 & year < 1986))

balance_8_TU_tbl <- tableby(as.factor(state) ~college + beer + secondary + 
                              population + unemploy + totalvmt + precip + snow32 +
                              rural_speed + urban_speed, data = balance_8_TU)
summary(balance_8_TU_tbl, text = T)
#write2html(balance_8_TU_tbl, file = "ps5q1b.html" )


#### Part b
### ii: Carry out synthetic control using Synth
## First use "dataprep" command to create list of matrices
tsdat <- as.data.frame(tsdat)
xxzz <- dataprep(foo = tsdat, 
               predictors=c("college","beer","secondary","population","unemploy","totalvmt","precip","snow32"), 
               dependent = "log_fatalities_pc",
               unit.variable = "state",
               time.variable = "year",
               treatment.identifier = "TU",
               controls.identifier = c(unique(tsdat_ctrl$state)),
               time.predictors.prior = 1981:1985,
               time.optimize.ssr = 1981:1985,
               unit.names.variable = "state_name",
               time.plot = 1981:2003)

#NOTE: Urban Speed and Rural Speed are always excluded because no variation pre-treatment

synth_ctrl <- synth(data.prep.obj = xxzz)
synth_ctrl$loss.v

#What if we use different optimization methods?
#An alternative specification with different optimization methods
xxzz4 <- dataprep(foo = tsdat, 
                  predictors=c("college","beer", "secondary","population","unemploy","totalvmt","precip","snow32"), 
                  dependent = "log_fatalities_pc",
                  unit.variable = "state",
                  time.variable = "year",
                  treatment.identifier = "TU",
                  controls.identifier = c(unique(tsdat_ctrl$state)),
                  time.predictors.prior = 1981:1985,
                  time.optimize.ssr = 1981:1985,
                  unit.names.variable = "state_name",
                  time.plot = 1981:2003)

synth_ctrl4 <- synth(data.prep.obj = xxzz4,
                     optimxmethod = c("CG", "L-BFGS-B", "nlm", "nlminb", "spg", "ucminf"))
synth_ctrl4$loss.v
#MSPE gets smaller

# Further investigation reveals "nlm" is the method responsible for generating the lowest MSPE.
# Use only "nlm" to save computational power for rest "Alt. Opt." specifications.

synthtab4 <- synth.tab(synth.res = synth_ctrl4,
          dataprep.res = xxzz4,
          round.digit = 5)
synthtab4$tab.v


# What if we get rid of the least heavily weighted varible? (Precip)
# An alternative specification with different optimization methods AND no precip
xxzz3 <- dataprep(foo = tsdat, 
                  predictors=c("college","beer","secondary","population","unemploy","totalvmt","snow32"), 
                  dependent = "log_fatalities_pc",
                  unit.variable = "state",
                  time.variable = "year",
                  treatment.identifier = "TU",
                  controls.identifier = c(unique(tsdat_ctrl$state)),
                  time.predictors.prior = 1981:1985,
                  time.optimize.ssr = 1981:1985,
                  unit.names.variable = "state_name",
                  time.plot = 1981:2003)

synth_ctrl3 <- synth(data.prep.obj = xxzz3,
                     optimxmethod = c("nlm"))

synth_ctrl3$loss.v

#MSPR is larger. Decide not to drop precip.

# Let's try dropping "secondary" since it's arbitrarily matched with most states anyways
xxzz2 <- dataprep(foo = tsdat, 
                  predictors=c("college","beer","population","unemploy","totalvmt","precip","snow32"), 
                  dependent = "log_fatalities_pc",
                  unit.variable = "state",
                  time.variable = "year",
                  treatment.identifier = "TU",
                  controls.identifier = c(unique(tsdat_ctrl$state)),
                  time.predictors.prior = 1981:1985,
                  time.optimize.ssr = 1981:1985,
                  unit.names.variable = "state_name",
                  time.plot = 1981:2003)

synth_ctrl2 <- synth(data.prep.obj = xxzz2,
                     optimxmethod =  c("nlm"))

synth_ctrl2$loss.v < synth_ctrl4$loss.v
#The MSPE is very slightly higher now. Reject this specification.

#One last check to see if taking population out will make a difference since
#Dependent variable is already normalized to population
xxzz1 <- dataprep(foo = tsdat, 
                  predictors=c("college","beer","secondary","unemploy","totalvmt","precip","snow32"), 
                  dependent = "log_fatalities_pc",
                  unit.variable = "state",
                  time.variable = "year",
                  treatment.identifier = "TU",
                  controls.identifier = c(unique(tsdat_ctrl$state)),
                  time.predictors.prior = 1981:1985,
                  time.optimize.ssr = 1981:1985,
                  unit.names.variable = "state_name",
                  time.plot = 1981:2003)

synth_ctrl1 <- synth(data.prep.obj = xxzz1,
                     optimxmethod = c("nlm"))

synth_ctrl1$loss.v < synth_ctrl4$loss.v

# The MSPE is actually lower in this specification! This is the new preferred spec.

# Actual last specification check. What if run without population using old optimization?

xxzz0 <- dataprep(foo = tsdat, 
                  predictors=c("college","beer","secondary","unemploy","totalvmt","precip","snow32"), 
                  dependent = "log_fatalities_pc",
                  unit.variable = "state",
                  time.variable = "year",
                  treatment.identifier = "TU",
                  controls.identifier = c(unique(tsdat_ctrl$state)),
                  time.predictors.prior = 1981:1985,
                  time.optimize.ssr = 1981:1985,
                  unit.names.variable = "state_name",
                  time.plot = 1981:2003)

synth_ctrl0 <- synth(data.prep.obj = xxzz0)
synth_ctrl0$loss.v < synth_ctrl1$loss.v

# Original Specification still gives a (very slightly) higher MSPE
# Choose "synth_ctrl1" as the "preferred specification"
# Create some summary visuals and tables of the synthetic control

path.plot(synth.res = synth_ctrl1,
          dataprep.res = xxzz,
          Z.plot = FALSE,
          tr.intake = 1986,
          Legend = c("TU", "Synthetic"),
          Xlab = c("Year"),
          Ylab = c("Log Fatalities Per Capita"),
          Main = c("Log Fatalities Per Capita by Year: TU vs. Synthetic Control"),
          Ylim = c(-1,-2))


tab2 <- synth.tab(synth.res = synth_ctrl1,
                  dataprep.res = xxzz1,
                  round.digit = 5)

MSPEs <- c(synth_ctrl$loss.v, synth_ctrl4$loss.v, synth_ctrl3$loss.v, synth_ctrl2$loss.v, synth_ctrl1$loss.v,
           synth_ctrl0$loss.v)
MSPE_labels <- c("Basic: Full", "Alt Optimization: Full", "Alt Opt: No Precip", 
                 "Alt Opt: No Secondary", "Alt Opt: No Population", "Basic: No Population ")
MSPE_Summ <- as.table(cbind(MSPE_labels, signif(MSPEs,5)))

#xtable(MSPE_Summ, digits = 5, caption = "MSPE of Different Specifications")

#xtable(tab2$tab.pred, digits = 5, caption = "Balance Table of Predictor Variables")
#xtable(tab2$tab.v, digits = 5, caption = "Predictor Variable Weights")
#xtable(tab2$tab.w %>% arrange(desc(w.weights)) %>% filter(w.weights > 0.005),
#       digits = 5, caption = "Control State Weights ($>0.005$)")


#### Part c
### i: Gap plots

# Preferred specification
gaps.plot(synth.res = synth_ctrl1,
          dataprep.res = xxzz1,
          Z.plot = FALSE,
          tr.intake = 1986,
          Xlab = c("Year"),
          Ylab = c("Gap (Log Fatalities Per Capita)"),
          Main = c("Gap in Log Fatalities Per Capita by Year: TU vs.Synthetic Control (Preferred)"))

gaps.plot(synth.res = synth_ctrl,
          dataprep.res = xxzz,
          Z.plot = FALSE,
          tr.intake = 1986,
          Xlab = c("Year"),
          Ylab = c("Gap (Log Fatalities Per Capita)"),
          Main = c("Gap in Log Fatalities Per Capita by Year: TU vs. Synthetic Control (Default Optimization Method)"))

gaps.plot(synth.res = synth_ctrl3,
          dataprep.res = xxzz3,
          Z.plot = FALSE,
          tr.intake = 1986,
          Xlab = c("Year"),
          Ylab = c("Gap (Log Fatalities Per Capita)"),
          Main = c("Gap in Log Fatalities Per Capita by Year: TU vs. Synthetic Control (Alt Opt: No Precipitation)"))

gaps.plot(synth.res = synth_ctrl2,
          dataprep.res = xxzz2,
          Z.plot = FALSE,
          tr.intake = 1986,
          Xlab = c("Year"),
          Ylab = c("Gap (Log Fatalities Per Capita)"),
          Main = c("Gap in Log Fatalities Per Capita by Year: TU vs. Synthetic Control (Alt Opt: No Secondary)"))

### ii: Do the same gap plots but for all control states instead
tsdat_ctrl <- as.data.frame(tsdat_ctrl)

get_synth_proj <- function(name, dat) {
  controls <- dat %>% filter(state_name != name)
  temp_xxzz <- dataprep(foo = dat, 
                        predictors=c("college","beer","secondary","unemploy","totalvmt","precip","snow32"), 
                        dependent = "log_fatalities_pc",
                        unit.variable = "state",
                        time.variable = "year",
                        treatment.identifier = name,
                        controls.identifier = c(unique(controls$state)),
                        time.predictors.prior = 1981:1985,
                        time.optimize.ssr = 1981:1985,
                        unit.names.variable = "state_name",
                        time.plot = 1981:2003)
  
  temp_synth_ctrl <- synth(data.prep.obj = temp_xxzz,
                           optimxmethod = c("nlm"))
  
  synth_projection <- temp_xxzz$Y0plot %*% temp_synth_ctrl$solution.w
  return(synth_projection)
}

ctrl_synth_proj <- tsdat_ctrl %>% group_by(state_name, year) %>%
  mutate(proj = purrr::map(tsdat_ctrl, get_synth_proj(dat = tsdat_ctrl, name = state_name))) %>% unnest()

states <- as.data.frame(c(unique(tsdat_ctrl$state_name)))
colnames(states) <- c("state_name")
states <- states %>% mutate(proj = get_synth_proj(name = state_name, dat = tsdat_ctrl))

datalist <- list()

for(i in c(unique(tsdat_ctrl$state_name))[1:25]) { 
  dat <- as.data.frame(c(1981:2003))
  dat$i <- get_synth_proj(name = i, dat = tsdat_ctrl)
  datalist[[i]] <- dat
}

big_data <- do.call(rbind, datalist)

# Must exclude NJ because no variation
# Also, must exclude UT, and all states after including and after WA 
# because incompatible with used optimization method

datalist2 <- list()
for(i in c(unique(tsdat_ctrl$state_name))[c(27:35)]) { 
  dat <- as.data.frame(c(1981:2003))
  dat$i <- get_synth_proj(name = i, dat = tsdat_ctrl)
  datalist2[[i]] <- dat
}

big_data2 <- do.call(rbind, datalist2)

big_data_all <- rbind(big_data, big_data2)
big_data_all$state_name <- substr(rownames(big_data_all),1,2)

colnames(big_data_all) <- c("year", "synth", "state_name")
big_data_all$year <- as.numeric(big_data_all$year)

gaps <- left_join(big_data_all, tsdat_ctrl, by = c("year", "state_name"))
gaps <- gaps %>% mutate(gap = log_fatalities_pc - synth)

## Create another dataframe for the actual TU
TU_gaps <- tsdat %>% filter(state == 99) %>% select(state_name, year, log_fatalities_pc)
TU_proj <- xxzz1$Y0plot %*% synth_ctrl1$solution.w
TU_gaps <- cbind(TU_gaps, TU_proj)
TU_gaps <- TU_gaps %>% mutate(gap = log_fatalities_pc - TU_proj)

# The desired plot of gaps

ggplot(data = gaps, aes(x = year, y = gap, group = state_name)) +
  geom_line(aes(color = "grey"),alpha = 0.8) +
  geom_line(data = TU_gaps, aes(colour = "dark green")) + 
  scale_colour_manual(name = 'State', 
                      values =c('dark green'='dark green','grey'='grey'), labels = c('TU','Controls')) +
  geom_vline(xintercept = 1986, linetype = 'dashed') + 
  geom_hline(yintercept = 0, linetype = 'solid') + 
  labs(x = "Year", y = "Gap (Log Fatalities Per Capita)",title = "Placebo Test") + 
  theme_minimal()

       