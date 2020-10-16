// Question 1
// import data
use "\\itsnas\udesk\users\lukechen\Documents\simulated0_2020.dta", clear

// estimate regressions
eststo: quietly regress y0 treat
eststo: quietly regress y0 treat, cluster(group)

// see results in stata
esttab, se ar2

// output results to a .tex file
esttab using question1.tex, se ar2 ///
	label replace ///
	nonumbers mtitle("No Cluster" "Cluster"[, depvars]) ///
	title("Regression Results" \label{tab1})
eststo clear

// Question 2
// import data
use "\\itsnas\udesk\users\lukechen\Documents\simulated1_2020.dta", clear

// Part a and b
// Estimate OLS regressions with and without clustered SE
eststo: quietly regress y treat
eststo: quietly regress y treat, cluster(group)

// see results in stata
esttab, se ar2

// output results to a .tex file
esttab using question2ab.tex, se ar2 ///
	label replace ///
	nonumbers mtitle("No Cluster" "Cluster"[, depvars]) ///
	title("Regression Results" \label{tab2})
eststo clear

// Part d. i, ii: Get b_1, a bootstrapped sample
use simulated1_2020.dta, clear
bsample 900
eststo: quietly regress y treat
esttab using question2d.tex, se ar2 ///
	label replace ///
	title("Example Bootstrap Results" \label{tab3})
eststo clear
	
// Part d. iii, iv
// Bootstrap sample estimates
local boots = 1000
clear
set obs `boots'
g store_dat = .

forvalues i = 1(1)`boots' {
		if floor((`i'-1)/100) == (`i'-1)/100 {
		display "Booting `i' out of `boots'"
		}
		preserve
		
		use simulated1_2020.dta, clear
		
		bsample 900
		
		quietly: regress y treat
		local b1 = _b[treat]
		
		restore
		
		quietly replace store_dat = `b1' in `i'
	}
display "Bootstrapping Done"

// Calculate bootstrapped variance
egen betamean = mean(store_dat)
g sqdif = (store_dat - betamean)^2
quietly: summarize sqdif
g bs_var = (1/999)*r(sum)
g bs_sd = sqrt(bs_var)
display bs_sd
clear

// Part e. redo with clustering
// i, ii
use simulated1_2020.dta, clear
bsample, cluster(group)
eststo: quietly regress y treat
esttab using question2e.tex, se ar2 ///
	label replace ///
	title("Example Clustered Bootstrap Results" \label{tab4})
eststo clear

// iii, iv
// Bootstrap sample estimates
local boots = 1000
clear
set obs `boots'
g cluster_bs = .

forvalues i = 1(1)`boots' {
		if floor((`i'-1)/100) == (`i'-1)/100 {
		display "Booting `i' out of `boots'"
		}
		preserve
		
		use simulated1_2020.dta, clear
		
		bsample , cluster(group)
		
		quietly: regress y treat
		local b1 = _b[treat]
		
		restore
		
		quietly replace cluster_bs = `b1' in `i'
	}
display "Bootstrapping Done"

// Calculate bootstrapped standard error
egen betamean = mean(cluster_bs)
g diff2 = (cluster_bs - betamean)^2
quietly: summarize diff2
g bs2_var = (1/999)*r(sum)
g bs2_sd = sqrt(bs2_var)
display bs2_sd
clear

// Question f
// Get 100 sets of B=5

clear
g sqdiff = .
foreach k in 5 10 15 20 30 50 80 100{

	local boots = `k'
	
	qui g cluster_bs_`k' = .
	qui g set_`k' = .
	set obs 100
	forvalues j = 1(1)100 {
		display "Running iteration `j' out of 100 for set `k'"
			forvalues i = 1(1)`boots' {
					preserve
					
					use simulated1_2020.dta, clear
					
					bsample , cluster(group)
					
					quietly: regress y treat
					local b1 = _b[treat]
					
					restore
					
					quietly replace cluster_bs_`k' = `b1' in `i'
					
					quietly summarize cluster_bs_`k'
					qui replace sqdiff = (cluster_bs_`k' - r(mean))^2 in `i'
				}
		quietly summarize sqdiff
		local bs_sd = sqrt(r(sum)/(`k'-1))
		
		quietly replace set_`boots' = `bs_sd' in `j'
	}
}
drop cluster_*
drop sqdiff

foreach n in 5 10 15 20 30 50 80 100 {
la var set_`n' "B=`n'"
}

graph box set_*, showyvars legend(off) ytitle("Bootstrapped OLS Coeff. SE") ///
	title("Distribution of SE's by number of bootstrap samples (B)")
	
graph export "PS4_graph2f.eps", as(eps) replace
