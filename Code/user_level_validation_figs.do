// Create figure for user-level validation of signalling algorithm
set scheme plotplainblind
clear all


local output "$path_overleaf/VS Replication Copy/Figures"
cd "C:/Users/deivi/github/vs_nature_replication"


local title "title("")"
local img pdf
graph set window fontface "Times New Roman"
local title " "
local axis_size " "




// Loop over mean/max classification and signal types
foreach type in mean max {
	use Data/validation, clear
		local Nacademics = _N
		local Nchecks = r(N)

	local i=1
		



		count if signaller==1
		local NSig = r(N)
		count if signaller==0
		local NNon = r(N)

		count if questions_max==1
		count if questions_mean==1
		bys signaller: su heavy_support_mean
		
		// Run regressions to see difference between signallers and non-signallers
		foreach var of varlist opposes_`type' questions_`type' neutral_`type' ///
			some_support_`type' heavy_support_`type' any_support_`type'  {
				
			reg `var' nonsignaller signaller, nocons robust
			
			preserve
				parmest, fast
				g outcome = "`var'"
				tempfile `var'
				save ``var''
			restore
		}
		
		clear
		foreach x in opposes_`type' questions_`type' neutral_`type' ///
			some_support_`type' heavy_support_`type' any_support_`type' {
				
			append using ``x''
		}
		g order = _n + round(_n/2,1)-1

		replace estimate = estimate*100
		replace min95 = min95*100
		replace max95 = max95*100
		

		// Now graph
		twoway (bar estimate order if inlist(order,1,4,7,10,13,16), ///
				barwidth(0.8) bcolor(black*0.3)) ///
			(bar estimate order if inlist(order,2,5,8,11,14,17), ///
				barwidth(0.8) bcolor(vermillion*0.6)) ///
			(rcap min95 max95 order, lwidth(0.3) lcolor(vermillion)), ///				
			xtitle("") ///
			`title' ///
			legend(label(1 "Silent (Auto-Classified, N=`NNon')") ///
					label(2 "Vocal (Auto-Classified, N=`NSig')") ///
					pos(6) order(1 2) rows(1)) ///
			xlabel(1.5 "Opposes" 4.5 "Questions" 7.5 "Neutral" 10.5 `""Some" "Support""' 13.5 `""Heavy" "Support""' ///
				16.5 `""Any" "Support""', nogrid notick labsize(4)) ///
			ylabel(0(20)100, labsize(3.5)) ///
			xscale(range(0 12)) ///
			ytitle("Percentage of Users Manually Classified", size(4)) ///
			text(90 12.5 "Number of Academics = `Nacademics'", size(3))
		graph export "`output'/user_level_validation_`type'_sigRaceBLMPpl.`img'", replace

		local i=`i'+1
	
}



		
