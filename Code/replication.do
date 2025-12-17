// Create main figures and tables for "Do Virtue Signals Signal Virtue?"

pause off
cd *REPO PATH HERE*


global figpath *OUTPUT PATH FOR FIGURES*
global tabpath *OUTPUT PATH FOR TABLES*

// switches
	// audit
		local sumstats			        1 // table A1
		local balance			        0 // table A3
		local discrim			        0 // figs 3, 4, 5, A5
		local intensive			        0 // fig A6 and A7
		local conditions		        0 // fig 6
		local other_predictors	        0 // figs 2 and A8
		local detection	                0 // fig A9
		local tweet_type		        0 // table 3
		local pre_postGF				0 // fig 8 and A14
		local tabGF						0 // table 2
		local daily_signallers_2020     0 // fig A13
	// political contributions
		local fec_bars       		    0 // fig 1
	// donation experiment 
		local donations		        	0 // figs 7  and A12
		local infoloss	        		0 // fig 9
		local donationsumstats      	0 // table A2
	// high stakes
		local high_stakes         		0 // table 1 and A4
	// student survey
		local studentsumstats       	0 // table A5
		local predict_fig	   	        0 // fig 10
	// left twitter
		local left_twitter		        0 // figs A10 and A11

// general configs
local title "title("")"
local img pdf
graph set window fontface "Times New Roman"
local title " "
local axis_size " "
pause on


// Fig 1	
if `fec_bars'==1 { // Giving to Black vs. white candidates (or DEM vs. REP committees) 
	
 	// load stacked data, each academic appears twice
	use Data/fec_fig1_anon, clear
	
	// switches
	local audit = 0 // 1 = audit sample only, 0 = full sample
	local treatment = "dem" // dem or black
	local outcome = "anyPartyCont" // 
	
	if "`treatment'"=="dem" { // black/white, dem/rep
		local control = "rep"
	}
	

	if "`outcome'"=="anyPartyCont" | "`outcome'"=="totPartyCont" {
		local treatLab = "Democrat Committees"
		local ctrlLab = "Republican Committees"
	}

		
	g beta = .
	g u95 = .
	g l95 = .
	
	** Overall Giving Gap: non-Black Profs **
	// get control and treatment mean and 95% CI
	reg `outcome' `control' `treatment' if encoded_ethnicity!=1 & nontwitter==0, cluster(id) nocons 
		
	local i=1
	foreach x in control treatment {
		replace beta = _b[``x''] in `i'
		local `x'Mean = _b[``x'']
		replace u95 = _b[``x'']+_se[``x'']*invttail(e(df_r),0.025) in `i'
		replace l95 = _b[``x'']-_se[``x'']*invttail(e(df_r),0.025) in `i'
		local i=`i'+1
	}
		
	// get p-value (don't need to add FE this time)
	test `treatment'=`control'
	if r(p)<0.001 {
		local pAll = "{it:p}<0.001"
	}
	else if r(p)<0.01 {
		local pAll = "{it:p}<0.01"
	}
	else if r(p)>=0.995 {
		local pAll = "{it:p}=1"
	}	
	else {
		local pAll = "{it:p}=0"+string(round(r(p),.01))
	}	


	reg `outcome' `control'Xs3 `control'XnonS3 `treatment'Xs3 `treatment'XnonS3 ///
		if encoded_ethnicity!=1 & nontwitter==0, cluster(id) nocons 

	local i=4
	foreach x in nonS3 s3 {
		foreach y in control treatment {
			replace beta = _b[``y''X`x'] in `i'
			local `y'X`x'Mean = _b[``y''X`x']
			replace u95 = _b[``y''X`x']+_se[``y''X`x']*invttail(e(df_r),0.025) in `i'
			replace l95 = _b[``y''X`x']-_se[``y''X`x']*invttail(e(df_r),0.025) in `i'
			local i=`i'+1
		}
		local i=`i'+1
	}	

	// get p-value
	test `treatment'XnonS3=`control'XnonS3
	
	if r(p)<0.001 {
		local pNonSig = "{it:p}<0.001"
	}
	else if r(p)<0.01 {
		local pNonSig = "{it:p}<0.01"
	}
	else if r(p)>=0.995 {
		local pNonSig = "{it:p}=1"
	}
	else {
		local pNonSig = "{it:p}=0"+string(round(r(p),.01))
	}	
		
	test `control'Xs3 = `treatment'Xs3
	if r(p)<0.001 {
		local pSig = "{it:p}<0.001"
	}
	else if r(p)<0.01 {
		local pSig = "{it:p}<0.01"
	}
	else if r(p)>=0.995 {
		local pSig = "{it:p}=1"
	}	
	else {
		local pSig = "{it:p}=0"+string(round(r(p),.01))
	}		
	
	// get DiD p-value
	reg `outcome' `treatment' s3 `treatment'Xs3, cluster(id)
	test `treatment'Xs3
	if r(p)<0.001 {
		local pDiD = "{it:p}<0.001"
	}
	else if r(p)<0.01 {
		local pDiD = "{it:p}<0.01"
	}
	else if r(p)>=0.995 {
		local pDiD = "{it:p}=1"
	}	
	else {
		local pDiD = "{it:p}=0"+string(round(r(p),.01))
	}	
	
	
	** Giving Gap for Black Profs **
	// get control and treatment mean and 95% CI
	reg `outcome' `control' `treatment' if encoded_ethnicity==1, cluster(id) nocons 
		
	local i=10
	foreach x in control treatment {
		replace beta = _b[``x''] in `i'
		local `x'Mean = _b[``x'']
		replace u95 = _b[``x'']+_se[``x'']*invttail(e(df_r),0.025) in `i'
		replace l95 = _b[``x'']-_se[``x'']*invttail(e(df_r),0.025) in `i'
		local i=`i'+1
	}
	
	// get p-value (don't need to add FE this time)
	test `treatment'=`control'
	if r(p)<0.001 {
		local pBlack = "{it:p}<0.001"
	}
	else if r(p)<0.01 {
		local pBlack = "{it:p}<0.01"
	}
	else if r(p)>=0.995 {
		local pBlack = "{it:p}=1"
	}	
	else {
		local pBlack = "{it:p}=0"+string(round(r(p),.01))
	}	
	
	if strpos("`outcome'","tweet")==0 {
		** Giving Gap for Non-Twitter Non-Black Profs **
		// get control and treatment mean and 95% CI
		reg `outcome' `control' `treatment' if nontwitter==1, cluster(id) nocons 
			
		local i=13
		foreach x in control treatment {
			replace beta = _b[``x''] in `i'
			local `x'Mean = _b[``x'']
			replace u95 = _b[``x'']+_se[``x'']*invttail(e(df_r),0.025) in `i'
			replace l95 = _b[``x'']-_se[``x'']*invttail(e(df_r),0.025) in `i'
			local i=`i'+1
		}
		
		// get p-value (don't need to add FE this time)
		test `treatment'=`control'
		if r(p)<0.001 {
			local pNonTwit = "{it:p}<0.001"
		}
		else if r(p)<0.01 {
			local pNonTwit = "{it:p}<0.01"
		}
		else if r(p)>=0.995 {
			local pNonTwit = "{it:p}=1"
		}	
		else {
			local pNonTwit = "{it:p}=0"+string(round(r(p),.01))
		}	
	}
	
	// plot
	keep in 1/14
	keep beta u95 l95
	
	if strpos("`outcome'","any")>0 | strpos("`outcome'","tweet") {
		foreach var of varlist beta l95 u95 {
			replace `var'=`var'*100
		}
		g betashort = string(round(beta,.1))+"%"
	}
	else {
		g betashort = "$"+string(round(beta,.01))
	}

	g order = _n
	g ctrl = mod(order,3)==1
	
	forv i=1/14 {
		local mean`i' = betashort in `i'
	}
	
	// get p-value bracket positions
	forv i=1(3)13 {
		local j=`i'+1
		qui su u95 if order>=`i' & order<=`j'
		
		if strpos("`outcome'","Georgia")>0 {
			local pbrac`i'Y = r(max)+0.5
			local pbrac`i'Yl = r(max)+0.25
			local pbrac`i'Yu = r(max)+0.51
		}		
		else {
			local pbrac`i'Y = r(max)+1.4
			local pbrac`i'Yl = r(max)+0.6
			local pbrac`i'Yu = r(max)+1.45
		}
	}
	
	// for DiD p-value bracket
	qui su u95 if order>=3 & order<=8
	if strpos("`outcome'","Georgia")>0 {
		local pbracDiD = r(max)+1.2
		local pbracDiDl = `pbracDiD'-0.25
		local pbracDiDu = `pbracDiD'+0.01	
	}
	else if "`outcome'"=="totPartyCont" {
		local pbracDiD = r(max)+30
		local pbracDiDl = `pbracDiD'-10
		local pbracDiDu = `pbracDiD'+0.1
	}
	else {
		local pbracDiD = r(max)+3.5
		local pbracDiDl = `pbracDiD'-0.8
		local pbracDiDu = `pbracDiD'+0.05		
	}


	if "`outcome'"=="anyCont" {
		local rangemax = 35
		local rangemaxB = `rangemax'
		local ygap = 5
		local meanlevel = 1.5
		local outtitle = "Contributed at Least Once (%)"
	}
	else if "`outcome'"=="totCont" {
		local rangemax = 200
		local rangemaxB = `rangemax'		
		local ygap = 40
		local meanlevel = 6
		local outtitle = "Total Contributions ($)"
	}
	else if "`outcome'"=="anyPartyCont" {
		local rangemax = 50
		local rangemaxB = `rangemax'
		local ygap = 10
		local meanlevel = 3
		local outtitle = "Contributed at Least Once (%)"
	}
	else if "`outcome'"=="totPartyCont" {
		local rangemax = 600
		local rangemaxB = `rangemax'		
		local ygap = 100
		local meanlevel = 20
		local outtitle = "Total Contributions ($)"		
	}
	
	local colour1 "white"
	local colour2 "turquoise*0.3"
	
	if strpos("`outcome'","Party")>0 {
		local colour1 "red*0.8"
		local colour2 "blue*0.8"
		local didbrac ""
		local didp ""
	}
	else {
		local didbrac "(pci `pbracDiD' 1.5 `pbracDiD' 2.5 `pbracDiDl' 1.5 `pbracDiDu' 1.5 `pbracDiD' 4.5 `pbracDiD' 3.5 `pbracDiDl' 4.5 `pbracDiDu' 4.5, lc(black))"
		local didp "text(`pbracDiD' 3 "DiD" "`pDiD'", size(2.5))"
	}
	

	if strpos("`outcome'","tweet")==0 {
		// Figure: Silent vs. Vocal vs. Black vs. Non-Twitter
		preserve
			drop in 1/3
			replace order = order-3
			twoway (bar beta order if ctrl==1, barw(0.8) fc(`colour1') lc(black) lw(medium)) ///
				   (bar beta order if ctrl==0, barw(0.8) fc(`colour2') fi(inten50) lc(black) lw(medium)) ///
				   (pci `pbrac4Y' 1 `pbrac4Y' 1.1 `pbrac4Yl' 1 `pbrac4Yu' 1 `pbrac4Y' 2 `pbrac4Y' 1.9 `pbrac4Yl' 2 `pbrac4Yu' 2, lc(black)) ///
				   (pci `pbrac7Y' 4 `pbrac7Y' 4.1 `pbrac7Yl' 4 `pbrac7Yu' 4 `pbrac7Y' 5 `pbrac7Y' 4.9 `pbrac7Yl' 5 `pbrac7Yu' 5, lc(black)) ///
				   (pci `pbrac10Y' 7 `pbrac10Y' 7.1 `pbrac10Yl' 7 `pbrac10Yu' 7 `pbrac10Y' 8 `pbrac10Y' 7.9 `pbrac10Yl' 8 `pbrac10Yu' 8, lc(black)) ///	
				   (pci `pbrac13Y' 10 `pbrac13Y' 10.1 `pbrac13Yl' 10 `pbrac13Yu' 10 `pbrac13Y' 11 `pbrac13Y' 10.9 `pbrac13Yl' 11 `pbrac13Yu' 11, lc(black)) ///
				   `didbrac' ///
				   (rcap u95 l95 order, lc(black)), ///
				   ylabel(0(`ygap')`rangemaxB', labsize(4) nogrid) ///
				   ysc(r(0 `rangemax')) ///
				   legend(order(1 "`ctrlLab'" 2 "`treatLab'")) ///
				   `title' ///
				   ytitle("`outtitle'", size(4.5)) ///
				   xtitle("") ///
				   xlabel(1.5 "Silent" 4.5 "Vocal" 7.5 "Black" 10.5 "Off Twitter", noticks nogrid labsize(4)) ///
				   xtick(0(3)9) ///
				   xline(6 9, lpattern(dash) lcolor(black)) ///
				   xsize(8) ///
				   text(`pbrac4Y' 1.5 "`pNonSig'", size(2.5)) ///
				   text(`pbrac7Y' 4.5 "`pSig'", size(2.5)) ///
				   text(`pbrac10Y' 7.5 "`pBlack'", size(2.5)) ///
				   text(`pbrac13Y' 10.5 "`pNonTwit'", size(2.5)) ///
				   `didp' ///
				   text(`meanlevel' 1 "`mean4'", size(3)) ///
				   text(`meanlevel' 2 "`mean5'", size(3)) ///
				   text(`meanlevel' 4 "`mean7'", size(3)) ///
				   text(`meanlevel' 5 "`mean8'", size(3)) ///
				   text(`meanlevel' 7 "`mean10'", size(3)) ///
				   text(`meanlevel' 8 "`mean11'", size(3)) ///
				   text(`meanlevel' 10 "`mean13'", size(3)) ///
				   text(`meanlevel' 11 "`mean14'", size(3))
			graph export "$figpath/`outcome'_`treatment'_bysigBN.`img'", replace	
		restore	
	}
	
}
	  	
// Fig 2 and A8.b
if `other_predictors'==1 { // What else predicts signalling and discrimination?

	local treatment = "black" // black / female / firstgen
	// load data on all academics
	use Data/academics_anon, clear

	
	// What predicts signalling?
	reg s3 academic_female i.encoded_position i.encoded_ethnicity ///
		countTweetsAbovMed anyDEMCont ///
		deptbroad1 deptbroad2 deptbroad3 deptbroad4 deptbroad5 deptbroad6 ///
		rank1to50 rank51to100 undergradBlackFracAbovMed, vce(hc3)
	est sto predicts3
	
	coefplot predicts3, ///
		keep(academic_female countTweetsAbovMed *position* *ethnicity* ///
		anyDEMCont *dept* rank1to50 rank51to100 undergradBlackFracAbovMed) ///
		xline(0, lpattern(dash)) ///
		`title' ///
		coeflabels(academic_female = "Female" ///
			countTweetsAbovMed = "Number of Tweets Above-Median" ///
			anyDEMCont = "Any Democratic Contributions" ///
			rank1to50 = "Rank 1 to 50" ///
			rank51to100 = "Rank 51 to 100" ///
			undergradBlackFracAbovMed = "Undergrad Black % Above-Median" ///
			2.encoded_ethnicity = "East Asian" ///
			5.encoded_ethnicity = "South Asian") ///
		headings(rank1to50 = "{bf:University Measures} (Omitted: Rank 101 to 150)" ///
			2.encoded_ethnicity = "{bf:Race/Ethnicity} (Omitted: White)" ///
			2.encoded_position = "{bf:Position} (Omitted: Full Professor)" ///
			deptbroad3 = "{bf:Field} (Omitted: Social Sciences)" ///
			countTweetsAbovMed = "{bf:Twitter Measures}" ///
			anyDEMCont = "{bf:Political Preferences}") ///
		order(academic_female 2.encoded_position 3.encoded_position ///
			2.encoded_ethnicity 5.encoded_ethnicity 3.encoded_ethnicity 4.encoded_ethnicity ///
			countTweetsAbovMed anyDEMCont ///
			rank1to50 rank51to100 undergradBlackFracAbovMed ///
			deptbroad3 deptbroad5 deptbroad4 deptbroad6 deptbroad1 deptbroad2) ///
		xlabel(-0.2(0.1)0.5) xsize(7)
	graph export "$figpath/other_predictors_signalling.`img'", replace	
	
	
	
	// What predicts discrimination?
	reghdfe accepted100 `treatment' s3 `treatment'Xs3 `treatment'##academic_female ///
			`treatment'##encoded_position `treatment'##encoded_ethnicity ///
			`treatment'##(countTweetsAbovMed) ///
			`treatment'##(following_vocal_abovmed mi_following_network) ///			
			`treatment'##(anyDEMCont) ///
			`treatment'##(rank1to50 rank51to100) ///
			`treatment'##(undergradBlackFracAbovMed) ///
			`treatment'##(deptbroad1 deptbroad2 deptbroad3 deptbroad4 deptbroad5 deptbroad6), ///
			ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)	
	est sto predictdisc		
								
	coefplot predictdisc, ///
		keep(`treatment'Xs3 1.`treatment'#*) ///
		drop(1.`treatment'#4.encoded_ethnicity 1.`treatment'#1.mi_following_network) ///
		xline(0, lpattern(dash)) ///
		`title' ///
		coeflabels(`treatment'Xs3 = "Vocal" ///
			1.`treatment'#1.academic_female = "Female" ///
			1.`treatment'#1.countTweetsAbovMed = "Number of Tweets Above-Median" ///
			1.`treatment'#1.following_vocal_abovmed = "Following Vocal % Above-Median" ///			
			1.`treatment'#1.anyDEMCont = "Any Democratic Contributions" ///
			1.`treatment'#1.rank1to50 = "Rank 1 to 50" ///
			1.`treatment'#1.rank51to100 = "Rank 51 to 100" ///
			1.`treatment'#1.undergradBlackFracAbovMed = "Undergrad Black % Above-Median" ///
			1.`treatment'#2.encoded_ethnicity = "East Asian" ///
			1.`treatment'#3.encoded_ethnicity = "Hispanic" ///
			1.`treatment'#5.encoded_ethnicity = "South Asian" ///
			1.`treatment'#2.encoded_position = "Assistant Professor" ///
			1.`treatment'#3.encoded_position = "Associate Professor" ///
			1.`treatment'#1.deptbroad1 = "Business" ///
			1.`treatment'#1.deptbroad2 = "Engineering and Technology" ///
			1.`treatment'#1.deptbroad3 = "Humanities" ///
			1.`treatment'#1.deptbroad4 = "Life Sciences" ///
			1.`treatment'#1.deptbroad5 = "Physical Sciences" ///
			1.`treatment'#1.deptbroad6 = "Professional Schools") ///
		headings(1.`treatment'#1.rank1to50 = "{bf:University Measures}" ///
			1.`treatment'#2.encoded_ethnicity = "{bf:Race/Ethnicity} (Omitted: White)" ///
			1.`treatment'#2.encoded_position = "{bf:Position} (Omitted: Full Professor)" ///
			1.`treatment'#1.deptbroad1 = "{bf:Department} (Omitted: Social Sciences)" ///
			1.`treatment'#1.countTweetsAbovMed = "{bf:Twitter Measures}" ///
			1.`treatment'#1.anyDEMCont = "{bf:Political Preferences}") ///			
		xsize(7)
	graph export "$figpath/other_predictors_`treatment'_discrim.`img'", replace

	
	// What predicts discrimination?
	reghdfe accepted100 `treatment' s3 `treatment'Xs3, ///
			ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)	
	est sto eq0
	
	reghdfe accepted100 `treatment'##academic_female, ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	est sto eq1
	
	reghdfe accepted100 `treatment'##encoded_position, ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	est sto eq2
	
	reghdfe accepted100 `treatment'##encoded_ethnicity, ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	est sto eq3
	
	reghdfe accepted100 `treatment'##(countTweetsAbovMed), ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	est sto eq4
	
	reghdfe accepted100 `treatment'##(following_vocal_abovmed mi_following_network), ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	est sto eq5	
	
	reghdfe accepted100 `treatment'##(anyDEMCont), ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	est sto eq6
	
	reghdfe accepted100 `treatment'##(rank1to50 rank51to100), ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	est sto eq7
	
	reghdfe accepted100 `treatment'##(undergradBlackFracAbovMed), ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	est sto eq8
	
	reghdfe accepted100 `treatment'##(deptbroad1 deptbroad2 deptbroad3 deptbroad4 deptbroad5 deptbroad6), ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	est sto eq9
	
	coefplot (eq0, keep(`treatment'Xs3)) (eq1, keep(1.`treatment'#*)) (eq2, keep(1.`treatment'#*)) (eq3, keep(1.`treatment'#*) drop(1.`treatment'#4.encoded_ethnicity)) (eq4, keep(1.`treatment'#*)) (eq5, keep(1.`treatment'#*) drop(1.`treatment'#1.mi_following_network)) (eq6, keep(1.`treatment'#*)) (eq7, keep(1.`treatment'#*)) (eq8, keep(1.`treatment'#*)) (eq9, keep(1.`treatment'#*)), ///
	xline(0, lpattern(dash)) ///
	`title' ///
	ciopts(color(black)) msymbol(circle_hollow) mcolor(black) ///
	coeflabels(`treatment'Xs3 = "Vocal" ///
		1.`treatment'#1.academic_female = "Female" ///
		1.`treatment'#2.encoded_ethnicity = "East Asian" ///
		1.`treatment'#3.encoded_ethnicity = "Hispanic" ///
		1.`treatment'#5.encoded_ethnicity = "South Asian" ///
		1.`treatment'#2.encoded_position = "Assistant Professor" ///
		1.`treatment'#3.encoded_position = "Associate Professor" ///
		1.`treatment'#1.countTweetsAbovMed = "Number of Tweets Above-Median" ///
		1.`treatment'#1.following_vocal_abovmed = "Following Vocal % Above-Median" ///		
		1.`treatment'#1.anyDEMCont = "Any Democratic Contributions" ///
		1.`treatment'#1.rank1to50 = "Rank 1 to 50" ///
		1.`treatment'#1.rank51to100 = "Rank 51 to 100" ///
		1.`treatment'#1.undergradBlackFracAbovMed = "Undergrad Black % Above-Median" ///
		1.`treatment'#1.deptbroad1 = "Business" ///
		1.`treatment'#1.deptbroad2 = "Engineering and Technology" ///
		1.`treatment'#1.deptbroad3 = "Humanities" ///
		1.`treatment'#1.deptbroad4 = "Life Sciences" ///
		1.`treatment'#1.deptbroad5 = "Physical Sciences" ///
		1.`treatment'#1.deptbroad6 = "Professional Schools") ///
	headings(1.`treatment'#2.encoded_position = "{bf:Position} (Omitted: Full Professor)" ///
			1.`treatment'#2.encoded_ethnicity = "{bf:Race/Ethnicity} (Omitted: White)" /// 
			1.`treatment'#1.countTweetsAbovMed = "{bf:Twitter Measures}" ///
			1.`treatment'#1.anyDEMCont = "{bf:Political Preferences}" ///
			1.`treatment'#1.deptbroad1 = "{bf:Department} (Omitted: Social Sciences)" ///
			1.`treatment'#1.rank1to50 = "{bf:University Measures}") ///
	xsize(7) ///
	legend(off)
	
	graph export "$figpath/other_predictors_separately_`treatment'_discrim.`img'", replace
	
}

// Fig 3, 4, 5 ,A5
if `discrim'==1 { // Produce audit experiment figures

	foreach treatment in female black firstgen underrep{
		foreach outcome in accepted helpful replied{
						
			
			if "`treatment'"=="black" {
				local control = "white"
			}
			else if "`treatment'"=="female" {
				local control = "male"
			}
			else if "`treatment'"=="firstgen" {
				local control = "nextgen"
			}
			else if "`treatment'"=="underrep" {
				local control = "overrep"
			}
			
			local treatLab = proper("`treatment'")
			local ctrlLab = proper("`control'")
			
			if "`treatment'"=="firstgen" {
				local treatLab = "First-generation"
				local ctrlLab = "Regular"
			}
			if "`treatment'"=="underrep" {
				local treatLab = "Black or Female or First-generation"
				local ctrlLab = "White Male"
			}
			
			use Data/audit_anon, clear
			
			g beta = .
			g u95 = .
			g l95 = .
			
			** Overall Discrimination **
			// get control and treatment mean and 95% CI
			reg `outcome' `control' `treatment', cluster(deptschlXName) nocons 
			
			local i=1
			foreach x in control treatment {
				replace beta = _b[``x''] in `i'
				local `x'Mean = _b[``x'']
				replace u95 = _b[``x'']+_se[``x'']*invttail(e(df_r),0.025) in `i'
				replace l95 = _b[``x'']-_se[``x'']*invttail(e(df_r),0.025) in `i'
				local i=`i'+1
			}
			
			// get p-value
			reghdfe `outcome' `treatment', ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
				
			test `treatment'
			if r(p)<0.001 {
				local pAll = "{it:p}<0.001"
			}
			else if r(p)<0.01 {
				local pAll = "{it:p}<0.01"
			}
			else if r(p)>=0.995 {
				local pAll = "{it:p}=1"
			}
			else {
				local pAll = "{it:p}=0"+string(round(r(p),.01))
			}	
			test `treatment'==-0.021 // Kline comparison p.p.
			test `treatment'==-0.0268 // Kline comparison % (9% of Black mean)
			test `treatment'==-0.08 // Milkman comparison


			** Do Virtue Signals Signal Virtue?
			// get control and treatment mean and 95% CI
			reg `outcome' `control'Xs3 `control'XnonS3 `treatment'Xs3 `treatment'XnonS3, ///
				cluster(deptschlXName) nocons 
					
			local i=4
			foreach x in nonS3 s3 {
				foreach y in control treatment {
					replace beta = _b[``y''X`x'] in `i'
					local `y'X`x'Mean = _b[``y''X`x']
					replace u95 = _b[``y''X`x']+_se[``y''X`x']*invttail(e(df_r),0.025) in `i'
					replace l95 = _b[``y''X`x']-_se[``y''X`x']*invttail(e(df_r),0.025) in `i'
					local i=`i'+1
				}
				local i=`i'+1
			}	

			// get p-value
			reghdfe `outcome' `control'Xs3 `treatment'Xs3 `treatment'XnonS3, ///
				ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
										
			test `treatment'XnonS3
			if r(p)<0.001 {
				local pNonSig = "{it:p}<0.001"
			}
			else if r(p)<0.01 {
				local pNonSig = "{it:p}<0.01"
			}
			else if r(p)>=0.995 {
				local pNonSig = "{it:p}=1"
			}
			else {
				local pNonSig = "{it:p}=0"+string(round(r(p),.01))
			}	
				
			test `control'Xs3 = `treatment'Xs3
			if r(p)<0.001 {
				local pSig = "{it:p}<0.001"
			}
			else if r(p)<0.01 {
				local pSig = "{it:p}<0.01"
			}
			else if r(p)>0.995 {
				local pSig = "{it:p}=1"
			}
			else {
				local pSig = "{it:p}=0"+string(round(r(p),.01))
			}		
			
			// get DiD p-value
			reghdfe `outcome' `treatment' s3 `treatment'Xs3, ///
				ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
			test `treatment'Xs3
			if r(p)<0.001 {
				local pDiD = "{it:p}<0.001"
			}
			else if r(p)<0.01 {
				local pDiD = "{it:p}<0.01"
			}
			else if r(p)>0.995 {
				local pDiD = "{it:p}=1"
			}
			else {
				local pDiD = "{it:p}=0"+string(round(r(p),.01))
			}	

			// plot
			keep in 1/8
			keep beta u95 l95
			foreach var of varlist beta l95 u95 {
				replace `var'=`var'*100
			}
			g order = _n
			g ctrl = mod(order,3)==1
			
			g betashort = string(round(beta,.1))+"%"
			forv i=1/8 {
				local mean`i' = betashort in `i'
			}
			
			// get p-value bracket positions
			forv i=1(3)7 {
				local j=`i'+1
				qui su u95 if order>=`i' & order<=`j'
				local pbrac`i'Y = r(max)+1.4
				local pbrac`i'Yl = r(max)+0.6
				local pbrac`i'Yu = r(max)+1.45
			}
			
			// for DiD p-value bracket
			qui su u95 if order>=3 & order<=8
			local pbracDiD = r(max)+3.5
			local pbracDiDl = `pbracDiD'-0.8
			local pbracDiDu = `pbracDiD'+0.05
			
			if "`outcome'"=="accepted" {
				local rangemax = 40
				local outtitle = "Meeting Acceptance"
			}
			else if "`outcome'"=="helpful" {
				local rangemax = 55
				local outtitle = "Accepted Meeting or Helpful"
			}
			else if "`outcome'"=="replied" {
				local rangemax = 55
				local outtitle = "Replied"
			}	
			
			// full figure	
			twoway (bar beta order if ctrl==1, barw(0.8) fc(white) lc(black) lw(medium)) ///
				   (bar beta order if ctrl==0, barw(0.8) fc(sea*0.5) fi(inten50) lc(black) lw(medium)) ///
				   (pci `pbrac1Y' 1 `pbrac1Y' 1.2 `pbrac1Yl' 1 `pbrac1Yu' 1 `pbrac1Y' 2 `pbrac1Y' 1.8 `pbrac1Yl' 2 `pbrac1Yu' 2, lc(black)) ///
				   (pci `pbrac4Y' 4 `pbrac4Y' 4.2 `pbrac4Yl' 4 `pbrac4Yu' 4 `pbrac4Y' 5 `pbrac4Y' 4.8 `pbrac4Yl' 5 `pbrac4Yu' 5, lc(black)) ///
				   (pci `pbrac7Y' 7 `pbrac7Y' 7.2 `pbrac7Yl' 7 `pbrac7Yu' 7 `pbrac7Y' 8 `pbrac7Y' 7.8 `pbrac7Yl' 8 `pbrac7Yu' 8, lc(black)) ///	
				   (pci `pbracDiD' 4.5 `pbracDiD' 5.7 `pbracDiDl' 4.5 `pbracDiDu' 4.5 `pbracDiD' 7.5 `pbracDiD' 6.3 `pbracDiDl' 7.5 `pbracDiDu' 7.5, lc(black)) ///
				   (rcap u95 l95 order, lc(black)), ///
				   ylabel(0(5)`rangemax', labsize(4) nogrid) ///
				   ysc(r(0 `rangemax')) ///
				   legend(order(1 "`ctrlLab'" 2 "`treatLab'")) ///
				   `title' ///
				   ytitle("`outtitle' (%)", size(4.5)) ///
				   xtitle("") ///
				   xlabel(1.5 "Full Sample" 4.5 "Silent" 7.5 "Vocal", noticks nogrid labsize(4)) ///
				   xtick(0(3)9) ///
				   xline(3, lpattern(dash) lcolor(black)) ///
				   xsize(8) ///
				   text(`pbrac1Y' 1.5 "`pAll'", size(2.5)) ///
				   text(`pbrac4Y' 4.5 "`pNonSig'", size(2.5)) ///
				   text(`pbrac7Y' 7.5 "`pSig'", size(2.5)) ///
				   text(`pbracDiD' 6 "DiD" "`pDiD'", size(2.5)) ///
				   text(2 1 "`mean1'", size(3)) ///
				   text(2 2 "`mean2'", size(3)) ///
				   text(2 4 "`mean4'", size(3)) ///
				   text(2 5 "`mean5'", size(3)) ///
				   text(2 7 "`mean7'", size(3)) ///
				   text(2 8 "`mean8'", size(3))
			graph export "$figpath/`outcome'_`treatment'_bysig.`img'", replace
			
		}
	}

}

// fig 6
if `conditions'==1 { // Unconditional vs. conditional signalling: audit study
	
	local treatment = "black" // black / female / firstgen
	
	if "`treatment'"=="black" {
		local control = "white"
	}
	else if "`treatment'"=="female" {
		local control = "male"
	}
	else if "`treatment'"=="firstgen" {
		local control = "nextgen"
	}
	
	local treatLab = proper("`treatment'")
	local ctrlLab = proper("`control'")
	
	use Data/audit_anon, clear
		
	// (1) Unconditional (replicate earlier figure)
	reghdfe accepted100 `treatment' s3 `treatment'Xs3, ///
			ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	est sto uncond
	
	// (2) Number of tweets
	reghdfe accepted100 `treatment' s3 `treatment'Xs3 ///
			`treatment'##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply), ///
			ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	est sto cond1	
	
	// (3) Academic's gender, ethnicity, position
	reghdfe accepted100 `treatment' s3 `treatment'Xs3 `treatment'##academic_female ///
			`treatment'##encoded_position `treatment'##encoded_ethnicity, ///
			ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	est sto cond2	
	
	// (4) 2 + 3
	reghdfe accepted100 `treatment' s3 `treatment'Xs3 `treatment'##academic_female ///
			`treatment'##encoded_position `treatment'##encoded_ethnicity ///
			`treatment'##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply), ///
			ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	est sto cond3	
	
	// (5) + university
	reghdfe accepted100 `treatment' s3 `treatment'Xs3 `treatment'##academic_female ///
			`treatment'##encoded_position `treatment'##encoded_ethnicity ///
			`treatment'##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply) ///
			`treatment'##encoded_university, ///
			ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	est sto cond4	
	
	// (6) + broad department
	reghdfe accepted100 `treatment' s3 `treatment'Xs3 `treatment'##academic_female ///
			`treatment'##encoded_position `treatment'##encoded_ethnicity ///
			`treatment'##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply) ///
			`treatment'##encoded_university `treatment'##encoded_dept_broad, ///
			ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	est sto cond5		
	
	// (7) + fine department
	reghdfe accepted100 `treatment' s3 `treatment'Xs3 `treatment'##academic_female ///
			`treatment'##encoded_position `treatment'##encoded_ethnicity ///
			`treatment'##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply) ///
			`treatment'##encoded_university `treatment'##encoded_dept_fine, ///
			ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	est sto cond6	
	
	// (8) + Blindspotter
	reghdfe accepted100 `treatment' s3 `treatment'Xs3 `treatment'##academic_female ///
			`treatment'##encoded_position `treatment'##encoded_ethnicity ///
			`treatment'##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply) ///
			`treatment'##(c.BspotterPercentLeft c.BspotterPercentCenter BspotterPrivate BspotterInsufficient) ///
			`treatment'##encoded_university `treatment'##encoded_dept_fine, ///
			ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	est sto cond7	
	
	// (9) + Politicians Following
	reghdfe accepted100 `treatment' s3 `treatment'Xs3 `treatment'##academic_female ///
			`treatment'##encoded_position `treatment'##encoded_ethnicity ///
			`treatment'##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply) ///
			`treatment'##(c.BspotterPercentLeft c.BspotterPercentCenter BspotterPrivate BspotterInsufficient) ///
			`treatment'##encoded_university `treatment'##encoded_dept_fine ///
			`treatment'##(no_followed_data zero_followed pol_followed_oneplus c.pol_followed c.d_followed_perc), ///
			ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	est sto cond8	
	
	// (10) + FEC
	reghdfe accepted100 `treatment' s3 `treatment'Xs3 `treatment'##academic_female ///
			`treatment'##encoded_position `treatment'##encoded_ethnicity ///
			`treatment'##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply) ///
			`treatment'##(c.BspotterPercentLeft c.BspotterPercentCenter BspotterPrivate BspotterInsufficient) ///
			`treatment'##encoded_university `treatment'##encoded_dept_fine ///
			`treatment'##(no_followed_data zero_followed pol_followed_oneplus c.pol_followed c.d_followed_perc) ///
			`treatment'##(anyDEMCont anyREPCont c.totDEMCont c.totREPCont), ///
			ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	est sto cond9	
	
	// (11) + Fraction Vocal Following
	reghdfe accepted100 `treatment' s3 `treatment'Xs3 `treatment'##academic_female ///
			`treatment'##encoded_position `treatment'##encoded_ethnicity ///
			`treatment'##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply) ///
			`treatment'##(c.BspotterPercentLeft c.BspotterPercentCenter BspotterPrivate BspotterInsufficient) ///
			`treatment'##encoded_university `treatment'##encoded_dept_fine ///
			`treatment'##(no_followed_data zero_followed pol_followed_oneplus c.pol_followed c.d_followed_perc) ///
			`treatment'##(anyDEMCont anyREPCont c.totDEMCont c.totREPCont) ///
			`treatment'##(c.following_vocal_fr mi_following_network), ///
			ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	est sto cond10	
			
	local sampsize = string(e(N),"%9.0gc")	
	qui su accepted100 if e(sample)==1
	local depmean = string(r(mean),"%9.1f")
			
	coefplot uncond cond1 cond2 cond3 cond4 cond5 cond6 cond7 cond8 cond9 cond10, ///
		vert coeflabels(`treatment'Xs3 = "Coefficient on `treatLab'{c 215}Vocal") ///
		keep(`treatment'Xs3) ///
		`title' ///
		ylabel(0(2)12) ///
		legend(label(2 "Unconditional") label(4 "Number of Tweets") ///
				label(6 "Academic's Gender, Ethnicity, Position") ///
				label(8 "Number of Tweets, Gender, Ethnicity, Position") ///
				label(10 "Tweets, Gender, Ethnicity, Position, University") ///
				label(12 "Tweets, Gender, Ethnicity, Position, University, Broad Dept.") ///
				label(14 "Tweets, Gender, Ethnicity, Position, University, Fine Dept.") ///
				label(16 "+ Blindspotter-measured News Slant") ///
				label(18 "+ Political Accounts Following") ///	
				label(20 "+ FEC-reported Political Contributions") ///								
				label(22 "+ Fraction of Vocal Academics Followed")) ///								
		xsize(7) xline(0, lpattern(dash))
	graph export "$figpath/conditions_`treatment'.`img'", replace	
}

// fig 7 and A12
if `donations'==1 { // Do racial justice tweets predict donations?

use Data/donation_anon, clear

g beta=.
g l95=.
g u95=.
g order=_n

** Unconditional signalling first

* NAACP retweet
reg donation_amount no_retweet_check retweet_check, vce(hc3) nocons

replace beta=_b[no_retweet_check] in 1
replace u95=_b[no_retweet_check]+_se[no_retweet_check]*invttail(e(df_r),0.025) in 1
replace l95=_b[no_retweet_check]-_se[no_retweet_check]*invttail(e(df_r),0.025) in 1

replace beta=_b[retweet_check] in 2
replace u95=_b[retweet_check]+_se[retweet_check]*invttail(e(df_r),0.025) in 2
replace l95=_b[retweet_check]-_se[retweet_check]*invttail(e(df_r),0.025) in 2

test no_retweet_check=retweet_check
if r(p)<0.001 {
	local pRT = "{it:p}<0.001"
}
else if r(p)<0.01 {
	local pRT = "{it:p}<0.01"
}
else if r(p)>=0.995 {
	local pRT = "{it:p}=1"
}
else {
	local pRT = "{it:p}=0"+string(round(r(p),.01))
}	

count if e(sample)==1 & retweet_check==1
local NRT = string(r(N),"%9.0gc")
count if e(sample)==1 & retweet_check==0
local NnoRT = string(r(N),"%9.0gc")

* Vocal in 2020
reg donation_amount silent vocal, vce(hc3) nocons

replace beta=_b[silent] in 4
replace u95=_b[silent]+_se[silent]*invttail(e(df_r),0.025) in 4
replace l95=_b[silent]-_se[silent]*invttail(e(df_r),0.025) in 4

replace beta=_b[vocal] in 5
replace u95=_b[vocal]+_se[vocal]*invttail(e(df_r),0.025) in 5
replace l95=_b[vocal]-_se[vocal]*invttail(e(df_r),0.025) in 5

test silent=vocal
if r(p)<0.001 {
	local pVoc = "{it:p}<0.001"
}
else if r(p)<0.01 {
	local pVoc = "{it:p}<0.01"
}
else if r(p)>=0.995 {
	local pVoc = "{it:p}=1"
}
else {
	local pVoc = "{it:p}=0"+string(round(r(p),.01))
}	

count if e(sample)==1 & vocal==1
local NV = string(r(N),"%9.0gc")
count if e(sample)==1 & vocal==0
local NS = string(r(N),"%9.0gc")

// plot
keep in 1/5
keep beta u95 l95 order
g ctrl = mod(order,3)==1

g betashort = "$"+string(beta,"%9.2f")
forv i=1/5 {
	local mean`i' = betashort in `i'
}


// get p-value bracket positions
forv i=1(3)4 {
	local j=`i'+1
	qui su u95 if order>=`i' & order<=`j'
	local pbrac`i'Y = r(max)+0.2
	local pbrac`i'Yl = r(max)+0.175
	local pbrac`i'Yu = r(max)+0.205
}


// full figure
twoway (bar beta order if ctrl==1, barw(0.8) fc(white) lc(black) lw(medium)) ///
		(bar beta order if ctrl==0, barw(0.8) fc(vermillion*0.5) fi(inten50) lc(black) lw(medium)) ///
		(pci `pbrac1Y' 1 `pbrac1Y' 1.2 `pbrac1Yl' 1 `pbrac1Yu' 1 `pbrac1Y' 2 `pbrac1Y' 1.8 `pbrac1Yl' 2 `pbrac1Yu' 2, lc(black)) ///
		(pci `pbrac4Y' 4 `pbrac4Y' 4.2 `pbrac4Yl' 4 `pbrac4Yu' 4 `pbrac4Y' 5 `pbrac4Y' 4.8 `pbrac4Yl' 5 `pbrac4Yu' 5, lc(black)) ///		
		(rcap u95 l95 order, lc(black)), ///
		ylabel(0 "$0.00" 0.5 "$0.50" 1 "$1.00" 1.5 "$1.50" 2 "$2.00" 2.5 "$2.50" 3 "$3.00", labsize(4) nogrid) ///
		ysc(r(0 3)) ///
		legend(off) ///
		xlabel(1 `""Did Not" "Retweet" "NAACP" "(N=`NnoRT')""' 2 `""Retweeted" "NAACP" "(N=`NRT')""' 4 `""Silent" "In 2020" "(N=`NS')""' 5 `""Vocal" "In 2020" "(N=`NV')""', noticks nogrid labsize(4)) ///
		xtick(0(3)6) ///
	    ytitle("Donated to NAACP", size(4.5)) ///		
		xtitle("") ///
		text(`pbrac1Y' 1.5 "`pRT'", size(2.5)) ///
		text(`pbrac4Y' 4.5 "`pVoc'", size(2.5)) ///		
		xline(3, lpattern(dash) lcolor(black)) ///
		text(0.2 1 "`mean1'", size(3.5)) ///
		text(0.2 2 "`mean2'", size(3.5)) ///
		text(0.2 4 "`mean4'", size(3.5)) ///
		text(0.2 5 "`mean5'", size(3.5))
graph export "$figpath/donation_bysig.`img'", replace


** Conditional signalling
use Data/donation_anon, clear

g beta=.
g l95=.
g u95=.
g order=_n

* NAACP retweet
reg donation_amount retweet_check twitter_activity female i.income_enc i.politics_enc not_white hispanic_d age, vce(hc3)

qui su donation_amount if retweet_check==0 & e(sample)==1
replace beta = r(mean) in 1

replace beta=r(mean)+_b[retweet_check] in 2
replace u95=r(mean)+_b[retweet_check]+_se[retweet_check]*invttail(e(df_r),0.025) in 2
replace l95=r(mean)+_b[retweet_check]-_se[retweet_check]*invttail(e(df_r),0.025) in 2

test retweet_check
if r(p)<0.001 {
	local pRT = "{it:p}<0.001"
}
else if r(p)<0.01 {
	local pRT = "{it:p}<0.01"
}
else if r(p)>=0.995 {
	local pRT = "{it:p}=1"
}
else {
	local pRT = "{it:p}=0"+string(round(r(p),.01))
}	

count if e(sample)==1 & retweet_check==1
local NRT = string(r(N),"%9.0gc")
count if e(sample)==1 & retweet_check==0
local NnoRT = string(r(N),"%9.0gc")

* Vocal in 2020
reg donation_amount vocal NumberofpostsMayOctober2020 plusfifty female i.income_enc i.politics_enc not_white hispanic_d age, vce(hc3)

qui su donation_amount if vocal==0 & e(sample)==1
replace beta = r(mean) in 4

replace beta=r(mean)+_b[vocal] in 5
replace u95=r(mean)+_b[vocal]+_se[vocal]*invttail(e(df_r),0.025) in 5
replace l95=r(mean)+_b[vocal]-_se[vocal]*invttail(e(df_r),0.025) in 5

test vocal
if r(p)<0.001 {
	local pVoc = "{it:p}<0.001"
}
else if r(p)<0.01 {
	local pVoc = "{it:p}<0.01"
}
else if r(p)>=0.995 {
	local pVoc = "{it:p}=1"
}
else {
	local pVoc = "{it:p}=0"+string(round(r(p),.01))
}	

count if e(sample)==1 & vocal==1
local NV = string(r(N),"%9.0gc")
count if e(sample)==1 & vocal==0
local NS = string(r(N),"%9.0gc")

// plot
keep in 1/5
keep beta u95 l95 order
g ctrl = mod(order,3)==1

g betashort = "$"+string(beta,"%9.2f")
forv i=1/5 {
	local mean`i' = betashort in `i'
}

// get p-value bracket positions
forv i=1(3)4 {
	local j=`i'+1
	qui su u95 if order>=`i' & order<=`j'
	local pbrac`i'Y = r(max)+0.2
	local pbrac`i'Yl = r(max)+0.175
	local pbrac`i'Yu = r(max)+0.205
}

// full figure
twoway (bar beta order if ctrl==1, barw(0.8) fc(white) lc(black) lw(medium)) ///
		(bar beta order if ctrl==0, barw(0.8) fc(vermillion*0.5) fi(inten50) lc(black) lw(medium)) ///
		(pci `pbrac1Y' 1 `pbrac1Y' 1.2 `pbrac1Yl' 1 `pbrac1Yu' 1 `pbrac1Y' 2 `pbrac1Y' 1.8 `pbrac1Yl' 2 `pbrac1Yu' 2, lc(black)) ///
		(pci `pbrac4Y' 4 `pbrac4Y' 4.2 `pbrac4Yl' 4 `pbrac4Yu' 4 `pbrac4Y' 5 `pbrac4Y' 4.8 `pbrac4Yl' 5 `pbrac4Yu' 5, lc(black)) ///		
		(rcap u95 l95 order, lc(black)), ///
		ylabel(0 "$0.00" 0.5 "$0.50" 1 "$1.00" 1.5 "$1.50" 2 "$2.00" 2.5 "$2.50" 3 "$3.00", labsize(4) nogrid) ///
		ysc(r(0 3)) ///
		legend(off) ///
		xlabel(1 `""Did Not" "Retweet" "NAACP" "(N=`NnoRT')""' 2 `""Retweeted" "NAACP" "(N=`NRT')""' 4 `""Silent" "In 2020" "(N=`NS')""' 5 `""Vocal" "In 2020" "(N=`NV')""', noticks nogrid labsize(4)) ///
		xtick(0(3)6) ///
	    ytitle("Donated to NAACP", size(4.5)) ///		
		xtitle("") ///
		text(`pbrac1Y' 1.5 "`pRT'", size(2.5)) ///
		text(`pbrac4Y' 4.5 "`pVoc'", size(2.5)) ///		
		xline(3, lpattern(dash) lcolor(black)) ///
		text(0.2 1 "`mean1'", size(3.5)) ///
		text(0.2 2 "`mean2'", size(3.5)) ///
		text(0.2 4 "`mean4'", size(3.5)) ///
		text(0.2 5 "`mean5'", size(3.5))
graph export "$figpath/donation_bysig_ctrl.`img'", replace	

}

// table A2
if `donationsumstats'==1 { // Summary statistics for participants in donation study

use Data/donation_anon, clear

local vars age female male nonbinaryoth not_white hispanic_d politics_enc ///
	income_enc twitter_activity donation_amount vocal retweet_check support support_d

la var age "Age (Years)"
la var female "Female/Woman"
la var male "Male/Man"
la var nonbinary "Non-binary/Prefer Not to Answer"
la var not_white "Race not White or European"
la var hispanic_d "Hispanic, Latino, or Spanish Origin"
la var politics_enc "Political Views (1 = v. liberal, 5 = v. conservative)"
la var income_enc "Income Category (1 = less than 25k USD, 6 = 150k USD or more)"
la var twitter_activity "Number of Posts Per Month Since Joined Twitter"
la var donation_amount "Amount Donated to NAACP (0 to 5 USD)"
la var vocal "Tweeted in Support of Racial Justice May to October 2020"
la var retweet_check "Retweeted NAACP Tweet (Manually Checked)"
la var support "Raw Support for NAACP (0 to 100)"
la var support_d "Binary Support for NAACP"

tempname postStatsDonation
tempfile donationstats
postfile `postStatsDonation' ///
    str100 varname N Min Mean Max using "`donationstats'", replace

foreach v of local vars {
    local name: variable label `v'
	
	qui su `v'
	local N = r(N)
	local Min = r(min)
	local Mean = r(mean)
	local Max = r(max)
			
    post `postStatsDonation' ("`name'") (`N') (`Min') (`Mean') (`Max')
}
postclose `postStatsDonation'

use `donationstats', clear
format Min Mean Max %9.2fc
format N %9.0gc

/* make latex table */
listtab * using "$tabpath/donation_sumstats.tex", ///
    rstyle(tabular) replace ///
    head("\begin{tabular}{lcccc}" ///
    "\toprule" ///
	"\cmidrule(lr){2-5}" ///	
    "& N & Min & Mean & Max \\" ///
    "\midrule") ///
    foot("\bottomrule \end{tabular}")	
}

// fig 9
if `infoloss'==1 { // Are NAACP retweets less informative than private statements of support?

* Switches
local first		0 // set to 1 to keep only the question they answered first (retweet vs. support)
local left 		0 // set to 1 to keep only left-wing people (politics score <=2)

use Data/donation_anon, clear

local suf ""
if `first'==1 {
	keep if retweet_first==1
	local suf "_first"
}
if `left'==1 {
	keep if inlist(politics_enc,1,2)
	local suf "_left"
}

ren retweet_check retweet_checkORsupport_d 
g support_d_sample = 0 
tempfile retweet_check
save `retweet_check'

use Data/donation_anon, clear

if `first'==1 {
	keep if retweet_first==0
}
if `left'==1 {
	keep if inlist(politics_enc,1,2)
}

ren support_d retweet_checkORsupport_d
g support_d_sample = 1

append using `retweet_check'
g support_extra = retweet_checkORsupport_d*support_d_sample

*Unconditional
reg donation_amount retweet_checkORsupport_d support_extra support_d_sample, cluster(ResponseId)
test support_extra
if r(p)<0.001 {
	local pUnc = "{it:p}<0.001"
}
else if r(p)<0.01 {
	local pUnc = "{it:p}<0.01"
}
else if r(p)>=0.995 {
	local pUnc = "{it:p}=1"
}
else {
	local pUnc = "{it:p}=0"+string(round(r(p),.01))
}	

*Conditional
reg donation_amount retweet_checkORsupport_d support_extra support_d_sample ///
	twitter_activity female i.income_enc i.politics_enc not_white hispanic_d age, cluster(ResponseId)
test support_extra
if r(p)<0.001 {
	local pCon = "{it:p}<0.001"
}
else if r(p)<0.01 {
	local pCon = "{it:p}<0.01"
}
else if r(p)>=0.995 {
	local pCon = "{it:p}=1"
}
else {
	local pCon = "{it:p}=0"+string(round(r(p),.01))
}	

*Now for informativeness regressions separately
use Data/donation_anon, clear

if `left'==1 {
	keep if inlist(politics_enc,1,2)
}

g beta=.
g l95=.
g u95=.
g order=_n

local i=1
local cond ""
foreach var of varlist retweet_check support_d {
		
	if `first'==1 {
		if "`var'"=="retweet_check" {
			local cond "if retweet_first==1"
		}
		if "`var'"=="support_d" {
			local cond "if retweet_first==0"
		}
	}

	if `i'==4 {
		local i=2
	}
	
	reg donation_amount `var' `cond', vce(hc3)
	replace beta=_b[`var'] in `i'
	replace u95=_b[`var']+_se[`var']*invttail(e(df_r),0.025) in `i'
	replace l95=_b[`var']-_se[`var']*invttail(e(df_r),0.025) in `i'
	local i=`i'+3

	reg donation_amount `var' twitter_activity female i.income_enc i.politics_enc not_white hispanic_d age `cond', vce(hc3)
	replace beta=_b[`var'] in `i'
	replace u95=_b[`var']+_se[`var']*invttail(e(df_r),0.025) in `i'
	replace l95=_b[`var']-_se[`var']*invttail(e(df_r),0.025) in `i'
	local Nppl = string(e(N),"%9.0gc")
	
	if `first'==1 {
		qui su donation_amount if retweet_first==1
		local Nppl1 = string(r(N),"%9.0gc")
		local Nppl = "`Nppl1' / `Nppl'"
	}
}

// plot
keep in 1/5
keep beta u95 l95 order
g ctrl = mod(order,3)==1


// get p-value bracket positions
forv i=1(3)4 {
	local j=`i'+1
	qui su u95 if order>=`i' & order<=`j'
	local pbrac`i'Y = r(max)+0.1
	local pbrac`i'Yl = r(max)+0.075
	local pbrac`i'Yu = r(max)+0.102
}


// full figure
twoway (scatter beta order if ctrl==1, color(black)) ///
		(scatter beta order if ctrl==0, color(vermillion)) ///	
		(rcap u95 l95 order if ctrl==1, lc(black)) ///
		(rcap u95 l95 order if ctrl==0, lc(vermillion)) ///
		(pci `pbrac1Y' 1 `pbrac1Y' 1.2 `pbrac1Yl' 1 `pbrac1Yu' 1 `pbrac1Y' 2 `pbrac1Y' 1.8 `pbrac1Yl' 2 `pbrac1Yu' 2, lc(black)) ///
		(pci `pbrac4Y' 4 `pbrac4Y' 4.2 `pbrac4Yl' 4 `pbrac4Yu' 4 `pbrac4Y' 5 `pbrac4Y' 4.8 `pbrac4Yl' 5 `pbrac4Yu' 5, lc(black)), ///		
		ylabel(0 "$0.00" 0.5 "$0.50" 1 "$1.00" 1.5 "$1.50" 2 "$2.00", labsize(4) nogrid) ///
		ysc(r(0 1.5)) ///
		legend(off) ///
		xlabel(1 `" "NAACP" "Retweet""' 2 `" "NAACP" "Support" "' 4 `" "NAACP" "Retweet""' 5 `" "NAACP" "Support" "', noticks nogrid labsize(4)) ///
		xtick(0(3)6) ///
	    ytitle("Informativeness = Giving Gap", size(4.5)) ///		
		xtitle("") ///
		text(1.85 1.5 "Unconditional" "Informativeness", size(3.5) just(center)) ///
		text(1.85 4.5 "Conditional" "Informativeness", size(3.5) just(center)) ///	
		text(0.15 5 "N Participants = `Nppl'", size(3.5) just(right)) ///
		text(`pbrac1Y' 1.5 "`pUnc'", size(2.5)) ///	
		text(`pbrac4Y' 4.5 "`pCon'", size(2.5)) ///				
		xline(3, lpattern(dash) lcolor(black))
graph export "$figpath/donation_info_loss`suf'.`img'", replace


* Now see whether info loss is coming from high types or low types
use "Data/donation_anon", clear

g beta=.
g l95=.
g u95=.
g order=_n

* Low types
count if donation_amount==0
local Nlow = r(N)

local i=1
foreach var of varlist retweet_check support_d {
	reg `var' if donation_amount==0, robust
	replace beta=_b[_cons] in `i'
	replace u95=_b[_cons]+_se[_cons]*invttail(e(df_r),0.025) in `i'
	replace l95=_b[_cons]-_se[_cons]*invttail(e(df_r),0.025) in `i'
	local i=`i'+1
}

ttest retweet_check=support_d if donation_amount==0
if r(p)<0.001 {
	local pLow = "{it:p}<0.001"
}
else if r(p)<0.01 {
	local pLow = "{it:p}<0.01"
}
else if r(p)>=0.995 {
	local pLow = "{it:p}=1"
}
else {
	local pLow = "{it:p}=0"+string(round(r(p),.01))
}	


* High types
count if donation_amount==5
local Nhigh = r(N)

local i=4
foreach var of varlist retweet_check support_d {
	reg `var' if donation_amount==5, robust
	replace beta=_b[_cons] in `i'
	replace u95=_b[_cons]+_se[_cons]*invttail(e(df_r),0.025) in `i'
	replace l95=_b[_cons]-_se[_cons]*invttail(e(df_r),0.025) in `i'
	local i=`i'+1
}
ttest retweet_check=support_d if donation_amount==5
if r(p)<0.001 {
	local pHigh = "{it:p}<0.001"
}
else if r(p)<0.01 {
	local pHigh = "{it:p}<0.01"
}
else if r(p)>=0.995 {
	local pHigh = "{it:p}=1"
}
else {
	local pHigh = "{it:p}=0"+string(round(r(p),.01))
}


count if e(sample)==1 & vocal==1
local NV = string(r(N),"%9.0gc")
count if e(sample)==1 & vocal==0
local NS = string(r(N),"%9.0gc")

// plot
keep in 1/5
keep beta u95 l95 order
g ctrl = mod(order,3)==1
replace beta = beta*100
replace u95 = u95*100
replace l95 = l95*100

g betashort = string(beta,"%9.1f")+"%"
forv i=1/5 {
	local mean`i' = betashort in `i'
}

// get p-value bracket positions
forv i=1(3)4 {
	local j=`i'+1
	qui su u95 if order>=`i' & order<=`j'
	local pbrac`i'Y = r(max)+1
	local pbrac`i'Yl = r(max)+0.75
	local pbrac`i'Yu = r(max)+1.05
}

// full figure
twoway (bar beta order if ctrl==1, barw(0.8) fc(white) lc(black) lw(medium)) ///
		(bar beta order if ctrl==0, barw(0.8) fc(vermillion*0.5) fi(inten50) lc(black) lw(medium)) ///
		(pci `pbrac1Y' 1 `pbrac1Y' 1.2 `pbrac1Yl' 1 `pbrac1Yu' 1 `pbrac1Y' 2 `pbrac1Y' 1.8 `pbrac1Yl' 2 `pbrac1Yu' 2, lc(black)) ///
		(pci `pbrac4Y' 4 `pbrac4Y' 4.2 `pbrac4Yl' 4 `pbrac4Yu' 4 `pbrac4Y' 5 `pbrac4Y' 4.8 `pbrac4Yl' 5 `pbrac4Yu' 5, lc(black)) ///		
		(rcap u95 l95 order, lc(black)), ///
		ylabel(0(5)35, labsize(4) nogrid) ///
		ysc(r(0 3)) ///
		legend(off) ///
		xlabel(1 "Retweeted" 2 "Supported" 4 "Retweeted" 5 "Supported", noticks nogrid labsize(4)) ///
		xtick(0(3)6) ///
	    ytitle("% That Retweeted or Supported", size(4.5)) ///		
		xtitle("") ///
		text(`pbrac1Y' 1.5 "`pLow'", size(2.5)) ///
		text(`pbrac4Y' 4.5 "`pHigh'", size(2.5)) ///		
		xline(3, lpattern(dash) lcolor(black)) ///
		text(1.5 1 "`mean1'", size(3.5)) ///
		text(1.5 2 "`mean2'", size(3.5)) ///
		text(1.5 4 "`mean4'", size(3.5)) ///
		text(1.5 5 "`mean5'", size(3.5)) ///
		text(32 1.5 "Donated Nothing" "(N = `Nlow')", size(3.5)) ///
		text(32 4.5 "Donated Everything" "(N = `Nhigh')", size(3.5))		
graph export "$figpath/donation_info_loss_where.`img'", replace
}

// fig 10 
if `predict_fig'==1 { // Plot student predictions against experimental results

// choose what percentiles to use for lower and upper range
local lowPerc = 25
local highPerc = 75

use Data/audit_anon, clear

g beta = .
g u95 = .
g l95 = .
	
** Racial Discrimination: Full Sample **
* all *
reghdfe accepted black, ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
local u95 = 100*(_b[black]+_se[black]*invttail(e(df_r),0.025))
local l95 = 100*(_b[black]-_se[black]*invttail(e(df_r),0.025))

* among Vocal *
// get point estimate and 95% CI
reghdfe accepted blackXnonS3 s3 blackXs3, ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)

local i=7
replace beta = _b[blackXs3] in `i'
replace u95 = _b[blackXs3]+_se[blackXs3]*invttail(e(df_r),0.025) in `i'
replace l95 = _b[blackXs3]-_se[blackXs3]*invttail(e(df_r),0.025) in `i'
local u95Vocal = 100*(_b[blackXs3]+_se[blackXs3]*invttail(e(df_r),0.025))
local l95Vocal = 100*(_b[blackXs3]-_se[blackXs3]*invttail(e(df_r),0.025))

* among Silent and Unconditional Difference *
// get point estimate and 95% CI	
reghdfe accepted black s3 blackXs3, ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)

local i=1
foreach x in black blackXs3 {
	replace beta = _b[`x'] in `i'
	replace u95 = _b[`x']+_se[`x']*invttail(e(df_r),0.025) in `i'
	replace l95 = _b[`x']-_se[`x']*invttail(e(df_r),0.025) in `i'
	
	if `i'==1 {
		local u95Silent = 100*(_b[`x']+_se[`x']*invttail(e(df_r),0.025))
		local l95Silent = 100*(_b[`x']-_se[`x']*invttail(e(df_r),0.025))
	}
	else {
		local u95Info = 100*(_b[`x']+_se[`x']*invttail(e(df_r),0.025))
		local l95Info = 100*(_b[`x']-_se[`x']*invttail(e(df_r),0.025))
	}	
	local i=`i'+12
}



keep beta l95 u95
g order = _n
drop if mi(beta)
foreach var of varlist beta l95 u95 {
	replace `var'=`var'*100
}
tempfile betas
save `betas'


* Now get predictions
use Data\grad_survey_answers_anon, clear

g beta=.
g l95=.
g u95=.

// Basic stats
* What % overestimate/underestimate anti-Black discrimination of all/Vocal/Silent?
foreach x in "" "Vocal" "Silent" {
	di "Discrimination CI: `l95`x'', `u95`x''"
	count if !mi(predict`x'Disc)
	local Nall = r(N)
	count if predict`x'Disc<`l95`x''
	local Nbelow = r(N)
	count if predict`x'Disc>=`l95`x'' & predict`x'Disc<=`u95`x''
	local Nwithin = r(N)
	count if predict`x'Disc>`u95`x''
	local Nabove = r(N)

	local PercBelow = round(100*(`Nbelow'/`Nall'),1)
	local PercWithin = round(100*(`Nwithin'/`Nall'),1)
	local PercAbove = round(100*(`Nabove'/`Nall'),1)
	di "Predictions for `x' Academics"
	di "Below lower CI: `PercBelow'%"
	di "Within CI: `PercWithin'%"
	di "Above CI: `PercAbove'%"
}


* What % overestimate/underestimate informativeness?
foreach x in ""  {
	di "`x' Informativeness CI: `l95Info`x'', `u95Info`x''"
	count if !mi(predict`x'DiscDiff)
	local Nall = r(N)
	count if predict`x'DiscDiff<`l95Info`x''
	local Nbelow = r(N)
	count if predict`x'DiscDiff>=`l95Info`x'' & predict`x'DiscDiff<=`u95Info`x''
	local Nwithin = r(N)
	count if predict`x'DiscDiff>`u95Info`x''
	local Nabove = r(N)
	
	local PercBelow = round(100*(`Nbelow'/`Nall'),1)
	local PercWithin = round(100*(`Nwithin'/`Nall'),1)
	local PercAbove = round(100*(`Nabove'/`Nall'),1)
	di "Predictions for `x' Informativeness"
	di "Below lower CI: `PercBelow'%"
	di "Within CI: `PercWithin'%"
	di "Above CI: `PercAbove'%"	
}

// Silent discrimination
local i=2
foreach x in SilentDisc VocalDisc DiscDiff  {
	foreach y in black nonblack liberal nonliberal {
		su predict`x' if `y'==1, det
		replace beta = r(p50) in `i'
		replace l95 = r(p`lowPerc') in `i'
		replace u95 = r(p`highPerc') in `i'
		
		local N`y'=string(r(N),"%9.0gc")
		
		local i=`i'+1
	}
	local i=`i'+2
}

keep beta l95 u95
g order = _n
keep if order<=19
drop if inlist(order,1,7,13,19)
append using `betas'
sort order

local ylow = l95 in 2
local ymid = beta in 2
local yhigh = u95 in 2


// plot
	g colour = mod(order,6)==1 // to enable bars to be coloured based on the different specifications
	replace colour = colour[_n-1]+1 if colour[_n]==0 
	replace colour=. if inlist(colour,6,12,18)



// full figure	
twoway (scatter beta order if colour==1 & order<=17, msymbol(circle) mcolor(purple)) ///
	   (rcap l95 u95 order if colour==1 & order<=17, lcolor(purple)) ///
	   (scatter beta order if colour==2 & order<=17, msymbol(diamond) mcolor(erose)) ///
	   (scatter l95 order if colour==2 & order<=17, msymbol(diamond) mcolor(erose%50)) ///
	   (scatter u95 order if colour==2 & order<=17, msymbol(diamond) mcolor(erose%50)) ///
	   (rcap l95 u95 order if colour==2 & order<=17, lcolor(erose) lpattern(dot)) ///
	   (scatter beta order if colour==3 & order<=17, msymbol(diamond) mcolor(dkorange)) ///
	   (scatter l95 order if colour==3 & order<=17, msymbol(diamond) mcolor(dkorange%50)) ///
	   (scatter u95 order if colour==3 & order<=17, msymbol(diamond) mcolor(dkorange%50)) ///
	   (rcap l95 u95 order if colour==3 & order<=17, lcolor(dkorange) lpattern(dot)) ///	   
	   (scatter beta order if colour==4 & order<=17, msymbol(diamond) mcolor(emerald%70)) ///
	   (scatter l95 order if colour==4 & order<=17, msymbol(diamond) mcolor(emerald%35)) ///
	   (scatter u95 order if colour==4 & order<=17, msymbol(diamond) mcolor(emerald%35)) ///
	   (rcap l95 u95 order if colour==4 & order<=17, lcolor(emerald%70) lpattern(dot)) ///	   	   
	   (scatter beta order if colour==5 & order<=17, msymbol(diamond) mcolor(cranberry)) ///
	   (scatter l95 order if colour==5 & order<=17, msymbol(diamond) mcolor(cranberry%50)) ///
	   (scatter u95 order if colour==5 & order<=17, msymbol(diamond) mcolor(cranberry%50)) ///
	   (rcap l95 u95 order if colour==5 & order<=17, lcolor(cranberry) lpattern(dot)), ///	   	   
	   legend(order(1 "Audit Estimates" - "" - "Student Predictions:" ///
		3 "Black (N = `Nblack')" 7 "Non-Black (N = `Nnonblack')" ///
		11 "Liberal (N = `Nliberal')" 15 "Moderate or Conservative (N = `Nnonliberal')") ring(0) pos(4)) ///
	   `title' ///
	   ytitle("Black Minus White Gap in Meeting Acceptance (p.p.)", size(4)) ///
	   xtitle("") ///
	   xlabel(3 "Silent" 9 "Vocal" 15 `" "Difference" "', noticks nogrid labsize(4)) ///
	   xtick(0(6)18) ///
	   xline(6, lpattern(dash) lcolor(gs13)) ///
	   xline(12, lpattern(dash) lcolor(gs13)) ///
	   yline(0, lpattern(dash) lcolor(black)) ///
	   text(`ylow' 1.7 "p`lowPerc'", size(2) place(w) just(right)) ///
	   text(`ymid' 1.7 "Median", size(2) place(w) just(right)) ///
	   text(`yhigh' 1.7 "p`highPerc'", size(2) place(w) just(right)) ///	   
	   xsize(8)	   
graph export "$figpath/student_predictions_unc.`img'", replace
}

// table 1 and A4
if `high_stakes'==1 { // Informativeness of racial justice tweets for high stakes outcomes

use Data/table1_anon, clear

foreach var of varlist academic_female encoded_position encoded_ethnicity countOriginal ///
countQuoted countQuotedReplied countRT countReply encoded_university encoded_dept_fine ///
BspotterPercentLeft BspotterPercentCenter BspotterPrivate BspotterInsufficient ///
no_followed_data zero_followed pol_followed_oneplus pol_followed d_followed_perc ///
mi_Bspotter_data anyDEMCont anyREPCont totDEMCont totREPCont  {
	assert !mi(`var')
}

** Regress each outcome on S3 (Vocal); with and without controls

local i = 1
foreach var of varlist rmp_rating racial_topic black_share left_twitter ///
		pct_would_take_again racial_topic4o black_coauthor { // last 3 for robustness
	
	*Additional controls for each outcome
	if "`var'" == "rmp_rating" | "`var'"=="pct_would_take_again" local additional_controls "i.nratings10"
	if "`var'" == "racial_topic" | "`var'"=="racial_topic4o" local additional_controls ///
		"i.works_count10 i.namecommoness10 i.unique_topics_count10 i.first_work_year"
	if "`var'" == "black_share" | "`var'" == "black_coauthor" local additional_controls "i.names_coded10 zero_coauthors" 
	if "`var'" == "left_twitter" local additional_controls "" 
	
	* Unconditional
	reg `var' s3, vce(robust)
	est sto I`i'
	local obs`i' = string(e(N),"%9.2gc")
	
	sum `var' if s3 == 0 & e(sample) == 1
	local mean`i' = string(r(mean),"%9.2fc")
	local sd`i' = string(r(sd),"%9.2fc")

	local i = `i' + 1
	
		
	* Conditional (preferred controls)
	reg `var' s3 `additional_controls' academic_female i.encoded_position /// 
	i.encoded_ethnicity c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply ///
	i.encoded_university i.encoded_dept_fine, vce(robust)
	est sto I`i'
	local obs`i' = string(e(N),"%9.2gc")	
	
	local i = `i' + 1
}
	
** Output panel without controls
estout I1 I3 I5 I7 using "$tabpath/high_stakes_tableA.tex", style(tex) replace ///
keep(s3) cells(b(star fmt(%9.2f)) se(par)) ///
nolabel collabels(none) mlabels(none) starlevels(* 0.10 ** 0.05 *** 0.01) ///
varlabels(s3 "Vocal")

** Output panel with preferred controls
estout I2 I4 I6 I8 using "$tabpath/high_stakes_tableB.tex", style(tex) replace ///
keep(s3) cells(b(star fmt(%9.2f)) se(par)) ///
nolabel collabels(none) mlabels(none) starlevels(* 0.10 ** 0.05 *** 0.01) ///
varlabels(s3 "Vocal")

local tex "& \multicolumn{4}{c}{(a) Without Controls} \\ \input{Tables/high_stakes_tableA} \\ \hline \vspace{-8pt} & & & & \\"
local tex "`tex' & \multicolumn{4}{c}{(b) With Preferred Controls} \\"
local tex "`tex' \input{Tables/high_stakes_tableB} \\ \hline"
local tex "`tex' Observations & `obs2' & `obs4' & `obs6' & `obs8' \\"
local tex "`tex' Silent Mean & `mean1' & `mean3' & `mean5' & `mean7' \\"
local tex "`tex' Silent SD & `sd1' & `sd3' & `sd5' & `sd7' \\ \toprule"
local tex "`tex' \end{tabular} }"

esttab I1 I3 I5 I7 using "$tabpath/high_stakes_table.tex", style(tex) booktabs replace ///
	d(*) nolabel collabels(none) noobs nonum postfoot("`tex'") ///
	mtitles("\specialcell{Teacher\\Rating\\(1)}" ///
	"\specialcell{Work on\\Racial Topic\\(2)}" ///
	"\specialcell{Black\\Coauthor (\%)\\(3)}" ///
	"\specialcell{Left\\Twitter\\(4)}")	
	
** Robustness Table
** Output panel without controls
estout I9 I11 I13 using "$tabpath/high_stakes_table_rA.tex", style(tex) replace ///
keep(s3) cells(b(star fmt(%9.2f)) se(par)) ///
nolabel collabels(none) mlabels(none) starlevels(* 0.10 ** 0.05 *** 0.01) ///
varlabels(s3 "Vocal")

** Output panel with preferred controls
estout I10 I12 I14 using "$tabpath/high_stakes_table_rB.tex", style(tex) replace ///
keep(s3) cells(b(star fmt(%9.2f)) se(par)) ///
nolabel collabels(none) mlabels(none) starlevels(* 0.10 ** 0.05 *** 0.01) ///
varlabels(s3 "Vocal")

local tex "& \multicolumn{3}{c}{(a) Without Controls} \\ \input{Tables/high_stakes_table_rA} \\ \hline \vspace{-8pt} & & & \\"
local tex "`tex' & \multicolumn{3}{c}{(b) With Preferred Controls} \\"
local tex "`tex' \input{Tables/high_stakes_table_rB} \\ \hline"
local tex "`tex' Observations & `obs10' & `obs12' & `obs14' \\"
local tex "`tex' Silent Mean & `mean9' & `mean11' & `mean13' \\"
local tex "`tex' Silent SD & `sd9' & `sd11' & `sd13' \\ \toprule"
local tex "`tex' \end{tabular} }"

esttab I9 I11 I13 using "$tabpath/high_stakes_table_r.tex", style(tex) booktabs replace ///
	d(*) nolabel collabels(none) noobs nonum postfoot("`tex'") ///
	mtitles("\specialcell{Would Take\\Prof's Class\\Again (\%)\\(1)}" ///
	"\specialcell{Work on\\Racial Topic\\(AI-coded)\\(2)}" ///
	"\specialcell{Black\\Coauthor\\(0-1)\\(3)}")	
}

// table A1
if `sumstats'==1 { // Audit sample vs. non-audited

use Data/academics_sumstat_anon, clear

replace BspotterPercentLeft=. if BspotterInsufficientcontent==1 | BspotterPrivate==1
replace r_followed=. if r_followed==-99
replace d_followed=. if d_followed==-99
replace following_vocal_fr=. if mi_following_network==1

local vars academic_female rank fullprof assistprof assocprof ///
	deptbroad1 deptbroad2 deptbroad3 deptbroad4 deptbroad5 ///
	deptbroad6 deptbroad7 ethnicity1 ethnicity2 ethnicity3 ethnicity4 ethnicity6 ethnicity5 ///
	years_on_twitter count_all followers_count friends_count ///
	s3 pctS3Tweets following_vocal_fr ///
	BspotterPercentLeft r_followed d_followed ///
	anyREPCont anyDEMCont totREPCont totDEMCont 

	
la var s3 "Vocal"
la var pctS3Tweets "Percentage of Racial Justice Tweets"
la var following_vocal_fr "Fraction of Vocal Academics Following"
la var followers_count "Number of Twitter Followers"
la var friends_count "Number of Accounts Following"
la var count_all "Number of Tweets"
la var r_followed "Republican Accounts Following"
la var d_followed "Democratic Accounts Following"
la var anyREPCont "Any Republican Contributions"
la var anyDEMCont "Any Democratic Contributions"
la var totREPCont "Total Republican Contributions (USD)"
la var totDEMCont "Total Democratic Contributions (USD)"
	
tempname postStats
tempfile stats
postfile `postStats' ///
    str100 varname Mean1 Mean2 using "`stats'", replace

foreach v of local vars {
    local name: variable label `v'
	
	forv i=1/2 {
		qui su `v' if sample==`i'
		local Mean`i' = r(mean)
	}
			
    post `postStats' ("`name'") (`Mean1') (`Mean2')
}
postclose `postStats'

replace BspotterPercentLeft=0 if BspotterInsufficientcontent==1 | BspotterPrivate==1
replace r_followed=-99 if r_followed==.
replace d_followed=-99 if d_followed==.

use `stats', clear
format Mean* %9.2fc

/* make latex table */
listtab * using "$tabpath/summarystats.tex", ///
    rstyle(tabular) replace ///
    head("\begin{tabular}{lcccc}" ///
    "\toprule" ///
	"& \multicolumn{1}{c}{Audited} & \multicolumn{1}{c}{Not Audited} \\" ///
	"& \multicolumn{1}{c}{\textit{N = 11,450}} & \multicolumn{1}{c}{\textit{N = 7,064}} \\" ///
	"\cmidrule(lr){2-2} \cmidrule(lr){3-3}" ///	
    "& Mean & Mean \\" ///
    "\midrule") ///
    foot("\bottomrule \end{tabular}")
}

// table A3
if `balance'==1 { // Audit study balance checks

use Data/audit_anon, clear

local treatment = "black" // black / female / firstgen
local treatLab = proper("`treatment'")

g assistprof = encoded_position==2
g assocprof = encoded_position==3
g whiteprof = encoded_ethnicity==1

local i=1
foreach var of varlist academic_female assistprof assocprof whiteprof count_all s3 followers_count anyDEMCont {
	
	// Full sample
	reghdfe `var' `treatment', ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	est sto F`i'
	
	qui su `var' if e(sample)==1
	local Mean`i' = string(r(mean),"%9.2gc")
	local obs`i' = string(e(N),"%9.2gc")
	
	if "`var'"!="s3" {
		// Balance by Vocal vs. Silent
		reghdfe `var' `treatment'Xs3 `treatment'XnonS3 s3, ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
		est sto V`i'
		
		test `treatment'Xs3 = `treatment'XnonS3
		local pval`i' = string(r(p),"%9.2g")
	}
	
	local i=`i'+1
}


** Output balance table
estout F1 F2 F3 F4 F5 F6 F7 F8 using "$tabpath/balance`treatment'full.tex", style(tex) replace ///
	keep(`treatment') cells(b(star fmt(%9.2f)) se(par)) ///
	nolabel collabels(none) mlabels(none) starlevels(* 0.10 ** 0.05 *** 0.01) ///
	varlabels(`treatment' "`treatLab'")

estout V1 V2 V3 V4 V5 V7 V8 using "$tabpath/balance`treatment'het.tex", style(tex) replace ///
	keep(`treatment'Xs3 `treatment'XnonS3) cells(b(star fmt(%9.2f)) se(par)) ///
	nolabel collabels(none) mlabels(none) starlevels(* 0.10 ** 0.05 *** 0.01) ///
	varlabels(`treatment'Xs3 "`treatLab' $\times$ Vocal" ///
				`treatment'XnonS3 "`treatLab' $\times$ Silent") ///
	extracols(6)

local tex "& \multicolumn{8}{c}{(a) Balance for Full Sample} \\ \input{Tables/balance`treatment'full} \\ \hline"
local tex "`tex' \vspace{-8pt} & & & & & & & & \\"
local tex "`tex' & \multicolumn{8}{c}{(b) Balance by Vocality} \\"
local tex "`tex' \input{Tables/balance`treatment'het} \\ \hline"
local tex "`tex' Observations & `obs1' & `obs2' & `obs3' & `obs4' & `obs5' & `obs6' & `obs7' & `obs8' \\"
local tex "`tex' Full Sample Outcome Mean & `Mean1' & `Mean2' & `Mean3' & `Mean4' & `Mean5' & `Mean6' & `Mean7' & `Mean8' \\" 
local tex "`tex' p-value (Black $\times$ Vocal = Black $\times$ Silent) & `pval1' & `pval2' & `pval3' & `pval4' & `pval5' & `pval6' & `pval7' & `pval8' \\" 
local tex "`tex' Vocal Dummy (Panel (b) only) & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes \\" 
local tex "`tex' Strata Fixed Effects & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes \\" 
local tex "`tex' Email Type Fixed Effects & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes \\ \toprule" 
local tex "`tex' \multicolumn{9}{p{23cm}}{\footnotesize \textit{Notes:} Standard errors are clustered at university-department-sender name-level."
local tex "`tex' Black is a dummy variable equal to one for receiving an email from a distinctively Black name. Outcome variables are:" 
local tex "`tex' (1) dummy variable for female academic," 
local tex "`tex' (2) dummy variable for Assistant Professor,"
local tex "`tex' (3) dummy variable for Associate Professor,"
local tex "`tex' (4) dummy variable for White academic," 
local tex "`tex' (5) number of tweets (Jan 1, 2020 to Mar 27, 2022)," 
local tex "`tex' (6) dummy variable for whether tweeted about racial justice (Jan 1, 2020 to Mar 27, 2022)," 
local tex "`tex' (7) number of Twitter followers as of May 10, 2022,"
local tex "`tex' and (8) dummy variable for any FEC-linked contributions to Democrat-related committees (Jan 1, 2020 to Mar 27, 2022)."
local tex "`tex' *** p<0.01, ** p<0.05, * p<0.1.} \\"
local tex "`tex' \end{tabular} }"

esttab F1 F2 F3 F4 F5 F6 F7 F8 using "$tabpath/balance`treatment'.tex", style(tex) booktabs replace ///
	d(*) nolabel collabels(none) noobs nonum postfoot("`tex'") ///
	mtitles("\specialcell{\\Female\\(1)}" ///
	"\specialcell{Assistant\\Professor\\(2)}" ///
	"\specialcell{Associate\\Professor\\(3)}" ///
	"\specialcell{\\White\\(4)}" ///
	"\specialcell{Number Of\\Tweets\\(5)}" ///
	"\specialcell{\\Vocal\\(6)}" ///	
	"\specialcell{Number Of\\Followers\\(7)}" ///
	"\specialcell{Any Democrat\\Contributions\\(8)}")
clear	
}

// fig A6 and A7
if `intensive'==1 { // Figure for extensive vs. intensive margin

	local outcome = "accepted" // accepted / helpful / replied	
	foreach treatment in female black firstgen{
	
	if "`treatment'"=="black" {
		local control = "white"
	}
	else if "`treatment'"=="female" {
		local control = "male"
	}
	else if "`treatment'"=="firstgen" {
		local control = "nextgen"
	}

	if "`treatment'"!="firstgen" {
		local treatLab = proper("`treatment'")
		local ctrlLab = proper("`control'")		
	}
	else if "`treatment'"=="firstgen" {
		local treatLab = "First-generation"
		local ctrlLab = "Regular"
	}

 	use Data/audit_anon, clear
		
	g beta = .
	g u95 = .
	g l95 = .
	
	** Do Virtue Signals Signal Virtue?
	foreach x in `control' `treatment' {
		ren `x'XpctS3GT0ToMed `x'Xmid
		ren `x'XpctS3GTMed `x'Xhigh
	}
	
	// get control and treatment mean and 95% CI
	reg `outcome' `control'XnonS3 `treatment'XnonS3 ///
				`control'Xmid `treatment'Xmid ///
				`control'Xhigh `treatment'Xhigh, ///
		cluster(deptschlXName) nocons 

	local i=1
	foreach x in nonS3 mid high {
		foreach y in control treatment {
			replace beta = _b[``y''X`x'] in `i'
			local `y'X`x' = _b[``y''X`x']
			replace u95 = _b[``y''X`x']+_se[``y''X`x']*invttail(e(df_r),0.025) in `i'
			replace l95 = _b[``y''X`x']-_se[``y''X`x']*invttail(e(df_r),0.025) in `i'
			local i=`i'+1
		}
		local i=`i'+1
	}	
	
	// get p-values
	reghdfe `outcome' `control'Xmid `control'Xhigh ///
			`treatment'XnonS3 `treatment'Xmid `treatment'Xhigh, ///
		ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)	
			
	foreach x in nonS3 mid high {
		test `treatment'X`x'
		if r(p)<0.001 {
			local p`x' = "{it:p}<0.001"
		}
		else if r(p)<0.01 {
			local p`x' = "{it:p}<0.01"
		}
		else if r(p)>=0.995 {
			local p`x' = "{it:p}=1"
		}
		else {
			local p`x' = "{it:p}=0"+string(round(r(p),.01))
		}		
	}
		
		
	// plot
	keep in 1/8
	keep beta u95 l95
	foreach var of varlist beta l95 u95 {
		replace `var'=`var'*100
	}
	g order = _n
	g ctrl = mod(order,3)==1
	
	g betashort = string(round(beta,.1))+"%"
	forv i=1/8 {
		local mean`i' = betashort in `i'
	}
	
	// get p-value bracket positions
	forv i=1(3)7 {
		local j=`i'+1
		qui su u95 if order>=`i' & order<=`j'
		local pbrac`i'Y = r(max)+1.4
		local pbrac`i'Yl = r(max)+0.6
		local pbrac`i'Yu = r(max)+1.45
	}	
		
	if "`outcome'"=="accepted" {
		local rangemax = 40
		local outtitle = "Meeting Acceptance"
	}
	else if "`outcome'"=="helpful" {
		local rangemax = 55
		local outtitle = "Accepted Meeting or Helpful"
	}
	else if "`outcome'"=="replied" {
		local rangemax = 55
		local outtitle = "Replied"
	}	
	
	// full figure	
	twoway (bar beta order if ctrl==1, barw(0.8) fc(white) lc(black) lw(medium)) ///
		   (bar beta order if ctrl==0, barw(0.8) fc(sea*0.5) fi(inten50) lc(black) lw(medium)) ///
		   (pci `pbrac1Y' 1 `pbrac1Y' 1.2 `pbrac1Yl' 1 `pbrac1Yu' 1 `pbrac1Y' 2 `pbrac1Y' 1.8 `pbrac1Yl' 2 `pbrac1Yu' 2, lc(black)) ///
		   (pci `pbrac4Y' 4 `pbrac4Y' 4.2 `pbrac4Yl' 4 `pbrac4Yu' 4 `pbrac4Y' 5 `pbrac4Y' 4.8 `pbrac4Yl' 5 `pbrac4Yu' 5, lc(black)) ///
		   (pci `pbrac7Y' 7 `pbrac7Y' 7.2 `pbrac7Yl' 7 `pbrac7Yu' 7 `pbrac7Y' 8 `pbrac7Y' 7.8 `pbrac7Yl' 8 `pbrac7Yu' 8, lc(black)) ///			   
		   (rcap u95 l95 order, lc(black)), ///
		   ylabel(0(5)`rangemax', labsize(4) nogrid) ///
		   ysc(r(0 `rangemax')) ///
		   legend(order(1 "`ctrlLab'" 2 "`treatLab'")) ///
		   `title' ///
		   ytitle("`outtitle' (%)", size(4.5)) ///
		   xtitle("") ///
		   xlabel(1.5 "Silent" 4.5 "Rarely Vocal" 7.5 "Regularly Vocal", noticks nogrid labsize(4)) ///
		   xtick(0(3)9) ///
		   xsize(8) ///
		   text(`pbrac1Y' 1.5 "`pnonS3'", size(2.5)) ///
		   text(`pbrac4Y' 4.5 "`pmid'", size(2.5)) ///
		   text(`pbrac7Y' 7.5 "`phigh'", size(2.5)) ///		   
		   text(2 1 "`mean1'", size(3)) ///
		   text(2 2 "`mean2'", size(3)) ///
		   text(2 4 "`mean4'", size(3)) ///
		   text(2 5 "`mean5'", size(3)) ///
		   text(2 7 "`mean7'", size(3)) ///
		   text(2 8 "`mean8'", size(3))
	graph export "$figpath/`outcome'_`treatment'_extint.`img'", replace	
	}
}

// table A5
if `studentsumstats'==1 { // Summary statistics for graduate student sample

use Data\grad_survey_answers_anon, clear

forv i=0/1 {
	count if nonblack==`i'
	local Nsamp`i' = string(r(N),"%9.0gc")	
}

local vars ageMid female male nonbinary ///
	black white asian indigenous pacificIslander ///
	hispanic american ///
	doingPhD rank program_yearEnc ///
	politics_scaleEnc ///
	hasTwitter tweets_about_raceEnc 
	
la var ageMid "Age (Years)"
la var female "Female/Woman"
la var male "Male/Man"
la var nonbinary "Non-binary/Genderfluid/Genderqueer"
la var black "Black or African American"
la var white "White or European"
la var asian "Asian"
la var indigenous "First Nations or Indigenous"
la var pacificIslander "Native Hawaiian or Other Pacific Islander"
la var hispanic "Hispanic, Latino, or Spanish origin"
la var american "Born in the USA"
la var doingPhD "Studying for a PhD"
la var rank "Rank of University (1 to 80)"
la var program_yearEnc "Year in Graduate Program"
la var politics_scaleEnc "Political Views (1 = v. liberal, 5 = v. conservative)"
la var hasTwitter "Has Twitter"
la var tweets_about_raceEnc "How Often Tweets About Racial Justice (1 to 4)"

tempname postStatsStudent
tempfile studentstats
postfile `postStatsStudent' ///
    str100 varname N1 Min1 Mean1 Max1 N2 Min2 Mean2 Max2 using "`studentstats'", replace

foreach v of local vars {
    local name: variable label `v'
	
	forv i=0/1 {
		local j=`i'+1
		qui su `v' if nonblack==`i'
		local N`j' = r(N)
		local Min`j' = r(min)
		local Mean`j' = r(mean)
		local Max`j' = r(max)
	}
			
    post `postStatsStudent' ("`name'") (`N1') (`Min1') (`Mean1') (`Max1') (`N2') (`Min2') (`Mean2') (`Max2')
}
postclose `postStatsStudent'

use `studentstats', clear
format Min? Mean? Max? %9.2fc
format N1 N2 %9.0gc

/* make latex table */
listtab * using "$tabpath/student_sumstats.tex", ///
    rstyle(tabular) replace ///
    head("\begin{tabular}{lcccccccc}" ///
    "\toprule" ///
	"& \multicolumn{4}{c}{Black Students} & \multicolumn{4}{c}{Non-Black Students} \\" ///
	"\cmidrule(lr){2-5} \cmidrule(lr){6-9}" ///	
    "& N & Min & Mean & Max & N & Min & Mean & Max \\" ///
    "\midrule") ///
    foot("\bottomrule \end{tabular}")
}

// fig A10 and A11
if `left_twitter'==1 { // Left Twitter analysis
	

use Data/activity_panel, clear

local initialSilent = statuses_count[1]
local initialVocal = statuses_count[2] 

egen order = group(date_collected)

format date_collected %tdMon_DD
generate date_text2 = string(date_collected, "%tdMon_DD")

labmask order, values(date_text2)

gen pctOnTwitter =. 
replace pctOnTwitter = (statuses_count/`initialVocal')*100 if s3==1
replace pctOnTwitter = (statuses_count/`initialSilent')*100 if s3==0


** All
drop pctOnTwitter
collapse (sum) statuses_count, by(date_collected order)
gen pctOnTwitter =. 
replace pctOnTwitter = (statuses_count/18514)*100 

twoway (scatter pctOnTwitter order, msymbol(circle) mcolor(ebblue)) ///
	   (line pctOnTwitter order, lcolor(ebblue) lstyle(solid)), ///	
	   title("Percentage of initial sample of academics on Twitter (using 2022-2023 plus March 2025 profile panels data)") ///
	   ytitle("% of initial sample of academics", size(4)) ///
	   xlabel(1(1)78, angle(50) valuelabel noticks nogrid labsize(3)) ///
	   xline(19.5, lpattern(dash) lcolor(black)) ///
	   xline(23, lpattern(dash) lcolor(black)) ///
	   xline(28, lpattern(dash) lcolor(black)) ///
	   xline(37.3, lpattern(dash) lcolor(black)) ///
	   xline(77.5, lpattern(dash) lcolor(black)) ///
	   xsize(12) ///
	   xtitle("2022-23") ///
	   text(97 19.3 "Musk takeover" "(Oct 27)", place(w) size(3)) ///
	   text(93 23.2 "Massive" "layoffs" "(Nov 4)", place(e) size(3)) ///
	   text(93 28.2 "Massive resignations" "post-ultimatum" "(Nov 16)", place(e) size(3)) ///
	   text(93 37.5 "Suspended ElonJet +" "banned journalists" "(Dec 14)", place(e) size(3)) ///
	   text(82 75.5 "2025", place(e) size(3)) ///
	   legend(off)
graph export "$figpath/count_by_week_all.pdf", replace	


use Data/table1_anon, clear

bysort encoded_dept_fine: gen acad_count = _N

** Scatters % Left Twitter and % Donate to Democrats/Republicans
bysort encoded_dept_fine: egen total_left_twitter_f = total(left_twitter == 1)
bysort encoded_dept_fine: gen pct_left_twitter_f = (total_left_twitter_f/_N)*100
bysort encoded_dept_fine: egen total_donated_dem_f = total(anyDEMCont == 1)
bysort encoded_dept_fine: gen pct_donated_dem_f = (total_donated_dem_f/_N)*100
bysort encoded_dept_fine: egen total_donated_rep_f = total(anyREPCont == 1)
bysort encoded_dept_fine: gen pct_donated_rep_f = (total_donated_rep_f/_N)*100

drop if acad_count < 100
twoway (scatter pct_left_twitter_f pct_donated_dem_f, mcolor(sea) msymbol(circle)) ///
	   (lfit pct_left_twitter_f pct_donated_dem_f, lpattern(solid) lcolor(black)), ///
	   xtitle("Percentage Donated to Democrats") ytitle("Percentage Left Twitter") ///
	   ylabel(5(5)40) legend(off) title("")
	   
   	   graph export "$figpath/donated_dem_left_twitter.pdf", replace	

}

// fig 8 and A14 (change settings inside the following curly brackets to produce A14 instead of 8)
if `pre_postGF'==1 { // Produce informativeness figure for pre-, during- and post-murder of George Floyd

// Choose binary/continuous measure of tweets
local measure = "continuous" // binary / continuous
// local measure = "binary" // binary / continuous

// Choose whether to include everyone and set var=0 if var==. or exclude those that didn't tweet during a given period 
local sample = "everyone" // everyone / non-missing

// Choose winsorised / non-winsorised continuous measure
local winsorised = "yes" // yes / no


local treatment = "black" // black / female / firstgen
local outcome = "accepted" // accepted / helpful / replied	
	
if "`treatment'"=="black" {
		local control = "white"
}
else if "`treatment'"=="female" {
		local control = "male"
}
else if "`treatment'"=="firstgen" {
		local control = "nextgen"
}


local treatLab = proper("`treatment'")
local ctrlLab = proper("`control'")
	
use Data/audit_anon, clear

destring s3_*, replace
	
foreach x in unc cond {	
	g beta_`x' = .
	g u95_`x' = .
	g l95_`x' = .
}


// Create pooled binary signal measures
	*Pre-murder: 1 Jan 2020 - 24 May 2020
	egen s3_preGF = rowmax(s3_012020 s3_022020 s3_032020 s3_042020 s3_may_pt1_2020)
	*During murder: 25 May 2020 - 30 June 2020
	egen s3_duringGF = rowmax(s3_may_pt2_2020 s3_062020)
	*Post-murder: 1 July 2020 - 30 November 2020
	egen s3_postGF = rowmax(s3_072020 s3_082020 s3_092020 s3_102020 s3_112020)

	
// Binary vs continuous measure of tweets
if "`measure'"=="binary" {
	local list s3_preGF s3_duringGF s3_postGF    
	local measLab "Vocal"
}
else if "`measure'"=="continuous" {
	local list pct_s3_preGF pct_s3_duringGF pct_s3_postGF
	
	foreach x in pre during post { // Fixing continuous variables
		replace pct_s3_`x'GF = 0 if mi(pct_s3_`x'GF) & !mi(tweets_count_`x'GF) 	
	}
	
	local measLab "Vocal (%)"
}	


** Store % vocal in each period
if "`sample'"=="everyone"  { // irrespective of binary or continuous
	foreach x in pre during post {
		assert s3_`x'GF==1 if s3count_`x'GF !=.
		qui su s3_`x'GF
		local vocalPerc`x' = round(r(mean)*100,1)
	}
}
else if "`sample'"=="non-missing" {
	foreach x in pre during post {
		count if s3count_`x'GF != .
		local num = r(N)
		count if tweets_count_`x'GF !=.
		local denom = r(N)
		local vocalPerc`x' = round((`num'/`denom')*100,1)
	}
}

	
// Getting point estimates and 95% CI	
local i=1 
foreach x in `list' {
    
	if "`x'"=="s3_preGF" | "`x'"=="pct_s3_preGF" {
	    local y = "tweets_count_preGF"
	}
	else if "`x'"=="s3_duringGF" | "`x'"=="pct_s3_duringGF" {
	    local y = "tweets_count_duringGF"
	}
	else if "`x'"=="s3_postGF" | "`x'"=="pct_s3_postGF" {
	    local y = "tweets_count_postGF"
	}
	
	if "`sample'"=="everyone" {
		replace `x'=0 if mi(`x')
		replace `y'=0 if mi(`y')		
	}
	
	if "`measure'"=="continuous" & "`winsorised'"=="yes" {
		sum `x', detail
		replace `x'=r(p99) if `x'> r(p99) & !mi(`x')
	}
		

	// Unconditional point estimate and 95% CI
		gen double `treatment'X`x' = `treatment'*`x'
	
		reghdfe accepted100 `treatment' `x' `treatment'X`x' if `y'!=., ///
		ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
		
			if "`x'"=="s3_preGF" | "`x'"=="pct_s3_preGF" {
				local Npre = e(N)
			}	
			else if "`x'"=="s3_duringGF" | "`x'"== "pct_s3_duringGF" {
				local Nduring = e(N)
			}
			else if "`x'"=="s3_postGF" | "`x'"== "pct_s3_postGF" {
				local Npost = e(N)
			}	
		
		replace beta_unc = _b[`treatment'X`x'] in `i'
		replace u95_unc = _b[`treatment'X`x']+_se[`treatment'X`x']*invttail(e(df_r),0.025) in `i'
		replace l95_unc = _b[`treatment'X`x']-_se[`treatment'X`x']*invttail(e(df_r),0.025) in `i'


	// Conditional point estimate and 95% CI (all controls)
		reghdfe accepted100 `treatment' `x' `treatment'X`x' ///
		`treatment'##academic_female `treatment'##encoded_position `treatment'##encoded_ethnicity ///
		`treatment'##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply) ///
		`treatment'##encoded_university `treatment'##encoded_dept_fine if `y'!=., ///
		ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)			
		
		replace beta_cond = _b[`treatment'X`x'] in `i'
		replace u95_cond = _b[`treatment'X`x']+_se[`treatment'X`x']*invttail(e(df_r),0.025) in `i'
		replace l95_cond = _b[`treatment'X`x']-_se[`treatment'X`x']*invttail(e(df_r),0.025) in `i'
		
		local i=`i'+1
}


// Stack and Cluster to get p-values

// Binary vs continuous measure of tweets
if "`measure'"=="binary" {
	local list1 s3_duringGF s3_postGF  
	local varlist1 black s3_duringGF blackXs3_duringGF
	local varlist2 black s3_postGF blackXs3_postGF
}
else if "`measure'"=="continuous" {
	local list1 pct_s3_duringGF pct_s3_postGF
	local varlist1 black pct_s3_duringGF blackXpct_s3_duringGF
	local varlist2 black pct_s3_postGF blackXpct_s3_postGF
}

preserve	
	// 1. Unconditional regressions
		local i = 1
		foreach x in `list1' {
		    if "`x'"=="s3_preGF" | "`x'"=="pct_s3_preGF" {
				local y = "tweets_count_preGF"
			}
			else if "`x'"=="s3_duringGF" | "`x'"=="pct_s3_duringGF" {
				local y = "tweets_count_duringGF"
			}
			else if "`x'"=="s3_postGF" | "`x'"=="pct_s3_postGF" {
				local y = "tweets_count_postGF"
			}
	
		// reg 1 and reg 2
			reghdfe accepted100 `treatment' `x' `treatment'X`x' if `y'!=., ///
			ab(deptStrata EmailTypeFull) vce(cluster deptschlXName) 

			gen reg`i'=e(sample)
			
			local i = `i'+1
		}

		// stack the samples
		gen stack_id=_n
		expand 2, gen(samp2)
		drop if (reg1==0 & samp2==0) | (reg2==0 & samp2==1)

		// transform the variables
		foreach var in `varlist1' {
			gen double `var'_1 = `var'*(1-samp2)
			assert `var'_1 == `var' if samp2==0 
			if "`sample'"=="non-missing" {
				replace `var'_1=0 if `var'_1==. 
			}	
			
		}		
		
		foreach var in `varlist2' {
			gen double `var'_2 = `var'*samp2
			assert `var'_2 == `var' if samp2==1
			if "`sample'"=="non-missing" {
				replace `var'_2=0 if `var'_2==.
			}
		} 
		
		// stacked regressions  
		if "`measure'"=="binary" {
			reghdfe accepted100 black_1##samp2 s3_duringGF_1##samp2 blackXs3_duringGF_1##samp2 ///
				black_2##samp2 s3_postGF_2##samp2 blackXs3_postGF_2##samp2, ///
				ab(deptStrata##samp2 EmailTypeFull##samp2) ///
				vce(cluster stack_id deptschlXName)
		
			// test for whether the post-period coefficient is different to the during-period coefficient
			test 1.blackXs3_postGF_2 = 1.blackXs3_duringGF_1 
		}
		else if "`measure'"=="continuous" {
			reghdfe accepted100 black_1##samp2 c.pct_s3_duringGF_1##samp2 c.blackXpct_s3_duringGF_1##samp2 ///
				black_2##samp2 c.pct_s3_postGF_2##samp2 c.blackXpct_s3_postGF_2##samp2, ///
				ab(deptStrata##samp2 EmailTypeFull##samp2) ///
				vce(cluster stack_id deptschlXName)
		
			// test for whether the post-period coefficient is different to the during-period coefficient
			test blackXpct_s3_postGF_2 = blackXpct_s3_duringGF_1
		}
		
		// store p-value
		if r(p)<0.001 {
			local p`measure'Unc = "{it:p}<0.001"
		}
		else if r(p)<0.01 {
			local p`measure'Unc = "{it:p}<0.01"
		}
		else if r(p)>=0.995 {
			local p`measure'Unc = "{it:p}=1"
		}
		else {
			local p`measure'Unc = "{it:p}=0"+string(round(r(p),.01))
		}			
restore	

preserve
	// 2. Conditional regressions	
		local i = 1
		foreach x in `list1' {
		    if "`x'"=="s3_preGF" | "`x'"=="pct_s3_preGF" {
				local y = "tweets_count_preGF"
			}
			else if "`x'"=="s3_duringGF" | "`x'"=="pct_s3_duringGF" {
				local y = "tweets_count_duringGF"
			}
			else if "`x'"=="s3_postGF" | "`x'"=="pct_s3_postGF" {
				local y = "tweets_count_postGF"
			}
	
		// reg 1 and reg 2
			reghdfe accepted100 `treatment' `x' `treatment'X`x' ///
			`treatment'##academic_female `treatment'##encoded_position `treatment'##encoded_ethnicity ///
			`treatment'##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply) ///
			`treatment'##encoded_university `treatment'##encoded_dept_fine if `y'!=., ///
			ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)

			gen reg`i'=e(sample)
			
			local i = `i'+1
		}
		
		// stack the samples
		gen stack_id=_n
		expand 2, gen(samp2)
		drop if (reg1==0 & samp2==0) | (reg2==0 & samp2==1)

		// transform the variables	
		foreach var in `varlist1' {
			gen double `var'_1 = `var'*(1-samp2)
			assert `var'_1 == `var' if samp2==0 
			if "`sample'"=="non-missing" {
				replace `var'_1=0 if `var'_1==. 
			}	
		}
		
		foreach var in `varlist2' {
			gen double `var'_2 = `var'*samp2
			assert `var'_2 == `var' if samp2==1
			if "`sample'"=="non-missing" {
				replace `var'_2=0 if `var'_2==.
			}
		} 
		
		// stacked regressions  
		if "`measure'"=="binary" {
			reghdfe accepted100 black_1##samp2 s3_duringGF_1##samp2 blackXs3_duringGF_1##samp2 black_2##samp2 ///
				s3_postGF_2##samp2 blackXs3_postGF_2##samp2 ///
				black##academic_female##samp2 black##encoded_position##samp2 black##encoded_ethnicity##samp2 ///
				black##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply)##samp2 ///
				black##encoded_university##samp2 black##encoded_dept_fine##samp2, ///
				ab(deptStrata##samp2 EmailTypeFull##samp2) ///
				vce(cluster stack_id deptschlXName)
			
			// test for whether the post-period coefficient is different to the during-period coefficient
			test 1.blackXs3_postGF_2 = 1.blackXs3_duringGF_1 
		}
		else if "`measure'"=="continuous" {
			reghdfe accepted100 black_1##samp2 c.pct_s3_duringGF_1##samp2 c.blackXpct_s3_duringGF_1##samp2 ///
				black_2##samp2 c.pct_s3_postGF_2##samp2 c.blackXpct_s3_postGF_2##samp2 ///
				black##academic_female##samp2 black##encoded_position##samp2 black##encoded_ethnicity##samp2 ///
				black##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply)##samp2 ///
				black##encoded_university##samp2 black##encoded_dept_fine##samp2, ///
				ab(deptStrata##samp2 EmailTypeFull##samp2) ///
				vce(cluster stack_id deptschlXName)
		
			// test for whether the post-period coefficient is different to the during-period coefficient
			test  blackXpct_s3_postGF_2 = blackXpct_s3_duringGF_1  
		}	
		
		// store p-value
		if r(p)<0.001 {
			local p`measure'Cond = "{it:p}<0.001"
		}
		else if r(p)<0.01 {
			local p`measure'Cond = "{it:p}<0.01"
		}
		else if r(p)>=0.995 {
			local p`measure'Cond = "{it:p}=1"
		}
		else {
			local p`measure'Cond = "{it:p}=0"+string(round(r(p),.01))
		}			
restore
	
	
// plot
preserve
	keep in 1/3
	keep beta* u95* l95* 
	
	g order_unc = _n
	g order_cond = order_unc + 0.1
	
	if "`measure'"=="continuous" {
		qui su u95_unc if order_unc==3
		local pbrac1Y = r(max)+0.3
		local pbrac1Yl = r(max)+0.2
		local pbrac1Yu = r(max)+0.31
		local pbrac2Y = r(max)+0.5
		local pbrac2Yl = r(max)+0.4
		local pbrac2Yu = r(max)+0.51		
	}
	else if "`measure'"=="binary" {
		qui su u95_unc if order_unc==3
		local pbrac1Y = r(max)+1.2
		local pbrac1Yl = r(max)+1
		local pbrac1Yu = r(max)+1.25
		local pbrac2Y = r(max)+2.2
		local pbrac2Yl = r(max)+2
		local pbrac2Yu = r(max)+2.25		
	}

	if "`sample'"=="everyone" {
		twoway (scatter beta_unc order_unc, msymbol(circle) mcolor(ebblue)) ///
			(rcap l95_unc u95_unc order_unc, lcolor(ebblue)) ///
			(scatter beta_cond order_cond, msymbol(square) mcolor(green)) ///
			(rcap l95_cond u95_cond order_cond, lcolor(green)) ///
			(pci `pbrac1Y' 2 `pbrac1Y' 2.4 `pbrac1Yl' 2 `pbrac1Yu' 2 `pbrac1Y' 3 `pbrac1Y' 2.6 `pbrac1Yl' 3 `pbrac1Yu' 3, lc(black*0.5)) ///
			(pci `pbrac2Y' 2.1 `pbrac2Y' 2.5 `pbrac2Yl' 2.1 `pbrac2Yu' 2.1 `pbrac2Y' 3.1 `pbrac2Y' ///
				2.7 `pbrac2Yl' 3.1 `pbrac2Yu' 3.1, lc(black*0.5)), ///			
			legend(order(1 "Unconditional" 3 "Conditional") rows(1) position(6)) ///		   
			`title' ///
			ytitle("Coefficient on `treatLab'{c 215}`measLab'", size(4)) ///
			xtitle("") ///
			text(`pbrac1Y' 2.5 "`p`measure'Unc'", size(2.5)) ///
			text(`pbrac2Y' 2.6 "`p`measure'Cond'", size(2.5)) ///			
			xlabel(1 `" "Pre-Period" "Jan 1 - May 24" "(`vocalPercpre'% Vocal)" "' ///
				2 `" "During-Period" "May 25 - June 30" "(`vocalPercduring'% Vocal)" "' ///
				3 `" "Post-Period" "July 1 - Nov 30" 	"(`vocalPercpost'% Vocal)" "', ///
				noticks nogrid labsize(3)) yline(0, lpattern(dash) lcolor(gs13)) 			
	}
	else if "`sample'"=="non-missing" {
		twoway (scatter beta_unc order_unc, msymbol(circle) mcolor(ebblue)) ///
			(rcap l95_unc u95_unc order_unc, lcolor(ebblue)) ///
			(scatter beta_cond order_cond, msymbol(square) mcolor(green)) ///
			(rcap l95_cond u95_cond order_cond, lcolor(green)) ///
			(pci `pbrac1Y' 2 `pbrac1Y' 2.4 `pbrac1Yl' 2 `pbrac1Yu' 2 `pbrac1Y' 3 `pbrac1Y' 2.6 `pbrac1Yl' 3 `pbrac1Yu' 3, lc(black*0.5)) ///
			(pci `pbrac2Y' 2.1 `pbrac2Y' 2.5 `pbrac2Yl' 2.1 `pbrac2Yu' 2.1 `pbrac2Y' 3.1 `pbrac2Y' ///
				2.7 `pbrac2Yl' 3.1 `pbrac2Yu' 3.1, lc(black*0.5)), ///				
			legend(order(1 "Unconditional" 3 "Conditional") rows(1) position(6)) ///		   
			`title' ///
			ytitle("Coefficient on `treatLab'{c 215}`measLab'", size(4)) ///
			xtitle("") ///
			text(`pbrac1Y' 2.5 "`p`measure'Unc'", size(2.5)) ///
			text(`pbrac2Y' 2.6 "`p`measure'Cond'", size(2.5)) ///						
			xlabel(1 `" "Pre-Period" "Jan 1 - May 24" "(`vocalPercpre'% Vocal)" "{it:N} =`:display %9.0fc `Npre''" "' ///
					2 `" "During-Period" "May 25 - June 30" "(`vocalPercduring'% Vocal)" "{it:N} =`:display %9.0fc `Nduring''" "' ///
					3 `" "Post-Period" "July 1 - Nov 30" 	"(`vocalPercpost'% Vocal)" "{it:N} =`:display %9.0fc `Npost''" "', ///
			noticks nogrid labsize(3)) yline(0, lpattern(dash) lcolor(gs13)) 
	}

	graph export "$figpath/pre_postGF_`measure'_`sample'_w-`winsorised'.`img'", replace
restore
}

// table 2
if `tabGF'==1 { // Table of informativeness of during vs. post-GF tweets

// Choose whether to include everyone and set var=0 if var==. or exclude those that didn't tweet during a given period 
local sample = "everyone" // everyone / non-missing

// Choose winsorised / non-winsorised continuous measure
local winsorised = "yes" // yes / no

local treatment = "black" // black / female / firstgen
local outcome = "accepted" // accepted / helpful / replied	
	
if "`treatment'"=="black" {
		local control = "white"
}
else if "`treatment'"=="female" {
		local control = "male"
}
else if "`treatment'"=="firstgen" {
		local control = "nextgen"
}

local treatLab = proper("`treatment'")
local ctrlLab = proper("`control'")
	

use Data/audit_anon, clear

destring s3_*, replace
	

// Create pooled binary signal measures
	*Pre-murder: 1 Jan 2020 - 24 May 2020
	egen s3_preGF = rowmax(s3_012020 s3_022020 s3_032020 s3_042020 s3_may_pt1_2020)
	*During murder: 25 May 2020 - 30 June 2020
	egen s3_duringGF = rowmax(s3_may_pt2_2020 s3_062020)
	*Post-murder: 1 July 2020 - 30 November 2020
	egen s3_postGF = rowmax(s3_072020 s3_082020 s3_092020 s3_102020 s3_112020)

*s3_preGF s3_duringGF s3_postGF pct_s3_preGF pct_s3_duringGF pct_s3_postGF  

foreach x in pre during post { // Fixing continuous variables
	replace pct_s3_`x'GF = 0 if mi(pct_s3_`x'GF) & !mi(tweets_count_`x'GF) 	
}

if "`sample'"=="everyone" {
	foreach x in pct_s3_duringGF pct_s3_postGF s3_duringGF s3_postGF tweets_count_duringGF tweets_count_postGF {
		replace `x'=0 if mi(`x')
	}
}

if "`winsorised'"=="yes" {
	foreach x in pct_s3_duringGF pct_s3_postGF {
		su `x', detail
		replace `x'=r(p99) if `x'>r(p99) & !mi(`x')
	}
}

g double `treatment'Xpct_s3_duringGF = `treatment'*pct_s3_duringGF
g double `treatment'Xpct_s3_postGF = `treatment'*pct_s3_postGF
g `treatment'Xs3_duringGF = `treatment'*s3_duringGF
g `treatment'Xs3_postGF = `treatment'*s3_postGF
	
** Continuous
* Unconditional
reghdfe accepted100 `treatment' pct_s3_duringGF pct_s3_postGF `treatment'Xpct_s3_duringGF ///
	`treatment'Xpct_s3_postGF if !mi(tweets_count_duringGF) & !mi(tweets_count_postGF), ///
	ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
test `treatment'Xpct_s3_duringGF=`treatment'Xpct_s3_postGF
local pval1 = string(r(p),"%9.2g")
est sto I1
local obs1 = string(e(N),"%9.2gc")


* Conditional
reghdfe accepted100 `treatment' pct_s3_duringGF pct_s3_postGF `treatment'Xpct_s3_duringGF `treatment'Xpct_s3_postGF ///
	`treatment'##academic_female `treatment'##encoded_position `treatment'##encoded_ethnicity ///
	`treatment'##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply) ///
	`treatment'##encoded_university `treatment'##encoded_dept_fine if !mi(tweets_count_duringGF) & !mi(tweets_count_postGF), ///
	ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)	
test `treatment'Xpct_s3_duringGF=`treatment'Xpct_s3_postGF
local pval2 = string(r(p),"%9.2g")
est sto I2
local obs2 = string(e(N),"%9.2gc")

** Binary
* Unconditional
reghdfe accepted100 `treatment' s3_duringGF s3_postGF `treatment'Xs3_duringGF ///
	`treatment'Xs3_postGF if !mi(tweets_count_duringGF) & !mi(tweets_count_postGF), ///
	ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
test `treatment'Xs3_duringGF=`treatment'Xs3_postGF
local pval3 = string(r(p),"%9.2g")
est sto I3
local obs3 = string(e(N),"%9.2gc")

* Conditional
reghdfe accepted100 `treatment' s3_duringGF s3_postGF `treatment'Xs3_duringGF `treatment'Xs3_postGF ///
	`treatment'##academic_female `treatment'##encoded_position `treatment'##encoded_ethnicity ///
	`treatment'##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply) ///
	`treatment'##encoded_university `treatment'##encoded_dept_fine if !mi(tweets_count_duringGF) & !mi(tweets_count_postGF), ///
	ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)	
test `treatment'Xs3_duringGF=`treatment'Xs3_postGF
local pval4 = string(r(p),"%9.2g")
est sto I4
local obs4 = string(e(N),"%9.2gc")


** Output balance table
estout I1 I2 I3 I4 using "$tabpath/duringVpostGFA.tex", style(tex) replace ///
	keep(`treatment'X*) cells(b(star fmt(%9.2f)) se(par)) ///
	nolabel collabels(none) mlabels(none) starlevels(* 0.10 ** 0.05 *** 0.01) ///
	varlabels(`treatment'Xs3_duringGF "`treatLab' $\times$ Vocal (Binary) During George Floyd" ///
			`treatment'Xs3_postGF "`treatLab' $\times$ Vocal (Binary) After George Floyd" ///
			`treatment'Xpct_s3_duringGF "`treatLab' $\times$ Vocal (\%) During George Floyd" ///
			`treatment'Xpct_s3_postGF "`treatLab' $\times$ Vocal (\%) After George Floyd")


local tex "\input{Tables/duringVpostGFA} \\ \hline"
local tex "`tex' Observations & `obs1' & `obs2' & `obs3' & `obs4' \\"
local tex "`tex' Strata Fixed Effects & Yes & Yes & Yes & Yes \\"
local tex "`tex' Email Type Fixed Effects & Yes & Yes & Yes & Yes \\"
local tex "`tex' Interacted Controls & No & Yes & No & Yes \\"
local tex "`tex' p-value (Black $\times$ During = Black $\times$ After) & `pval1' & `pval2' & `pval3' & `pval4' \\ \toprule} \\" 
local tex "`tex' \end{tabular} }"

esttab I1 I2 I3 I4 using "$tabpath/duringVpostGF.tex", style(tex) booktabs replace ///
	d(*) nolabel collabels(none) noobs nonum postfoot("`tex'") ///
	mgroups("Meeting Accepted (\%)", pattern(1 0 0 0) ///
	prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) alignment(D{.}{.}{-1}) ///
	mtitles("(1)" "(2)" "(3)" "(4)") 	

clear
}

// table 3
if `tweet_type'==1 { // Tables for informativeness of different types of tweets

clear all
use Data/audit_anon, clear

// Choose winsorised / non-winsorised measures
local winsorised = "yes" // yes / no

// Winsorise
gen blackXpctS3NonRT = black*pctS3NonRT
if "`winsorised'"=="yes" {
	foreach x in pctS3Tweets pctS3Original pctS3NonOriginal pctS3Quoted ///
		pctS3RT pctS3Replied pctS3NonRT {

		qui su `x', det
		replace `x'=r(p99) if `x'>r(p99)
		replace blackX`x'=black*`x'		
	}	
}

la var accepted100 "Meetings Accepted (\%)"
la var s3 "Signal"
la var blackXpctS3Tweets "Black $\times$ Racial Justice Tweets (\%)"
la var blackXpctS3Original "Black $\times$ Original Racial Justice Tweets (\%)"
la var blackXpctS3NonOriginal "Black $\times$ Non-Original Racial Justice Tweets (\%)"
la var blackXpctS3Quoted "Black $\times$ Racial Justice Quote Retweets (\%)"
la var blackXpctS3RT "Black $\times$ Racial Justice Retweets (\%)"
la var blackXpctS3Replied "Black $\times$ Racial Justice Tweet Replies (\%)"
la var blackXpctS3NonRT "Black $\times$ Racial Justice Non-Retweets (\%)"

// Sample mean of key interaction terms		
		
		local i=1
		local mean_list pctS3Tweets pctS3Original pctS3NonOriginal pctS3Quoted pctS3Replied pctS3RT
		reghdfe accepted100 ///
		pctS3RT pctS3Original pctS3Quoted pctS3Replied ///
		black##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply), ///
		ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
		foreach x in `mean_list' {
			sum `x' if e(sample)==1
			local Mean`i' = string(r(mean), "%04.3f")
			local i=`i'+1
		}


// Column 1, 2: overall effect of % signalling tweets
	* Tweet controls
		eststo: reghdfe accepted100 ///
		pctS3Tweets blackXpctS3Tweets ///
		black##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply), ///
		ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
		
		estadd local StrataFE Yes
		estadd local EmailFE Yes
		estadd local Controls Twitter

		
	* All controls
		eststo: reghdfe accepted100 ///
		pctS3Tweets blackXpctS3Tweets ///
		black##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply ///
		academic_female encoded_ethnicity ///
		encoded_position encoded_dept_fine ///
		c.following_vocal_fr mi_following_network), ///
		ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)

		estadd local StrataFE Yes
		estadd local EmailFE Yes		
		estadd local Controls All
			
				
// Column 3, 4: splitting up % signalling tweets into % original and % non-original
	* Tweet controls
		eststo: reghdfe accepted100 ///
		pctS3Original pctS3NonOriginal ///
		blackXpctS3Original blackXpctS3NonOriginal ///
		black##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply), ///
		ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
		
		estadd local StrataFE Yes
		estadd local EmailFE Yes		
		estadd local Controls Twitter 
		test blackXpctS3Original = blackXpctS3NonOriginal
		local DifferencePValue1=string(`r(p)', "%04.2f")
	
	* All controls
		eststo: reghdfe accepted100 ///
		pctS3Original pctS3NonOriginal ///
		blackXpctS3Original blackXpctS3NonOriginal ///
		black##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply ///
		academic_female encoded_ethnicity ///
		encoded_position encoded_dept_fine ///
		c.following_vocal_fr mi_following_network), ///
		ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)

		estadd local StrataFE Yes
		estadd local EmailFE Yes		
		estadd local Controls All
		test blackXpctS3Original = blackXpctS3NonOriginal
		local DifferencePValue2=string(`r(p)', "%04.2f")
	

// Column 5, 6: splitting up % signalling tweets into % original signalling tweets and components of % non-original signalling tweets
	* Tweet controls
		eststo: reghdfe accepted100 ///
		pctS3Original pctS3Quoted pctS3Replied pctS3RT ///
		blackXpctS3Original blackXpctS3RT ///
		blackXpctS3Quoted blackXpctS3Replied ///
		black##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply), ///
		ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)

		estadd local StrataFE Yes
		estadd local EmailFE Yes		
		estadd local Controls Twitter
		test blackXpctS3RT = blackXpctS3Original
		local DifferencePValue3=string(`r(p)', "%04.2f")		
		
	* All controls	
		eststo: reghdfe accepted100 ///
		pctS3Original pctS3Quoted pctS3Replied pctS3RT ///
		blackXpctS3Original blackXpctS3RT ///
		blackXpctS3Quoted blackXpctS3Replied ///
		black##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply ///
		academic_female encoded_ethnicity ///
		encoded_position encoded_dept_fine ///
		c.following_vocal_fr mi_following_network), ///
		ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)

		estadd local StrataFE Yes
		estadd local EmailFE Yes		
		estadd local Controls All
		test blackXpctS3RT = blackXpctS3Original
		local DifferencePValue4=string(`r(p)', "%04.2f")		


// Table output		
	
	local tex "`tex' \textit{p}-value &  &  & `DifferencePValue1' & `DifferencePValue2' & `DifferencePValue3' & `DifferencePValue4'"

estout est1 est2 est3 est4 est5 est6 using "$tabpath/signal_strength_table_w-`winsorised'A.tex", style(tex) replace ///
	keep(blackX*) label cells(b(star fmt(%9.2f)) se(par)) ///
	collabels(none) mlabels(none) starlevels(* 0.10 ** 0.05 *** 0.01) stats(N StrataFE EmailFE Controls, fmt(%12.0gc 0 3) labels("Observations" "Strata Fixed Effects" "Email Type Fixed Effects" "Controls")) ///
	varlabels(, elist(blackXpctS3Replied \hline)) ///
	postfoot(`"`tex'"')

	local tex "\input{Tables/signal_strength_table_w-`winsorised'A} \\ \hline \end{tabular}}"

	
esttab est1 est2 est3 est4 est5 est6 using "$tabpath/signal_strength_table_w-`winsorised'.tex", style(tex) replace booktabs ///
d(*) noobs label nonum nonotes collabels(none) postfoot(`"`tex'"') ///
mgroups("Meeting Accepted (\%)", pattern(1 0 0 0 0 0) ///
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) alignment(D{.}{.}{-1}) ///
mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)") 
	

// Display sample means of key variables (to double check table footnote in Overleaf)
local types "pctS3Tweets pctS3Original pctS3NonOriginal pctS3Quoted pctS3Replied pctS3RT"
forval i=1/5 {
	local type: word `i' of `types'
	dis "`type' Mean: `Mean`i''"
}

clear
	
				
}

// fig A9
if `detection'==1 { // Produce detection robustness figure


foreach treatment in firstgen black female{
local outcome = "accepted" // accepted / helpful / replied	
		
	if "`treatment'"=="black" {
			local control = "white"
	}
	else if "`treatment'"=="female" {
			local control = "male"
	}
	else if "`treatment'"=="firstgen" {
			local control = "nextgen"
	}
		
	local treatLab = proper("`treatment'")
	local ctrlLab = proper("`control'")
		
	use Data/audit_anon, clear
		
	g beta = .
	g u95 = .
	g l95 = .
		
	
	** 1. Racial Discrimination: Full Sample **
		* among Signallers *
		// get point estimate and 95% CI
		reghdfe `outcome' `treatment'XnonS3 s3 `treatment'Xs3, ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	 
		local i=10
		replace beta = _b[`treatment'Xs3] in `i'
		replace u95 = _b[`treatment'Xs3]+_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		replace l95 = _b[`treatment'Xs3]-_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		
		
		* among Non-Signallers and Unconditional Difference *
		// get point estimate and 95% CI	
		reghdfe `outcome' `treatment' s3 `treatment'Xs3, ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
		
		local i=1
		foreach x in `treatment' `treatment'Xs3 {
			replace beta = _b[`x'] in `i'
			replace u95 = _b[`x']+_se[`x']*invttail(e(df_r),0.025) in `i'
			replace l95 = _b[`x']-_se[`x']*invttail(e(df_r),0.025) in `i'
			local i=`i'+18
		}
		
		
		* Conditional Difference *
		// get point estimate and 95% CI
		reghdfe `outcome' `treatment' s3 `treatment'Xs3 `treatment'##academic_female ///
		`treatment'##encoded_position `treatment'##encoded_ethnicity ///
		`treatment'##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply) ///
		`treatment'##encoded_university `treatment'##encoded_dept_fine, ///
		ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
		
		local i=28
		replace beta = _b[`treatment'Xs3] in `i'
		replace u95 = _b[`treatment'Xs3]+_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		replace l95 = _b[`treatment'Xs3]-_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'

		
	** 2. Racial Discrimination: Sample Without Contaminated **
		* among Signallers *
		// get point estimate and 95% CI
		reghdfe `outcome' `treatment'XnonS3 s3 `treatment'Xs3 if contaminated==0, ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	 
		local i=11
		replace beta = _b[`treatment'Xs3] in `i'
		replace u95 = _b[`treatment'Xs3]+_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		replace l95 = _b[`treatment'Xs3]-_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		
		
		** among Non-Signallers and Unconditional Difference **
		// get point estimate and 95% CI	
		reghdfe `outcome' `treatment' s3 `treatment'Xs3 if contaminated==0, ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
		
		local i=2
		foreach x in `treatment' `treatment'Xs3 {
			replace beta = _b[`x'] in `i'
			replace u95 = _b[`x']+_se[`x']*invttail(e(df_r),0.025) in `i'
			replace l95 = _b[`x']-_se[`x']*invttail(e(df_r),0.025) in `i'
			local i=`i'+18
		}
	
		* Conditional Difference *
		// get point estimate and 95% CI
		reghdfe `outcome' `treatment' s3 `treatment'Xs3 `treatment'##academic_female ///
		`treatment'##encoded_position `treatment'##encoded_ethnicity ///
		`treatment'##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply) ///
		`treatment'##encoded_university `treatment'##encoded_dept_fine if contaminated==0, ///
		ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
		
		local i=29
		replace beta = _b[`treatment'Xs3] in `i'
		replace u95 = _b[`treatment'Xs3]+_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		replace l95 = _b[`treatment'Xs3]-_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		
		
	** 3. Racial Discrimination: Sample Without Social Science **
		* among Signallers *
		// get point estimate and 95% CI
		reghdfe `outcome' `treatment'XnonS3 s3 `treatment'Xs3 if !inlist(encoded_dept_broad,7), ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	
		local i=12
		replace beta = _b[`treatment'Xs3] in `i'
		replace u95 = _b[`treatment'Xs3]+_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		replace l95 = _b[`treatment'Xs3]-_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
	
		** among Non-Signallers and Unconditional Difference **
		// get point estimate and 95% CI	
		reghdfe `outcome' `treatment' s3 `treatment'Xs3 if !inlist(encoded_dept_broad,7), ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
		
		local i=3
		foreach x in `treatment' `treatment'Xs3 {
			replace beta = _b[`x'] in `i'
			replace u95 = _b[`x']+_se[`x']*invttail(e(df_r),0.025) in `i'
			replace l95 = _b[`x']-_se[`x']*invttail(e(df_r),0.025) in `i'
			local i=`i'+18
		}
		
		* Conditional Difference *
		// get point estimate and 95% CI
		reghdfe `outcome' `treatment' s3 `treatment'Xs3 `treatment'##academic_female ///
		`treatment'##encoded_position `treatment'##encoded_ethnicity ///
		`treatment'##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply) ///
		`treatment'##encoded_university `treatment'##encoded_dept_fine if !inlist(encoded_dept_broad,7), ///
		ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
		
		local i=30
		replace beta = _b[`treatment'Xs3] in `i'
		replace u95 = _b[`treatment'Xs3]+_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		replace l95 = _b[`treatment'Xs3]-_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
	
	
	** 4. Racial Discrimination: Sample Without Generic **
		* among Signallers *
		// get point estimate and 95% CI
		reghdfe `outcome' `treatment'XnonS3 s3 `treatment'Xs3 if generic==0, ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	 
		local i=13
		replace beta = _b[`treatment'Xs3] in `i'
		replace u95 = _b[`treatment'Xs3]+_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		replace l95 = _b[`treatment'Xs3]-_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		
		
		** among Non-Signallers and Unconditional Difference **
		// get point estimate and 95% CI	
		reghdfe `outcome' `treatment' s3 `treatment'Xs3 if generic==0, ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
		
		local i=4
		foreach x in `treatment' `treatment'Xs3 {
			replace beta = _b[`x'] in `i'
			replace u95 = _b[`x']+_se[`x']*invttail(e(df_r),0.025) in `i'
			replace l95 = _b[`x']-_se[`x']*invttail(e(df_r),0.025) in `i'
			local i=`i'+18
		}	
	
		* Conditional Difference *
		// get point estimate and 95% CI
		reghdfe `outcome' `treatment' s3 `treatment'Xs3 `treatment'##academic_female ///
		`treatment'##encoded_position `treatment'##encoded_ethnicity ///
		`treatment'##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply) ///
		`treatment'##encoded_university `treatment'##encoded_dept_fine if generic==0, ///
		ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
		
		local i=31
		replace beta = _b[`treatment'Xs3] in `i'
		replace u95 = _b[`treatment'Xs3]+_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		replace l95 = _b[`treatment'Xs3]-_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'

	
	** 5. Racial Discrimination: No. of professors e-mailed at this department-school is at most 10 **
		* among Signallers *
		// get point estimate and 95% CI
		reghdfe `outcome' `treatment'XnonS3 s3 `treatment'Xs3 if deptschlSize<=10, ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	 
		local i=14
		replace beta = _b[`treatment'Xs3] in `i'
		replace u95 = _b[`treatment'Xs3]+_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		replace l95 = _b[`treatment'Xs3]-_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		
		
		** among Non-Signallers and Unconditional Difference **
		// get point estimate and 95% CI	
		reghdfe `outcome' `treatment' s3 `treatment'Xs3 if deptschlSize<=10, ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
		
		local i=5
		foreach x in `treatment' `treatment'Xs3 {
			replace beta = _b[`x'] in `i'
			replace u95 = _b[`x']+_se[`x']*invttail(e(df_r),0.025) in `i'
			replace l95 = _b[`x']-_se[`x']*invttail(e(df_r),0.025) in `i'
			local i=`i'+18
		}	
	
		
		* Conditional Difference *
		// get point estimate and 95% CI
		reghdfe `outcome' `treatment' s3 `treatment'Xs3 `treatment'##academic_female ///
		`treatment'##encoded_position `treatment'##encoded_ethnicity ///
		`treatment'##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply) ///
		`treatment'##encoded_university `treatment'##encoded_dept_fine if deptschlSize<=10, ///
		ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
		
		local i=32
		replace beta = _b[`treatment'Xs3] in `i'
		replace u95 = _b[`treatment'Xs3]+_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		replace l95 = _b[`treatment'Xs3]-_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		
		
	** 6. Racial Discrimination: No. of professors e-mailed at this department-school is at most 5 **
		* among Signallers *
		// get point estimate and 95% CI
		reghdfe `outcome' `treatment'XnonS3 s3 `treatment'Xs3 if deptschlSize<=5, ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	 
		local i=15
		replace beta = _b[`treatment'Xs3] in `i'
		replace u95 = _b[`treatment'Xs3]+_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		replace l95 = _b[`treatment'Xs3]-_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		
		
		** among Non-Signallers and Unconditional Difference **
		// get point estimate and 95% CI	
		reghdfe `outcome' `treatment' s3 `treatment'Xs3 if deptschlSize<=5, ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
		
		local i=6
		foreach x in `treatment' `treatment'Xs3 {
			replace beta = _b[`x'] in `i'
			replace u95 = _b[`x']+_se[`x']*invttail(e(df_r),0.025) in `i'
			replace l95 = _b[`x']-_se[`x']*invttail(e(df_r),0.025) in `i'
			local i=`i'+18
		}	
		
		* Conditional Difference *
		// get point estimate and 95% CI
		reghdfe `outcome' `treatment' s3 `treatment'Xs3 `treatment'##academic_female ///
		`treatment'##encoded_position `treatment'##encoded_ethnicity ///
		`treatment'##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply) ///
		`treatment'##encoded_university `treatment'##encoded_dept_fine if deptschlSize<=5, ///
		ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
		
		local i=33
		replace beta = _b[`treatment'Xs3] in `i'
		replace u95 = _b[`treatment'Xs3]+_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		replace l95 = _b[`treatment'Xs3]-_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		
	
	** 7. Racial Discrimination: Outcome is email accepted within 1 day **
	local outcome_orig = "`outcome'"
	local outcome = "acceptedWithin1Days"
	
		* among Signallers *
		// get point estimate and 95% CI
		reghdfe `outcome' `treatment'XnonS3 s3 `treatment'Xs3, ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	 
		local i=16
		replace beta = _b[`treatment'Xs3] in `i'
		replace u95 = _b[`treatment'Xs3]+_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		replace l95 = _b[`treatment'Xs3]-_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		
		
		** among Non-Signallers and Unconditional Difference **
		// get point estimate and 95% CI	
		reghdfe `outcome' `treatment' s3 `treatment'Xs3, ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
		
		local i=7
		foreach x in `treatment' `treatment'Xs3 {
			replace beta = _b[`x'] in `i'
			replace u95 = _b[`x']+_se[`x']*invttail(e(df_r),0.025) in `i'
			replace l95 = _b[`x']-_se[`x']*invttail(e(df_r),0.025) in `i'
			local i=`i'+18
		}	
		
		* Conditional Difference *
		// get point estimate and 95% CI
		reghdfe `outcome' `treatment' s3 `treatment'Xs3 `treatment'##academic_female ///
		`treatment'##encoded_position `treatment'##encoded_ethnicity ///
		`treatment'##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply) ///
		`treatment'##encoded_university `treatment'##encoded_dept_fine, ///
		ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
		
		local i=34
		replace beta = _b[`treatment'Xs3] in `i'
		replace u95 = _b[`treatment'Xs3]+_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		replace l95 = _b[`treatment'Xs3]-_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		
		
	** 8. Racial Discrimination: Outcome is email accepted within 3 days **
	local outcome = "acceptedWithin3Days"
	
		* among Signallers *
		// get point estimate and 95% CI
		reghdfe `outcome' `treatment'XnonS3 s3 `treatment'Xs3, ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
	 
		local i=17
		replace beta = _b[`treatment'Xs3] in `i'
		replace u95 = _b[`treatment'Xs3]+_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		replace l95 = _b[`treatment'Xs3]-_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		
		
		** among Non-Signallers and Unconditional Difference **
		// get point estimate and 95% CI	
		reghdfe `outcome' `treatment' s3 `treatment'Xs3, ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
		
		local i=8
		foreach x in `treatment' `treatment'Xs3 {
			replace beta = _b[`x'] in `i'
			replace u95 = _b[`x']+_se[`x']*invttail(e(df_r),0.025) in `i'
			replace l95 = _b[`x']-_se[`x']*invttail(e(df_r),0.025) in `i'
			local i=`i'+18
		}	
		
		* Conditional Difference *
		// get point estimate and 95% CI
		reghdfe `outcome' `treatment' s3 `treatment'Xs3 `treatment'##academic_female ///
		`treatment'##encoded_position `treatment'##encoded_ethnicity ///
		`treatment'##(c.countOriginal c.countQuoted c.countQuotedReplied c.countRT c.countReply) ///
		`treatment'##encoded_university `treatment'##encoded_dept_fine, ///
		ab(deptStrata EmailTypeFull) vce(cluster deptschlXName)
		
		local i=35
		replace beta = _b[`treatment'Xs3] in `i'
		replace u95 = _b[`treatment'Xs3]+_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		replace l95 = _b[`treatment'Xs3]-_se[`treatment'Xs3]*invttail(e(df_r),0.025) in `i'
		
		
// plot
	keep in 1/35
	keep beta u95 l95
	foreach var of varlist beta l95 u95 {
		replace `var'=`var'*100
	}
	g order = _n
	g colour = mod(order,9)==1 // to enable bars to be coloured based on the different specifications
	replace colour = colour[_n-1]+1 if colour[_n]==0 
	replace colour=. if colour==9 | colour==18 | colour==27
	
	if "`treatment'"=="black" {
		local outtitle = "Black Minus White Gap"
	}
	else if "`treatment'"=="female" {
		local outtitle = "Female Minus Male Gap"
	}
	else if "`treatment'"=="firstgen" {
		local outtitle = "First-Gen Minus Regular Gap"
	}
	
	if "`outcome_orig'"=="accepted" {
		local rangemax = 10
		local outtitle = "`outtitle'" + " in Meeting Acceptance (p.p.)"
	}
	else if "`outcome_orig'"=="helpful" {
		local rangemax = 15
		local outtitle = "`outtitle'" + " in Accepting Meeting or Helpful (p.p)"
	}
	else if "`outcome_orig'"=="replied" {
		local rangemax = 15
		local outtitle = "`outtitle'" + " in Replying (p.p)"
	}	
	

// full figure	
	twoway (scatter beta order if colour==1, msymbol(circle) mcolor(purple)) ///
		   (rcap l95 u95 order if colour==1, lcolor(purple)) ///
		   (scatter beta order if colour==2, msymbol(X) mcolor(cranberry)) ///
		   (rcap l95 u95 order if colour==2, lcolor(cranberry)) ///
		   (scatter beta order if colour==3, msymbol(triangle) mcolor(dkorange)) ///
		   (rcap l95 u95 order if colour==3, lcolor(dkorange)) ///
		   (scatter beta order if colour==4, msymbol(square) mcolor(erose)) ///
		   (rcap l95 u95 order if colour==4, lcolor(erose)) ///
		   (scatter beta order if colour==5, msymbol(plus) mcolor(ebblue)) ///
		   (rcap l95 u95 order if colour==5, lcolor(ebblue)) ///
		   (scatter beta order if colour==6, msymbol(diamond) mcolor(green)) ///
		   (rcap l95 u95 order if colour==6, lcolor(green)) ///
		   (scatter beta order if colour==7, msymbol(circle_hollow) mcolor(emerald%70)) ///
		   (rcap l95 u95 order if colour==7, lcolor(emerald%70)) ///
		   (scatter beta order if colour==8, msymbol(triangle_hollow) mcolor(sand)) ///
		   (rcap l95 u95 order if colour==8, lcolor(sand)), ///
		   legend(order(1 "1" 3 "2" 5 "3" 7 "4" 9 "5" 11 "6" 13 "7" 15 "8")) ///		   
		   `title' ///
		   ytitle("`outtitle'", size(4)) ///
		   xtitle("") ///
		   xlabel(4.5 "Silent" 13.5 "Vocal" 22.5 `" "Difference" "(Unconditional)" "' 31.5 `" "Difference" "(Conditional)" "', noticks nogrid labsize(4)) ///
		   xtick(0(9)36) ///
		   xline(9, lpattern(dash) lcolor(gs13)) ///
		   xline(18, lpattern(dash) lcolor(gs13)) ///
		   xline(27, lpattern(dash) lcolor(gs13)) ///
		   yline(0, lpattern(dash) lcolor(black)) ///
		   xsize(8)	   
	graph export "$figpath/robustness_`outcome_orig'_`treatment'.`img'", replace

	}
}	

// fig A13
if `daily_signallers_2020'==1 { // Produce graph showing daily number of signallers during 2020 for the audit sample (11,450) and Black professors (1,094)

	use Data/signals_by_date, clear
		
	
	la var signaller_black "number of black signallers"
	la var signaller_non_black "number of non-black signallers"

	g signaller_black_pct = (signaller_black/1094)*100
	g signaller_non_black_pct = (signaller_non_black/11450)*100
	
// Figure

	tsset date 

	twoway (tsline signaller_non_black_pct, msize(0.1) lcolor(blue%70)) ///
		   (tsline signaller_black_pct, msize(0.1) lcolor(dkgreen%70) lstyle(solid)), ///
	ytitle("Vocal Academics (%)", size(4)) ///
	ylabel(, labsize(4) nogrid) ///
	tla(01jan2020 01feb2020 01mar2020 01apr2020 01may2020 01jun2020 01jul2020 01aug2020 01sep2020 01oct2020 01nov2020 01dec2020 01jan2021, angle(50)) ///
	tlabel(, format(%tdMon_DD) labsize(3.5)) ///
	xtitle("") ///
	`title' ///
	tline(25may2020, lcolor(black)) ///
	legend(position(6) order(1 "Non-Black Academics" 2 "Black Academics") size(3.5))
	graph export "$figpath/daily_signallers_BLM.`img'", replace
	
	twoway tsline signaller_non_black_pct, msize(0.1) lcolor(blue%70) ///
		ytitle("Vocal Academics (%)", size(4)) ///
		ylabel(, labsize(4) nogrid) ///
		tla(01jan2020 01feb2020 01mar2020 01apr2020 01may2020 01jun2020 01jul2020 01aug2020 01sep2020 01oct2020 01nov2020 01dec2020 01jan2021, angle(50)) ///
		tlabel(, format(%tdMon_DD) labsize(3.5)) ///
		xtitle("") ///
		`title' ///
		tline(25may2020, lcolor(black))
	graph export "$figpath/daily_nonblack_signallers_BLM.`img'", replace	
}



 
beep

