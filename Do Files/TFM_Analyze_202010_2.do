	*****************************************************************************
	*					PERU YOUNG LIVES PANEL DATA								   *
	*					Rounds 1 to 5, OC       								   *
	********************************************************************************

	* Author: 		Sara Gonzales
	* Date: 		22 Oct 2020
	* Last Update: 	29 Oct 2020
	* Last run: 	29 Oct 2020
		
		/* number of observations (as of 22 May 2018)

					   |                    Round of survey
		Younger cohort |         1          2          3          4          5 
		---------------+-------------------------------------------------------
		  Older cohort |       714        685        678        635        608 
		Younger cohort |     2,052      1,963      1,943      1,902      1,860 
		---------------+-------------------------------------------------------
				 Total |     2,766      2,648      2,621      2,537      2,468 */	
		
	/*-----------------------------------------------------------------------------*
									DATA SETS
	------------------------------------------------------------------------------*/
	clear
	set mem 600m
	set more off

	global wdir 	"/Users/saraynes.gs/Google Drive/Data_YL/Analyze/output"
	global dpanel 	"/Users/saraynes.gs/Google Drive/Data_YL/Analyze/input"

	cd "$wdir"

	* Regressions Older Cohorts - 2nd Transition: Secondary to Tertiary
	* Using mediating analysis: KHB 

	/* =========================================================================== *
								  DATA MANAGMENT
	 * =========================================================================== */
	 
	use "$dpanel/TFM_Peru.dta", clear

	/*-----------------------------------------------------------------------------*
								Renaming some variables
	------------------------------------------------------------------------------*/

	rename pEd_0			P_None
	rename pEd_1			P_Primary
	rename pEd_2 			P_HighSchool
	rename pEd_3    		P_College
	rename scoreST_ravens1  Cog_Ravens
	rename scoreST_cda2		Cog_CDA
	rename scoreST_math4	Cog_RMath // here is the 4th ; in the other transition scoreST_rmath3
	rename L1hhSize			hh_Size
	rename childOldest    	Oldest
	rename childFemale		Female
	rename childNoSpanish 	NonSpanish
	rename rural			Rural
	rename L1hhWealth 		Wealth
	rename childAgeGr1		AgeGr1
	rename SC_mem_sup1		SC_Membership_sup
	rename SC_mem1			SC_Membership
	rename SC_sup1			SC_Support
	rename commwork			Commute_Work
	rename timesch			Commute_School
	rename sch_public		Public
	rename sch_private		Private_School


	/*-----------------------------------------------------------------------------*
							Labeling variables
	------------------------------------------------------------------------------*/
	label var P_None 		"PE None"
	label var P_Primary 	"PE Primary"
	label var P_HighSchool 	"PE HighSchool"
	label var P_College 	"PE College"
	label var hh_Size 		"Household size"
	label var Cog_Ravens	"Cognitive: Ravens"
	label var Cog_CDA		"Cognitive: CDA"
	label var Cog_RMath		"Cognitive: RMath"

	/*-----------------------------------------------------------------------------*
							 Generating variable
	------------------------------------------------------------------------------*/

	* Combining PtoS3 (Older cohort) and PtoS5 (Younger cohort) into 1 variable
	gen StoT=.
	replace StoT = StoT5 if ( yc==0 & round==5)

	label var StoT "Transitions from Secondary to Tertiary"
	label define StoT 0 "Not Continue" 1 "Continue"
	label value StoT StoT

	* Parents' education
	gen pEd=0
	replace pEd=1 if P_Primary==1
	replace pEd=2 if P_HighSchool==1 | P_College==1

	* gen P_Aspirations = p_aspiration_ed * p_expectation_ed

	recode Loc3 (0 = 2) (2 = 0)

	recode entype (1 3 4 = 0) (2 = 1) , gen(School) 

	* Combining L1.Perf (Older cohort) and L2.Perf (Younger cohort) into 1 variable

	gen LPerf=.
	replace LPerf = L1perf 

	*recode NonSpanish (1 = 0) (0 =1)
					 
	*gen perf_2 = (L1perf==0) // Good
	*gen perf_0 = (L1perf==1) // Average
	*gen perf_1 = (L1perf==2) // Bad 

	/*-----------------------------------------------------------------------------*
								Sample selection
	------------------------------------------------------------------------------*/

	* The regression for the 1st transition
	keep if (yc==0 & round==5)

	/*-----------------------------------------------------------------------------*
								Some descriptives!
	------------------------------------------------------------------------------*/
	* Descriptive tabulation od subsample
	tab yc round

	tab StoT

	tab NonSpanish Rural 
	disp 540/595 // Spanish and Urban 0.90756303 %

	tempfile StoT
	save `StoT'

	/* =========================================================================== *
									 REGRESSIONS
	 * =========================================================================== */
	 
	 use `StoT', clear
	 global Controls c.L1hwork i.L1sch_public c.hh_Size c.AgeGr1
	 
	/*-----------------------------------------------------------------------------*
				             Linear Probability Regressiones
	------------------------------------------------------------------------------*/

	reg StoT ib1.pEd i.Female, vce(cluster clustid)
	regsave using StoT_LPM, detail(all) table(reg1, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) replace

	reg StoT ib1.pEd i.Female i.L2perf c.Cog_RMath , vce(cluster clustid)
	regsave using StoT_LPM, detail(all) table(reg2, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append			
			
	reg StoT ib1.pEd i.Female i.L2perf c.Cog_RMath  $Controls , vce(cluster clustid)
	regsave using StoT_LPM, detail(all) table(reg3, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append		
			
	reg StoT c.Wealth  i.Female , vce(cluster clustid)
	regsave using StoT_LPM, detail(all) table(reg4, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append

	reg StoT c.Wealth  i.Female i.L2perf c.Cog_RMath, vce(cluster clustid)
	regsave using StoT_LPM, detail(all) table(reg5, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append

	reg StoT c.Wealth  i.Female i.L2perf c.Cog_RMath  $Controls, vce(cluster clustid)
	regsave using StoT_LPM, detail(all) table(reg6, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append
			
	reg StoT i.NonSpanish  i.Female, vce(cluster clustid)
	regsave using StoT_LPM, detail(all) table(reg7, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append

	reg StoT i.NonSpanish  i.Female i.L2perf c.Cog_RMath, vce(cluster clustid)
	regsave using StoT_LPM, detail(all) table(reg8, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append

	reg StoT i.NonSpanish  i.Female i.L2perf c.Cog_RMath  $Controls, vce(cluster clustid)
	regsave using StoT_LPM, detail(all) table(reg9, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append			
			
	reg StoT i.Rural i.Female , vce(cluster region)
	regsave using StoT_LPM, detail(all) table(reg10, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append

	reg StoT i.Rural i.Female i.L2perf c.Cog_RMath , vce(cluster clustid)
	regsave using StoT_LPM, detail(all) table(reg11, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append

	reg StoT i.Rural i.Female i.L2perf c.Cog_RMath  $Controls, vce(cluster clustid)
	regsave using StoT_LPM, detail(all) table(reg12, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append	
			
	reg StoT ib1.pEd c.Wealth i.Rural  i.Female, vce(cluster clustid)
	regsave using StoT_LPM, detail(all) table(reg13, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append

	reg StoT ib1.pEd c.Wealth i.Rural  i.Female i.L2perf c.Cog_RMath, vce(cluster clustid)
	regsave using StoT_LPM, detail(all) table(reg14, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append

	reg StoT ib1.pEd c.Wealth i.Rural i.Female i.L2perf c.Cog_RMath $Controls , vce(cluster clustid)
	regsave using StoT_LPM, detail(all) table(reg15, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append	

	reg StoT ib2.Loc5 i.Female , vce(cluster region)
	regsave using StoT_LPM2, detail(all) table(reg10b, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) replace

	reg StoT ib2.Loc5 i.Female i.L2perf c.Cog_RMath , vce(cluster clustid)
	regsave using StoT_LPM2, detail(all) table(reg11b, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append

	reg StoT ib2.Loc5 i.Female i.L2perf c.Cog_RMath  $Controls, vce(cluster clustid)
	regsave using StoT_LPM2, detail(all) table(reg12b, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append	
	
			
	use StoT_LPM, clear

	drop if strpos(var,"_cons")!=0

	drop if var == "rank" | var == "ic" | var == "k" | var == "k_eq" | var =="k_dv" | ///
			var == "converged" | var == "rc" | var == "k_autoCns" | var == "k_eq_model" | ///
			var == "rc" | var == "k_autoCns" | var == "k_eq_model" | var == "cmdline" | ///
			var == "df_m" |  var== "p" | var == "N_cdf" | var == "estat_cmd" | ///
			var == "predict" | var == "marginsnotok" | var == "title" | var == "opt" | ///
			var == "N_cds"  | var == "cmd" | var == "chi2type" | var == "vcetype" | ///
			var == "vce" | var == "user" | var == "crittype" | var == "ml_method" | ///
			var == "singularHmethod" | var == "technique" | var == "which" | ///
			var == "depvar" | var == "properties" | var == "chi2" | var == "r2_p" | ///
			var == "ll_0" | var == "marginsok" | var == "df_r" | var == "rmse" | ///
			var == "mss" | var == "rss" | var == "model"

	compress // variables have a str160 display format (como autojustificar)

	replace var = subinstr(var,"_coef","",.)

	replace var = "" if strpos(var,"_stderr")!=0

	*replace var = "Pseudo R2" if var == "r2_p"

	replace var = "Log likelihood " if var == "ll"

	texsave using "StoT_LPM.tex", title("Secondary to Tertiary: Linear Probability Model") ///
			footnote("A */**/*** next to the coefficient indicates significance at the 10/5/1% level.") ///
			nonames autonumber hlines(-2) replace		
			
	export excel using "StoT_LPM", sheet("StoT_LPM") firstrow(variables) replace /// replace (use replace the second time)		

	
	
	use StoT_LPM2, clear

	drop if strpos(var,"_cons")!=0

	drop if var == "rank" | var == "ic" | var == "k" | var == "k_eq" | var =="k_dv" | ///
			var == "converged" | var == "rc" | var == "k_autoCns" | var == "k_eq_model" | ///
			var == "rc" | var == "k_autoCns" | var == "k_eq_model" | var == "cmdline" | ///
			var == "df_m" |  var== "p" | var == "N_cdf" | var == "estat_cmd" | ///
			var == "predict" | var == "marginsnotok" | var == "title" | var == "opt" | ///
			var == "N_cds"  | var == "cmd" | var == "chi2type" | var == "vcetype" | ///
			var == "vce" | var == "user" | var == "crittype" | var == "ml_method" | ///
			var == "singularHmethod" | var == "technique" | var == "which" | ///
			var == "depvar" | var == "properties" | var == "chi2" | var == "r2_p" | ///
			var == "ll_0" | var == "marginsok" | var == "df_r" | var == "rmse" | ///
			var == "mss" | var == "rss" | var == "model"

	compress // variables have a str160 display format (como autojustificar)

	replace var = subinstr(var,"_coef","",.)

	replace var = "" if strpos(var,"_stderr")!=0

	*replace var = "Pseudo R2" if var == "r2_p"

	replace var = "Log likelihood " if var == "ll"

	texsave using "StoT_LPM.tex", title("Secondary to Tertiary: Linear Probability Model. Location") ///
			footnote("A */**/*** next to the coefficient indicates significance at the 10/5/1% level.") ///
			nonames autonumber hlines(-2) replace		
			
	export excel using "StoT_LPM2", sheet("StoT_LPM2") firstrow(variables) replace /// replace (use replace the second time)		

	
/*-----------------------------------------------------------------------------*
							Linear Probability Regressiones
							     Mediation Analysis
------------------------------------------------------------------------------*/
use `StoT', clear

khb reg StoT c.parEduC2      || i.L2perf c.Cog_RMath, c(i.Female) vce(cluster clustid) summary // disentangle when there is more than one mediator!		

khb reg StoT c.Wealth 	  || i.L2perf c.Cog_RMath, c(i.Female) vce(cluster clustid) summary // disentangle

khb reg StoT i.NonSpanish || i.L2perf c.Cog_RMath, c(i.Female) vce(cluster clustid) summary 

khb reg StoT i.Rural	  || i.L2perf c.Cog_RMath, c(i.Female) vce(cluster clustid) summary 

/*-----------------------------------------------------------------------------*
				           Logistic Probability Regressiones
------------------------------------------------------------------------------*/

use `StoT', clear

logit StoT ib1.pEd i.Female, vce(cluster clustid)
regsave using StoT_logit, detail(all) table(reg1, format(%5.2f) parentheses(stderr) ///
		asterisk(10 5 1) order(N ll regvars)) replace

logit StoT ib1.pEd i.Female i.L2perf c.Cog_RMath , vce(cluster clustid)
regsave using StoT_logit, detail(all) table(reg2, format(%5.2f) parentheses(stderr) ///
		asterisk(10 5 1) order(N ll regvars)) append			
			
logit StoT ib1.pEd i.Female i.L2perf c.Cog_RMath  $Controls , vce(cluster clustid)
regsave using StoT_logit, detail(all) table(reg3, format(%5.2f) parentheses(stderr) ///
		asterisk(10 5 1) order(N ll regvars)) append		
			
logit StoT c.Wealth  i.Female , vce(cluster region)
regsave using StoT_logit, detail(all) table(reg4, format(%5.2f) parentheses(stderr) ///
		asterisk(10 5 1) order(N ll regvars)) append

logit StoT c.Wealth  i.Female i.L2perf c.Cog_RMath, vce(cluster clustid)
regsave using StoT_logit, detail(all) table(reg5, format(%5.2f) parentheses(stderr) ///
		asterisk(10 5 1) order(N ll regvars)) append

logit StoT c.Wealth  i.Female i.L2perf c.Cog_RMath  $Controls, vce(cluster clustid)
regsave using StoT_logit, detail(all) table(reg6, format(%5.2f) parentheses(stderr) ///
		asterisk(10 5 1) order(N ll regvars)) append
			
logit StoT i.NonSpanish  i.Female, vce(cluster clustid)
regsave using StoT_logit, detail(all) table(reg7, format(%5.2f) parentheses(stderr) ///
		asterisk(10 5 1) order(N ll regvars)) append

logit StoT i.NonSpanish  i.Female i.L2perf c.Cog_RMath, vce(cluster clustid)
regsave using StoT_logit, detail(all) table(reg8, format(%5.2f) parentheses(stderr) ///
		asterisk(10 5 1) order(N ll regvars)) append

logit StoT i.NonSpanish  i.Female i.L2perf c.Cog_RMath  $Controls, vce(cluster clustid)
regsave using StoT_logit, detail(all) table(reg9, format(%5.2f) parentheses(stderr) ///
		asterisk(10 5 1) order(N ll regvars)) append			
			
logit StoT i.Rural i.Female , vce(cluster clustid)
regsave using StoT_logit, detail(all) table(reg10, format(%5.2f) parentheses(stderr) ///
		asterisk(10 5 1) order(N ll regvars)) append

logit StoT i.Rural i.Female i.L2perf c.Cog_RMath , vce(cluster clustid)
regsave using StoT_logit, detail(all) table(reg11, format(%5.2f) parentheses(stderr) ///
		asterisk(10 5 1) order(N ll regvars)) append

logit StoT i.Rural i.Female i.L2perf c.Cog_RMath  $Controls, vce(cluster clustid)
regsave using StoT_logit, detail(all) table(reg12, format(%5.2f) parentheses(stderr) ///
		asterisk(10 5 1) order(N ll regvars)) append	
			
logit StoT ib1.pEd c.Wealth i.Rural  i.Female, vce(cluster clustid)
regsave using StoT_logit, detail(all) table(reg13, format(%5.2f) parentheses(stderr) ///
		asterisk(10 5 1) order(N ll regvars)) append

logit StoT ib1.pEd c.Wealth i.Rural  i.Female i.L2perf c.Cog_RMath, vce(cluster clustid)
regsave using StoT_logit, detail(all) table(reg14, format(%5.2f) parentheses(stderr) ///
		asterisk(10 5 1) order(N ll regvars)) append

logit StoT ib1.pEd c.Wealth i.Rural i.Female i.L2perf c.Cog_RMath $Controls , vce(cluster clustid)
regsave using StoT_logit, detail(all) table(reg15, format(%5.2f) parentheses(stderr) ///
		asterisk(10 5 1) order(N ll regvars)) append				

logit StoT ib2.Loc5 i.Female , vce(cluster region)
regsave using StoT_logit2, detail(all) table(reg10b, format(%5.2f) parentheses(stderr) ///
		asterisk(10 5 1) order(N ll regvars)) replace				

logit StoT ib2.Loc5 i.Female i.L2perf c.Cog_RMath , vce(cluster clustid)
regsave using StoT_logit2, detail(all) table(reg11b, format(%5.2f) parentheses(stderr) ///
		asterisk(10 5 1) order(N ll regvars)) append
		
logit StoT ib2.Loc5 i.Female i.L2perf c.Cog_RMath  $Controls, vce(cluster clustid)
regsave using StoT_logit2, detail(all) table(reg12b, format(%5.2f) parentheses(stderr) ///
		asterisk(10 5 1) order(N ll regvars)) append
			

use StoT_logit, clear

drop if strpos(var,"_cons")!=0

drop if var == "rank" | var == "ic" | var == "k" | var == "k_eq" | var =="k_dv" | ///
		var == "converged" | var == "rc" | var == "k_autoCns" | var == "k_eq_model" | ///
		var == "rc" | var == "k_autoCns" | var == "k_eq_model" | var == "cmdline" | ///
		var == "df_m" |  var== "p" | var == "N_cdf" | var == "estat_cmd" | ///
		var == "predict" | var == "marginsnotok" | var == "title" | var == "opt" | ///
		var == "N_cds"  | var == "cmd" | var == "chi2type" | var == "vcetype" | ///
		var == "vce" | var == "user" | var == "crittype" | var == "ml_method" | ///
		var == "singularHmethod" | var == "technique" | var == "which" | ///
		var == "depvar" | var == "properties" | var == "chi2" | var == "r2_p" | ///
		var == "ll_0" | var == "marginsok" | var == "df_r" | var == "rmse" | ///
		var == "mss" | var == "rss" | var == "model"

compress // variables have a str160 display format (como autojustificar)

replace var = subinstr(var,"_coef","",.)

replace var = "" if strpos(var,"_stderr")!=0

*replace var = "Pseudo R2" if var == "r2_p"

replace var = "Log likelihood " if var == "ll"

texsave using "StoT_logit.tex", title("Secondary to Tertiary: Logistic Probability Model") ///
		footnote("A */**/*** next to the coefficient indicates significance at the 10/5/1% level.") ///
		nonames autonumber hlines(-2) replace		
			
export excel using "/Users/saraynes.gs/Google Drive/Data_YL/Analyze/output/StoT_logit.xls", sheetmodify firstrow(variables)


use StoT_logit2, clear

drop if strpos(var,"_cons")!=0

drop if var == "rank" | var == "ic" | var == "k" | var == "k_eq" | var =="k_dv" | ///
		var == "converged" | var == "rc" | var == "k_autoCns" | var == "k_eq_model" | ///
		var == "rc" | var == "k_autoCns" | var == "k_eq_model" | var == "cmdline" | ///
		var == "df_m" |  var== "p" | var == "N_cdf" | var == "estat_cmd" | ///
		var == "predict" | var == "marginsnotok" | var == "title" | var == "opt" | ///
		var == "N_cds"  | var == "cmd" | var == "chi2type" | var == "vcetype" | ///
		var == "vce" | var == "user" | var == "crittype" | var == "ml_method" | ///
		var == "singularHmethod" | var == "technique" | var == "which" | ///
		var == "depvar" | var == "properties" | var == "chi2" | var == "r2_p" | ///
		var == "ll_0" | var == "marginsok" | var == "df_r" | var == "rmse" | ///
		var == "mss" | var == "rss" | var == "model"

compress // variables have a str160 display format (como autojustificar)

replace var = subinstr(var,"_coef","",.)

replace var = "" if strpos(var,"_stderr")!=0

*replace var = "Pseudo R2" if var == "r2_p"

replace var = "Log likelihood " if var == "ll"

texsave using "StoT_logit2.tex", title("Secondary to Tertiary: Logistic Probability Model. Location") ///
		footnote("A */**/*** next to the coefficient indicates significance at the 10/5/1% level.") ///
		nonames autonumber hlines(-2) replace		
			
export excel using "/Users/saraynes.gs/Google Drive/Data_YL/Analyze/output/StoT_logit2.xls", sheetmodify firstrow(variables)

	
/*-----------------------------------------------------------------------------*
							    Logit regression
							   Mediation analysis
------------------------------------------------------------------------------*/
use `StoT', clear

khb logit StoT c.parEduC2   || i.L2perf c.Cog_RMath, c(i.Female) vce(cluster clustid) or summary // disentangle when there is more than one mediator!		

khb logit StoT c.Wealth 	|| i.L2perf c.Cog_RMath, c(i.Female) vce(cluster clustid) or summary // disentangle

khb logit StoT i.NonSpanish || i.L2perf c.Cog_RMath, c(i.Female) vce(cluster clustid) or summary 

khb logit StoT i.Rural	    || i.L2perf c.Cog_RMath, c(i.Female) vce(cluster clustid) or summary 


/*-----------------------------------------------------------------------------*
							    Descriptive Statistics
------------------------------------------------------------------------------*/
use `StoT', clear

*** One-way tabulate

* Dichotomous
tabulate StoT if round==5 & yc==0  

tabulate pEd if round==5 & yc==0 

tabulate L2perf if round==5 & yc==0 

tabulate Rural if round==5 & yc==0 

tabulate NonSpanish if round==5 & yc==0 

tabulate Female if round==5 & yc==0 

tabulate entype if round==4 & yc==0 

* Continuous

*Ravens

summarize Cog_RMath if round==5 & yc==0

summarize Wealth if round==5 & yc==0  

summarize AgeGr1 if round==5 & yc==0 

summarize hh_Size if round==5 & yc==0  

summarize L1hwork if round==5 & yc==0 


tabulate StoT pEd if round==5 & yc==0  	

*** Two-way tabulation

#delimit ;
kdensity Wealth if StoT==0 , addplot(kdensity Wealth if StoT==1)
title("2nd Transition (22 years old)") legend(position(6) cols(2));
#delimit cr

#delimit ;
kdensity Wealth if NonSpanish==0 , addplot(kdensity Wealth if NonSpanish==1)
title("Wealth by ethnicity") legend(position(6) cols(2));
#delimit cr

tabulate StoT Rural if round==5 & yc==0  

tabulate StoT NonSpanish if round==5 & yc==0  
