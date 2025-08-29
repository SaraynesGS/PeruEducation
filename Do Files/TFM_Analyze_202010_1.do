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

	* Regressions Both Cohorts - 1st Transition: Primary to Secondry
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
	rename scoreST_rmath3	Cog_RMath
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
	gen PtoS=.
	replace PtoS = PtoS3 if ( yc==0 & round==3)
	replace PtoS = PtoS5 if ( yc==1 & round==5)

	label var PtoS "Transitions from Primary to Secondary"
	label define PtoS 0 "Not Continue" 1 "Continue"
	label value PtoS PtoS

	* Parents' education
	gen pEd=0
	replace pEd=1 if P_Primary==1
	replace pEd=2 if P_HighSchool==1 | P_College==1

	* gen P_Aspirations = p_aspiration_ed * p_expectation_ed

	gen LocPS=Loc3
	replace LocPS=0 if Loc5==0 & yc==1 & round==5 // recoding the variables in round5
	replace LocPS=2 if Loc5==2 & yc==1 & round==5 // recoding the variables in round5
	
	recode entype (1 3 4 = 0) (2 = 1) , gen(School) 

	* Combining L1.Perf (Older cohort) and L2.Perf (Younger cohort) into 1 variable

	gen LPerf=.
	replace LPerf = L1perf if ( yc==0 & round==3)
	replace LPerf = L2perf if ( yc==1 & round==5)

	*recode NonSpanish (1 = 0) (0 =1)
					 
	*gen perf_2 = (L1perf==0) // Good
	*gen perf_0 = (L1perf==1) // Average
	*gen perf_1 = (L1perf==2) // Bad 

	/*-----------------------------------------------------------------------------*
								Sample selection
	------------------------------------------------------------------------------*/

	* The regression for the 1st transition
	keep if (yc==1 & round==5) | (yc==0 & round==3)

	/*-----------------------------------------------------------------------------*
								Some descriptives!
	------------------------------------------------------------------------------*/

	* Descriptive tabulation od subsample
	tab yc round

	tab PtoS

	tab PtoS yc
	disp 102/714  // OC = 0.14285714
	disp 298/2052 // YC = 0.14522417

	tab yc AgeGr1
	disp (60+17+6+1)/674  // OC more than 6 years old at 1st grade = 0.12462908
	disp (46+7+2)/1926    // YC more than 6 years old at 1st grade = 0.02855659

	tab NonSpanish Rural 
	disp 1893/2038 // Spanish and Urban 0.92885182 %
	
	replace clustid=3 if clustid==0
	replace clustid=13 if clustid==88
	
	tempfile PtoS
	save `PtoS'

	/* =========================================================================== *
									 REGRESSIONS
	 * =========================================================================== */
	 
	 use `PtoS', clear
	 global Controls c.L1hwork i.L1sch_public c.hh_Size c.AgeGr1
	 
	/*-----------------------------------------------------------------------------*
				             Linear Probability Regressiones
	------------------------------------------------------------------------------*/

	reg PtoS ib1.pEd i.yc i.Female ib1.pEd##i.yc, vce(cluster clustid)
	regsave using PtoS_LPM, detail(all) table(reg1, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) replace

	reg PtoS ib1.pEd i.yc i.Female i.LPerf c.Cog_RMath ib1.pEd##i.yc, vce(cluster clustid)
	regsave using PtoS_LPM, detail(all) table(reg2, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append			
			
	reg PtoS ib1.pEd i.yc i.Female i.LPerf c.Cog_RMath ib1.pEd##i.yc $Controls , vce(cluster clustid)
	regsave using PtoS_LPM, detail(all) table(reg3, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append		
			
	reg PtoS c.Wealth i.yc i.Female c.Wealth##i.yc, vce(cluster clustid)
	regsave using PtoS_LPM, detail(all) table(reg4, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append

	reg PtoS c.Wealth i.yc i.Female i.LPerf c.Cog_RMath c.Wealth##i.yc, vce(cluster clustid)
	regsave using PtoS_LPM, detail(all) table(reg5, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append

	reg PtoS c.Wealth i.yc i.Female i.LPerf c.Cog_RMath c.Wealth##i.yc $Controls, vce(cluster clustid)
	regsave using PtoS_LPM, detail(all) table(reg6, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append
			
	reg PtoS i.NonSpanish i.yc i.Female i.NonSpanish##i.yc, vce(cluster clustid)
	regsave using PtoS_LPM, detail(all) table(reg7, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append

	reg PtoS i.NonSpanish i.yc i.Female i.LPerf c.Cog_RMath i.NonSpanish##i.yc, vce(cluster clustid)
	regsave using PtoS_LPM, detail(all) table(reg8, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append

	reg PtoS i.NonSpanish i.yc i.Female i.LPerf c.Cog_RMath i.NonSpanish##i.yc $Controls, vce(cluster clustid)
	regsave using PtoS_LPM, detail(all) table(reg9, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append			
			
	reg PtoS i.Rural i.yc i.Female i.Rural##i.yc, vce(cluster clustid)
	regsave using PtoS_LPM, detail(all) table(reg10, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append

	reg PtoS i.Rural i.yc i.Female i.LPerf c.Cog_RMath i.Rural##i.yc, vce(cluster clustid)
	regsave using PtoS_LPM, detail(all) table(reg11, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append

	reg PtoS i.Rural i.yc i.Female i.LPerf c.Cog_RMath i.Rural##i.yc $Controls, vce(cluster clustid)
	regsave using PtoS_LPM, detail(all) table(reg12, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append	
			
	reg PtoS ib1.pEd c.Wealth i.Rural i.yc i.Female ib1.pEd##i.yc, vce(cluster clustid)
	regsave using PtoS_LPM, detail(all) table(reg13, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append

	reg PtoS ib1.pEd c.Wealth i.Rural i.yc i.Female i.LPerf c.Cog_RMath ib1.pEd##i.yc, vce(cluster clustid)
	regsave using PtoS_LPM, detail(all) table(reg14, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append

	reg PtoS ib1.pEd c.Wealth i.NonSpanish i.NonSpanish i.yc i.Female i.LPerf c.Cog_RMath $Controls ib1.pEd##i.yc, vce(cluster clustid)
	regsave using PtoS_LPM, detail(all) table(reg15, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append				

	reg PtoS ib2.LocPS i.yc i.Female i.LocPS##i.yc, vce(cluster clustid)
	regsave using PtoS_LPM2, detail(all) table(reg10a, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) replace

	reg PtoS ib2.LocPS i.yc i.Female i.LPerf c.Cog_RMath i.LocPS##i.yc, vce(cluster clustid)
	regsave using PtoS_LPM2, detail(all) table(reg11a, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append

	reg PtoS ib2.LocPS i.yc i.Female i.LPerf c.Cog_RMath i.LocPS##i.yc $Controls, vce(cluster clustid)
	regsave using PtoS_LPM2, detail(all) table(reg12a, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append	
			
			
	use PtoS_LPM, clear

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

	texsave using "Tab_PtoS_LPM.tex", title("Primary to Secondary: Linear Probability Model") ///
			footnote("A */**/*** next to the coefficient indicates significance at the 10/5/1% level.") ///
			nonames autonumber hlines(-2) replace		
			
	export excel using "/Users/saraynes.gs/Google Drive/Data_YL/Analyze/output/Tab_PtoS_LPM.xls", sheetreplace firstrow(variables)

	
	use PtoS_LPM2, clear

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

	texsave using "Tab_PtoS_LPM2.tex", title("Primary to Secondary: Linear Probability Model") ///
			footnote("A */**/*** next to the coefficient indicates significance at the 10/5/1% level.") ///
			nonames autonumber hlines(-2) replace		
			
	export excel using "/Users/saraynes.gs/Google Drive/Data_YL/Analyze/output/Tab_PtoS_LPM2.xls", sheetreplace firstrow(variables)
	
	
/*-----------------------------------------------------------------------------*
							Linear Probability Regressiones
							     Mediation Analysis
------------------------------------------------------------------------------*/


/*--------------------------- FULL SAMPLE: OC and YC -------------------------*/		

use `PtoS', clear

khb reg PtoS c.parEduC2     	|| i.LPerf c.Cog_RMath, c(i.yc i.Female)  vce(cluster clustid) summary // ib1.pEd for dichotomous

khb reg PtoS c.Wealth 		|| i.LPerf c.Cog_RMath, c(i.yc i.Female)  vce(cluster clustid) summary // summary		

khb reg PtoS i.NonSpanish	|| i.LPerf c.Cog_RMath, c(i.yc i.Female)  vce(cluster clustid) summary 

khb reg PtoS i.Rural		|| i.LPerf c.Cog_RMath, c(i.yc i.Female)  vce(cluster clustid) summary 

khb reg PtoS ib1.pEd c.Wealth i.Rural || i.LPerf c.Cog_RMath, c(i.yc i.Female)  vce(cluster clustid) summary // disentangle when there is more than one mediator!		

/*--------------------------------- OLDER COHORT -----------------------------*/		

use `PtoS', clear
keep if yc==0

khb reg PtoS c.parEduC2     	|| i.LPerf c.Cog_RMath, c(i.Female)  vce(cluster clustid) summary // disentangle when there is more than one mediator!		

khb reg PtoS c.Wealth 		|| i.LPerf c.Cog_RMath, c(i.Female)  vce(cluster clustid) summary // summary		

khb reg PtoS i.NonSpanish	|| i.LPerf c.Cog_RMath, c(i.Female)  vce(cluster clustid) summary 

khb reg PtoS i.Rural		|| i.LPerf c.Cog_RMath, c(i.Female)  vce(cluster clustid) summary 

/*------------------------------- YOUNGER COHORT -----------------------------*/

use `PtoS', clear
keep if yc==1

khb reg PtoS c.parEduC2    	|| i.LPerf c.Cog_RMath, c(i.Female)  vce(cluster clustid) summary // disentangle when there is more than one mediator!		

khb reg PtoS c.Wealth 		|| i.LPerf c.Cog_RMath, c(i.Female)  vce(cluster clustid) summary // summary		

khb reg PtoS i.NonSpanish	|| i.LPerf c.Cog_RMath, c(i.Female)  vce(cluster clustid) summary 

khb reg PtoS i.Rural		|| i.LPerf c.Cog_RMath, c(i.Female)  vce(cluster clustid) summary 

khb reg PtoS c.parEduC2  c.Wealth i.NonSpanish i.Rural || i.LPerf c.Cog_RMath, c(i.Female)  vce(cluster region) summary // disentangle when there is more than one mediator!		

		
/*-----------------------------------------------------------------------------*
							     Logit Regressiones
------------------------------------------------------------------------------*/
	use `PtoS', clear

	logit PtoS ib1.pEd i.yc i.Female ib1.pEd##i.yc, vce(cluster clustid)
	regsave using PtoS_logit, detail(all) table(reg1, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) replace

	logit PtoS ib1.pEd i.yc i.Female i.LPerf c.Cog_RMath ib1.pEd##i.yc, vce(cluster clustid)
	regsave using PtoS_logit, detail(all) table(reg2, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append			
			
	logit PtoS ib1.pEd i.yc i.Female i.LPerf c.Cog_RMath ib1.pEd##i.yc $Controls , vce(cluster clustid)
	regsave using PtoS_logit, detail(all) table(reg3, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append		
			
	logit PtoS c.Wealth i.yc i.Female c.Wealth##i.yc, vce(cluster clustid)
	regsave using PtoS_logit, detail(all) table(reg4, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append

	logit PtoS c.Wealth i.yc i.Female i.LPerf c.Cog_RMath c.Wealth##i.yc, vce(cluster clustid)
	regsave using PtoS_logit, detail(all) table(reg5, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append

	logit PtoS c.Wealth i.yc i.Female i.LPerf c.Cog_RMath c.Wealth##i.yc $Controls, vce(cluster clustid)
	regsave using PtoS_logit, detail(all) table(reg6, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append
			
	logit PtoS i.NonSpanish i.yc i.Female i.NonSpanish##i.yc, vce(cluster clustid)
	regsave using PtoS_logit, detail(all) table(reg7, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append

	logit PtoS i.NonSpanish i.yc i.Female i.LPerf c.Cog_RMath i.NonSpanish##i.yc, vce(cluster clustid)
	regsave using PtoS_logit, detail(all) table(reg8, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append

	logit PtoS i.NonSpanish i.yc i.Female i.LPerf c.Cog_RMath i.NonSpanish##i.yc $Controls, vce(cluster clustid)
	regsave using PtoS_logit, detail(all) table(reg9, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append			
			
	logit PtoS i.Rural i.yc i.Female i.Rural##i.yc, vce(cluster clustid)
	regsave using PtoS_logit, detail(all) table(reg10, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append

	logit PtoS i.Rural i.yc i.Female i.LPerf c.Cog_RMath i.Rural##i.yc, vce(cluster clustid)
	regsave using PtoS_logit, detail(all) table(reg11, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append

	logit PtoS i.Rural i.yc i.Female i.LPerf c.Cog_RMath i.Rural##i.yc $Controls, vce(cluster clustid)
	regsave using PtoS_logit, detail(all) table(reg12, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append	
			
	logit PtoS ib1.pEd c.Wealth i.Rural i.yc i.Female ib1.pEd##i.yc, vce(cluster clustid)
	regsave using PtoS_logit, detail(all) table(reg13, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append

	logit PtoS ib1.pEd c.Wealth i.Rural i.yc i.Female i.LPerf c.Cog_RMath ib1.pEd##i.yc, vce(cluster clustid)
	regsave using PtoS_logit, detail(all) table(reg14, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append

	logit PtoS ib1.pEd c.Wealth i.Rural i.yc i.Female i.LPerf c.Cog_RMath $Controls ib1.pEd##i.yc, vce(cluster clustid)
	regsave using PtoS_logit, detail(all) table(reg15, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append				

	logit PtoS ib2.LocPS i.yc i.Female i.LocPS##i.yc, vce(cluster clustid)
	regsave using PtoS_logit2, detail(all) table(reg10a, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) replace

	logit PtoS ib2.LocPS i.yc i.Female i.LPerf c.Cog_RMath i.LocPS##i.yc, vce(cluster clustid)
	regsave using PtoS_logit2, detail(all) table(reg11a, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append

	logit PtoS ib2.LocPS i.yc i.Female i.LPerf c.Cog_RMath i.LocPS##i.yc $Controls, vce(cluster clustid)
	regsave using PtoS_logit2, detail(all) table(reg12a, format(%5.2f) parentheses(stderr) ///
			asterisk(10 5 1) order(N ll regvars)) append
			
				
			
	use PtoS_logit, clear

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

	texsave using "Tab_PtoS_logit.tex", title("Primary to Secondary: Logit regression (Parent's education, wealth x YC)") ///
			footnote("A */**/*** next to the coefficient indicates significance at the 10/5/1% level.") ///
			nonames autonumber hlines(-2) replace		
			
	export excel using "/Users/saraynes.gs/Google Drive/Data_YL/Analyze/output/Tab_PtoS_logit.xls", sheetreplace firstrow(variables)

 
 	use PtoS_logit2, clear

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

	texsave using "Tab_PtoS_logit2.tex", title("Primary to Secondary: Logit regression (Location)") ///
			footnote("A */**/*** next to the coefficient indicates significance at the 10/5/1% level.") ///
			nonames autonumber hlines(-2) replace		
			
	export excel using "/Users/saraynes.gs/Google Drive/Data_YL/Analyze/output/Tab_PtoS_logit2.xls", sheetreplace firstrow(variables)

 
/*-----------------------------------------------------------------------------*
							     Logit Regressiones
							     Mediation Analysis
------------------------------------------------------------------------------*/


/*--------------------------- FULL SAMPLE: OC and YC -------------------------*/		

use `PtoS', clear

khb logit PtoS c.parEduC2 	|| i.LPerf c.Cog_RMath, c(i.yc i.Female) or vce(cluster clustid) summary

khb logit PtoS c.Wealth 	|| i.LPerf c.Cog_RMath, c(i.yc i.Female) or vce(cluster clustid) summary		

khb logit PtoS i.NonSpanish	|| i.LPerf c.Cog_RMath, c(i.yc i.Female) or vce(cluster clustid) summary		

khb logit PtoS i.Rural		|| i.LPerf c.Cog_RMath, c(i.yc i.Female) or vce(cluster clustid) summary		

khb logit PtoS c.parEduC2 c.Wealth i.NonSpanish i.Rural || i.LPerf c.Cog_RMath, c(i.yc i.Female) or vce(cluster clustid) summary // disentangle when there is more than one mediator!		
	

/*--------------------------------- OLDER COHORT -----------------------------*/		

use `PtoS', clear
keep if yc==0

khb logit PtoS c.parEduC2 	|| i.LPerf c.Cog_RMath, c(i.Female) or vce(cluster clustid) summary // disentangle // disentangle when there is more than one mediator!		

khb logit PtoS c.Wealth 	|| i.LPerf c.Cog_RMath, c(i.Female) or vce(cluster clustid) disentangle // summary		

khb logit PtoS i.NonSpanish	|| i.LPerf c.Cog_RMath, c(i.Female) or vce(cluster clustid) disentangle 

khb logit PtoS i.Rural		|| i.LPerf c.Cog_RMath, c(i.Female) or vce(cluster clustid) disentangle 

khb logit PtoS ib1.pEd c.Wealth i.NonSpanish i.Rural || i.LPerf c.Cog_RMath, c(i.Female) or vce(cluster clustid) summary // disentangle when there is more than one mediator!		


/*------------------------------- YOUNGER COHORT -----------------------------*/

use `PtoS', clear
keep if yc==1

khb logit PtoS c.parEduC2 	|| i.LPerf c.Cog_RMath, c(i.Female) or vce(cluster clustid) summary

khb logit PtoS c.Wealth 	|| i.LPerf c.Cog_RMath, c(i.Female) or vce(cluster clustid) summary // summary		

khb logit PtoS i.NonSpanish	|| i.LPerf c.Cog_RMath, c(i.Female) or vce(cluster clustid) summary 

khb logit PtoS i.Rural		|| i.LPerf c.Cog_RMath, c(i.Female) or vce(cluster clustid) summary

/*-----------------------------------------------------------------------------*
							    Descriptive Statistics
------------------------------------------------------------------------------*/
use `PtoS', clear

*** One-way tabulate

* Dichotomous
tabulate PtoS if round==3 & yc==0  
tabulate PtoS if round==5 & yc==1

tabulate pEd if round==3 & yc==0 
tabulate pEd if round==5 & yc==1

tabulate LPerf if round==3 & yc==0 
tabulate LPerf if round==5 & yc==1

tabulate Rural if round==3 & yc==0 
tabulate Rural if round==5 & yc==1

tabulate NonSpanish if round==3 & yc==0 
tabulate NonSpanish if round==5 & yc==1

tabulate Female if round==3 & yc==0 
tabulate Female if round==5 & yc==1

tabulate L1sch_public if round==3 & yc==0 
tabulate L1sch_public if round==5 & yc==1

* Continuous
summarize Cog_Ravens if round==3 & yc==0 
summarize Cog_CDA 	 if round==2 & yc==1 

summarize Cog_RMath if round==1 & yc==0
summarize Cog_RMath if round==5 & yc==1

summarize Wealth if round==3 & yc==0 
summarize Wealth if round==5 & yc==1 

summarize AgeGr1 if round==3 & yc==0 
summarize AgeGr1 if round==5 & yc==1 

summarize hh_Size if round==3 & yc==0 
summarize hh_Size if round==5 & yc==1 

summarize L1hwork if round==3 & yc==0 
summarize L1hwork if round==5 & yc==1 

*** Two-way tabulation

tabulate PtoS Rural if round==3 & yc==0  
tabulate PtoS Rural if round==5 & yc==1

tabulate PtoS NonSpanish if round==3 & yc==0  
tabulate PtoS NonSpanish if round==5 & yc==1

#delimit ;
kdensity Wealth if PtoS==0 & round==3 & yc==0 , 
addplot(kdensity Wealth if PtoS==1 & round==3 & yc==0 || 
kdensity Wealth if PtoS==1 & round==5 & yc==1 || 
kdensity Wealth if PtoS==0 & round==5 & yc==1 ) 
title("1st Transition (15 years old)") legend(position(6) cols(4));
#delimit cr

* kdensity Wealth if PtoS==0 & round==3 & yc==0, title("Not Continue - Older Cohort") 
* kdensity Wealth if PtoS==1 & round==3 & yc==0, title("Continue - Older Cohort")  
* kdensity Wealth if PtoS==0 & round==5 & yc==1, title("Not Continue - Younger Cohort") 
* kdensity Wealth if PtoS==1 & round==5 & yc==1, title("Continue - Younger Cohort") 

#delimit ;
kdensity Wealth if NonSpanish==0 & round==3 & yc==0 , 
addplot(kdensity Wealth if NonSpanish==1 & round==3 & yc==0 || 
kdensity Wealth if NonSpanish==1 & round==5 & yc==1 || 
kdensity Wealth if NonSpanish==0 & round==5 & yc==1 ) 
title("Wealth by ethnicity") legend(position(6) cols(4));
#delimit cr


