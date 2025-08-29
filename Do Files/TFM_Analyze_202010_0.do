********************************************************************************
*					PERU YOUNG LIVES PANEL DATA								   *
*					Rounds 1 to 5, OC       								   *
********************************************************************************

	* Author: 		Sara Gonzales
	* Date: 		22 Oct 2020
	* Last Update: 	22 Oct 2020
	* Last run: 	22 Oct 2020
	
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

global wdir 		"/Users/saraynes.gs/Google Drive/Data_YL/Analyze/output"
global dpanel 	 	"/Users/saraynes.gs/Google Drive/Data_YL/Analyze/input"
global programs 	"/Users/saraynes.gs/Google Drive/Data_YL/Analyze/programs"

cd "$wdir"

/*
 ssc install estout \\(creating tables of summary statistics)
 ssc install tabout 
 net install http://www.stata-journal.com/software/sj18-3/gr0073/
 ssc install grstyle, replace
 ssc install palettes, replace
 ssc install blindschemes, replace all

 set scheme plotplainblind, permanently
 h plotplainblind */
 
* Bischof, D. 2017. “New graphic schemes for stata: Plotplain and plottig.” Stata Journal 17 (3).

/* 	Merge the "constructed data file" and data from each round. The final dataset
	is stored in  /Users/saraynes.gs/Google Drive/Data_YL/Build/programs/output/TFM_peru_full.dta */

* run "/Users/saraynes.gs/Google Drive/Data_YL/Build/programs/TFM_Build_202003.do" 

/* Transform most of the variables used (I didn't change it) */
*run "$programs/TFM_Analyze_202003_0.do"

* Regressions 1st Transition: Primary to Secondry - Both Cohorts
* Also has marginal effects

run "$programs/TFM_Analyze_202010_1.do"

* Regressions 2nd Transition: Secondary to Tertiary - Older Cohorts
* Also has marginal effects

run "$programs/TFM_Analyze_202010_2.do"


