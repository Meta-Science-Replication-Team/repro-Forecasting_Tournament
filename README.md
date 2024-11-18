# README

## How to reproduce our final report

1. Install R and RStudio
2. Clone the repository. 
If you don't have git installed on your device, or if you don't have experience with version control, you can download a zip file rather than close the repo (step 2).
3. Double click on `repro-Forecasting_Tournament.Rproj` to open the project in RStudio.
4. Knit `final_report\final_report.Rmd`. Alternatively, you can open `final_report\final_report.html` in the browser. 
The `html` file is the output we most recently knit.

## Default Branch

- [final_report](https://github.com/Meta-Science-Replication-Team/repro-Forecasting_Tournament)
	- Default branch that contains a cleaned up version of the repo
	- Updated all our code to ensure it calls to original data

### Folders

- `comp_repro_check`: files used to discuss the computational reproducibility check
- `final_report`: a folder that includes a markdown file of all of our results combined
- `jol_test`: include the markdown file and report to test if judgments of effect size changes show the same effects as judgments of learning or the Dunning-Kruger effect
- `original_files`: the original files (from the authors of the original paper) from the main branch - we did not clean these up
	- We did change a few of these files to work on reproducing the results
- `pre_reg_check`: files used to discuss the pre-registration
- `pre_reg_report`: files from our original pre-registration of reproducibility and re-data-analysis 
- `reproduce_ANOVA`: files used to investigate creating the original ANOVA result we wanted to reproduce
	
## Other Branches

- [main](https://github.com/Meta-Science-Replication-Team/repro-Forecasting_Tournament/tree/main)
	- Original copy of the main project (from the authors of the original paper: https://github.com/grossmania/Forecasting-Tournament/tree/main)
	- No edits from our team to preserve the work
- [test dynamic warping](https://github.com/Meta-Science-Replication-Team/repro-Forecasting_Tournament/commits/Test_dynamic_time_warping)
	- Original copy of the main project (from the authors of the original paper: https://github.com/grossmania/Forecasting-Tournament/tree/Test_dynamic_time_warping)
	- No edits from our team to preserve the work
	- Not used in our reproduction of this project
- [comp_repro_check](https://github.com/Meta-Science-Replication-Team/repro-Forecasting_Tournament/commits/comp_repro_check)
	- Our work on computational reproducibility
	- Includes check of the preregistration
	- Merged into final branch for report
- [ana-checks](https://github.com/Meta-Science-Replication-Team/repro-Forecasting_Tournament/tree/ana-checks)
	- Work by one team member to check reproducibility
	- Used to create report on if/where things could be reproduced
- [erin-jol-test](https://github.com/Meta-Science-Replication-Team/repro-Forecasting_Tournament/tree/erin-jol-test)
	- Work by one team member to examine judgments of learning comparison
	- Used to create report on data issues 
	- Merged into final_report branch

