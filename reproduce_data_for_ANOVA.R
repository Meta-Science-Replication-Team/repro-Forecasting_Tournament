library(tidyverse)
library(lme4)
library(car)
options(max.print = 20000, scipen = 1000)

original_dat <- read.csv("dat_for_analyses.csv", stringsAsFactors = FALSE)

original_phase1 <- original_dat %>% 
	filter(phase == 1) %>%
	select(MASE1_w1,
		 domain,
		 isExpert.factor,
		 ResponseId)

original_model.phase1.together <- lmer(log(MASE1_w1) ~ domain * isExpert.factor + (1 |	ResponseId), 
						   data = original_phase1)
car::Anova(original_model.phase1.together,
	     type = "III",
	     test.statistic = "F")
#> correct output:
# Analysis of Deviance Table (Type III Wald F tests with Kenward-Roger df)
# 
# Response: log(MASE1_w1)
# F Df Df.res               Pr(>F)    
# (Intercept)            118.3417  1 1645.8 < 0.0000000000000002 ***
# domain                  24.9028 11 1064.2 < 0.0000000000000002 ***
# isExpert.factor          0.8810  1 1747.0              0.34805    
# domain:isExpert.factor   1.9951 11 1304.0              0.02566 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# first, reproduce the df with onyl the vars needed for the ANOVA
# library(forecast)
# library(psych)
# library(tidyverse)
# library(irr)
# library(lme4)
# library(ggplot2)
# library(tidyr)
# library(emmeans)
# library(car)
# library(jtools)
# library(dplyr)
# library(ggsci)
# library(dplyr)
# library(Hmisc)

options(max.print = 20000, scipen = 1000)
# list of domains
domains <- c("lifesat", "posaffect", "negaffect", "ideoldem",  "ideolrep",  "polar", "iasian", "easian", "iafric", "eafric", "igend", "egend")
pct_change <- function(previous, new, as_decimal = FALSE) {
	x <- abs(((new - previous) / previous) * 100)
	if (as_decimal) x <- x / 100
	return(x)
}

# import 46 months of historical values up to October 2020
dat_hist <- read.csv("historical_data.csv", stringsAsFactors = FALSE)

# Create a list of trends for each domain, ordered in the same manner as domains variable up above
hist_trend <- list()

for (i in 1:length(domains)) {
	
	trend <- dat_hist[40, domains[i]] - dat_hist[1, domains[i]]
	hist_trend[[i]] <- trend
	
}
# filters by completion time, so that prolific sample excludes predictions that took less than 50 seconds to make
dat <- read.csv("Wave1+2data.csv", stringsAsFactors = FALSE)

# data set below does not filter lay sample by completion time
# dat <- read.csv("Wave1+2data_coded_2021-05-18.csv", stringsAsFactors = FALSE)

# list of notable columns and what they mean:
# * some columns are omitted because they will be removed / are redundant

# phase - value of 1 indicates submission was received during phase 1 (June 2020), value of 2 indicates submission was received during phase 2 (November 2020)

# isExpert - indicates whether submission is by an academic (1) or layperson (0)
dat$isExpert.factor <- factor(dat$isExpert, levels = c(0,1), labels = c("Prolific", "Academic"))

# revised - indicates whether a team submitted to both phase 1 & 2 of the tournament (i.e. submitted in June and then sent a revised submission in November)

# domain - indicates which domain the forecast is for. Shorthand is used here with the following terms referring to each domain:
# lifesat = Life Satisfaction
# posaffect = Positive Affect
# negaffect = Negative Affect
# ideoldem = Political Ideology - Democrat
# ideolrep = Political Ideology - Republican
# polar = Political Polarization
# iasian = Implicit Asian-American Bias
# easian = Explicit Asian-American Bias
# iafric = Implicit African-American Bias
# eafric = Explicit African-American Bias
# igend = Implicit Gender-Career Bias
# egend = Explicit Gender-Career Bias

# Month.1 - Month.18 columns list participant predictions for a given domain. 
#   All phase 1 (June) predictions range from Month.1 - Month.12
#   All phase 2 (November) predictions range from Month.7 - Month.18

# mean_error - output of forecast package's accuracy() function - displays the mean error (ME) of Month.1 - Month.6 predictions compared to the objective data
# root_mean_sqr_error - displays the root mean square error (RMSE) of Month.1 - Month.6 predictions compared to the objective data
# mean_abs_error - displays the root mean absolute error (MAE) of Month.1 - Month.6 predictions compared to the objective data
# mean_percent_error - displays the mean percent error (MPE) of Month.1 - Month.6 predictions compared to the objective data
# mean_abs_percent_error - displays the mean absolute percent error (MAPE) of Month.1 - Month.6 predictions compared to the objective data
# mean_abs_scaled_error_1 - MASE computed using custom computeMASE function
# mean_abs_scaled_error_2 - MASE computed using Metrics::mase function

# RMSE_cutoff - whether the prediction's RMSE is less than or greater than a naive forecast for the same time period
dat$RMSE_cutoff_Naive_linear.factor <- factor(dat$RMSE_cutoff_Naive_linear, levels = c(0, 1), labels = c("below cutoff", "above cutoff"))
dat$RMSE_cutoff_Naive_rwf.factor <- factor(dat$RMSE_cutoff_Naive_rwf, levels = c(0, 1), labels = c("below cutoff", "above cutoff"))

# confidence - indicates on scale of 1-7 how confident participants were in their predictions
# subexpert - indicates on scale of 1-7 the participant's self-reported expertise in the domain they are predicting
# pub - the number of publications the team has made on the predicted domain

# model, theory, parameters - all contain participant written responses regarding what model they used, what theory they relied on, and what conditionals they considered

# numpred - number of conditionals (beyond the domain predicted) that participants considered in their prediction
# covidcondyn - whether covid-19 was considered as a conditional in their forecast
# datatrain - whether participants used the forecast data that was provided to them
# counterfact & othercounter - written response indicating the counterfactual participants considered
# counter_imp & othercountim - how important they consider their counterfactual to be

# Method - indicates forecasting method used to generate forecast - either Intuition/Theory, Data-Driven, Mixed, Simulation, or Objective - latter category is used to indicate the objective data for each domain for Months 1-6
# Method.coded - 1 - Intuition / 2 - Theory / 3 - Data-driven / 4 - Mixed / 5 - Objective / 6 - Naive - linear / 7 - Naive - rwf

dat$Method.code[dat$Method.coded==1]<-"Intuition/Theory"
dat$Method.code[dat$Method.coded==2]<-"Intuition/Theory"
dat$Method.code[dat$Method.coded==3]<-"Data-Driven"
dat$Method.code[dat$Method.coded==4]<-"Hybrid"
dat$Method.code[dat$Method.coded==5]<-"Ground Truth"
dat$Method.code[dat$Method.coded==6]<-"Naive-linear"
dat$Method.code[dat$Method.coded==7]<-"Naive-rfw"
dat$Method.code[dat$isExpert==0]<-"Lay People"


# Method.complex - ONLY PHASE 1, coded 1-3 scale indicating whether the Data-driven or Mixed method used is simple (e.g., regression to the mean), moderate (e.g., auto-regression w time lag, univariate time series), or complex (e.g., ARIMA, dynamic econometric model)

dat$Method.complex.factor <- factor(dat$Method.complex, levels = c(1:3), labels = c("simple", "moderate", "complex"))

# team_size.coded - self-reported measure indicating number of team-members in the team
# team_expertise - written response of team's general expertise

# FOLLOWING VARIABLES ARE EXCLUSIVE TO LAY SAMPLE because it consists entirely of individuals whereas academic sample consists of teams
# Age (num)
# Sex (1 = Male, 2 = Female, 3 = Prefer not to say)
dat$Sex.factor <- factor(dat$Sex, levels = c(1:3), labels = c("Male", "Female", "Prefer not to say")) 

# Genderident (1 = trans/woman, 2= trans/man, 3= genderqueer, 4 = Prefer not to say, 5 = other)
dat$Genderident.factor <- factor(dat$Genderident, levels = c(1:5), labels = c("trans/woman", "trans/man", "genderqueer", "Prefer not to say", "other"))

# education (1-8 = less than highschool, high school, some college, Vocation or technical school, Bachelor's, Master's, Doctorate, Professional degree)
dat$education.factor <- factor(dat$Education, levels = c(1:8), labels = c("less than highschool", "high school", "some college", "Vocation or technical school", "Bachelor's", "Master's", "Doctorate", "Professional degree"))
# occupation (written response)

# Ethnicity
dat$Ethnicity.factor <- factor(dat$Ethnicity, levels = c(1:9), labels = c("Aboriginal/Native", "Asian", "Black", "White", "Middle Eastern", "Hispanic", "East Indian", "Mixed Race", "Other/Not Listed"))

# Religion
dat$Religion.factor <- factor(dat$Religion, levels = c(1:10), labels = c("Buddhist", "Christian - Catholic", "Christian - Protestant", "Christian - Other", "Hindu", "Jewish", "Muslim", "Sikh", "Other", "Non-Religious"))

# Income
dat$Income.factor <- factor(dat$Income, levels = c(1:8), labels = c("Under $15,000", "$15,001 - $25,000", "$25,001 - $35,000", "$35,001 - $50,000", "$50,001 - $75,000", "$75,001 - $100,000", "$100,001 - $150,000", "Over $150,000"))

# Residential Area
dat$Residential.Area.factor <- factor(dat$Residential.Area, levels = c(1:3), labels = c("Urban", "Suburban", "Rural"))

# get factor scores for team discipline coded
dat$discipline[dat$team_discipline.coded==1]<-"Behavioral Sciences"
dat$discipline[dat$team_discipline.coded==2]<-"Social Sciences"
dat$discipline[dat$team_discipline.coded==3]<-"Data/Computer Science"
dat$discipline[dat$team_discipline.coded==4]<-"Multi-disciplinary"
dat$discipline[dat$team_discipline.coded==5]<-"Other"


# Whether the team is multi-disciplinary (1) or mono (0)
dat$multi_dis.factor <- ifelse(dat$team_discipline.coded==4,"Multi domain expertise", "Single domain expertise")
#IMPORTANT: one team - Spartacus - does not have any demographics, and hene NA for discipline!

length(colnames(original_dat))
length(colnames(dat))

setdiff(colnames(original_dat),
	  colnames(dat))
setdiff(colnames(dat),
	  colnames(original_dat))
# ok, so I cannot get all the columns that were in the original dataset. weird
# can I get the ones needed for the ANOVA?

phase1 <- dat %>% 
	filter(phase == 1) %>%
	select(MASE1_w1,
		 domain,
		 isExpert.factor,
		 ResponseId)

model.phase1.together <- lmer(log(MASE1_w1) ~ domain * isExpert.factor + (1 |	ResponseId), 
					data = phase1)
car::Anova(model.phase1.together,
	     type = "III",
	     test.statistic = "F")
#> correct output:
# Analysis of Deviance Table (Type III Wald F tests with Kenward-Roger df)
# 
# Response: log(MASE1_w1)
# F Df Df.res               Pr(>F)    
# (Intercept)            118.3417  1 1645.8 < 0.0000000000000002 ***
# domain                  24.9028 11 1064.2 < 0.0000000000000002 ***
# isExpert.factor          0.8810  1 1747.0              0.34805    
# domain:isExpert.factor   1.9951 11 1304.0              0.02566 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# I can reproduce the 1.99 for the interaction, but the interecept seems to be different
# check for differences between the two phase1 dfs
phase1 %>%
	select(ResponseId,
		 domain,
		 isExpert.factor,
		 MASE1_w1) %>%
	distinct() %>%
	nrow()
original_phase1 %>%
	select(ResponseId) %>%
	distinct() %>%
	nrow()
df_test <- original_phase1 %>%
	left_join(phase1 %>%
		    	mutate(is_same = "same"),
		    by = c("ResponseId",
		    	 "domain",
		    	 "isExpert.factor",
		    	 "MASE1_w1"))
levels(phase1$isExpert.factor)
phase1 <- phase1 %>%
	mutate(isExpert.factor = as.character(isExpert.factor))
df_test <- original_phase1 %>%
	rename(original_MASE1_w1 = MASE1_w1) %>%
	left_join(phase1,
		    by = c("ResponseId",
		    	 "domain",
		    	 "isExpert.factor"))
# there are some weird NAs
original_phase1 %>%
	summarise(sum(is.na(ResponseId)),
		    sum(is.na(domain)),
		    sum(is.na(isExpert.factor)),
		    sum(is.na(MASE1_w1)))
phase1 %>%
	summarise(sum(is.na(ResponseId)),
		    sum(is.na(domain)),
		    sum(is.na(isExpert.factor)),
		    sum(is.na(MASE1_w1)))
# same number, but same values?
original_phase1 <- original_phase1 %>%
	select(ResponseId,
		 domain,
		 isExpert.factor,
		 MASE1_w1)
phase1 <- phase1 %>%
	select(ResponseId,
		 domain,
		 isExpert.factor,
		 MASE1_w1)

df_test <- original_phase1 %>%
	rename(original_MASE1_w1 = MASE1_w1) %>%
	filter(!is.na(ResponseId),
		 !is.na(domain),
		 !is.na(isExpert.factor),
		 !is.na(original_MASE1_w1)) %>%
	inner_join(phase1 %>%
		     	filter(!is.na(ResponseId),
		     		 !is.na(domain),
		     		 !is.na(isExpert.factor),
		     		 !is.na(MASE1_w1)),
		     by = c("ResponseId",
		     	 "domain",
		     	 "isExpert.factor"))
df_test <- df_test %>%
	mutate(dif_MASE = original_MASE1_w1 - MASE1_w1)
df_test %>%
	summarise(n(),
		    min(dif_MASE),
		    max(dif_MASE))
# very small differences, looks like due to rounding
# now check the NAs
original_phase1 %>%
	filter(is.na(ResponseId) |
		 	is.na(domain) |
		 	is.na(isExpert.factor) |
		 	is.na(MASE1_w1))
phase1 %>%
	filter(is.na(ResponseId) |
		 	is.na(domain) |
		 	is.na(isExpert.factor) |
		 	is.na(MASE1_w1))
# 36 for both
original_phase1 %>%
	filter(is.na(ResponseId),
		 is.na(isExpert.factor),
		 is.na(MASE1_w1)) %>%
	inner_join(phase1 %>%
		    	filter(is.na(ResponseId),
		    		 is.na(isExpert.factor),
		    		 is.na(MASE1_w1)))
original_phase1 %>%
	filter(is.na(ResponseId),
		 is.na(isExpert.factor),
		 is.na(MASE1_w1)) %>%
	nrow()
phase1 %>%
	filter(is.na(ResponseId),
		 is.na(isExpert.factor),
		 !is.na(MASE1_w1)) %>%
	nrow()
# next 24
original_phase1 %>%
	filter(is.na(ResponseId),
		 is.na(isExpert.factor),
		 !is.na(MASE1_w1)) %>%
	nrow()
original_phase1 %>%
	filter(is.na(ResponseId),
		 is.na(isExpert.factor),
		 !is.na(MASE1_w1)) %>%
	select(domain,
		 MASE1_w1) %>%
	inner_join(phase1 %>%
		     	filter(is.na(ResponseId),
		     		 is.na(isExpert.factor),
		     		 !is.na(MASE1_w1)) %>%
		    	select(domain,
		    		 MASE1_w1),
		     by = c("domain",
		     	 "MASE1_w1"))
original_phase1 %>%
	filter(is.na(ResponseId),
		 is.na(isExpert.factor),
		 !is.na(MASE1_w1)) 
phase1 %>%
	filter(is.na(ResponseId),
		 is.na(isExpert.factor),
		 !is.na(MASE1_w1)) %>%
	View()
# small differences probably due to rounding
# now, where is the MASE1_w1 comming from? I need to know to figure out where the 
# raw data is


