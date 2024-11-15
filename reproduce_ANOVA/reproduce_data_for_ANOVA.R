library(tidyverse)
library(lme4)
library(car)
options(max.print = 20000, scipen = 1000)

original_dat <- read.csv("original_files/dat_for_analyses.csv", stringsAsFactors = FALSE)

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

processed_dat <- read.csv("Wave1+2data.csv", stringsAsFactors = FALSE)
# isExpert - indicates whether submission is by an academic (1) or layperson (0)
processed_dat$isExpert.factor <- factor(processed_dat$isExpert, levels = c(0,1), labels = c("Prolific", "Academic"))

processed_phase1 <- processed_dat %>% 
	filter(phase == 1) %>%
	select(MASE1_w1,
		 domain,
		 isExpert.factor,
		 ResponseId)

processed_model.phase1.together <- lmer(log(MASE1_w1) ~ domain * isExpert.factor + (1 |	ResponseId), 
					data = processed_phase1)
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
