#> the Rmd file with matching name generates a few csv files that are saved in
#> the repo 
#> After making changes to the Rmd file, I could knit it without error
#> summary of changes needed to knit the file: 
#> 		ensure unique names for chuncks. quite a few duplicates (marked with bis) 
#> 		remove extra ```
#>		reformat code such that chunks are valid
#>		use here::here and relative paths instead of hard coded paths 
#>			that work only on Igor's device
#> it is unclear why Rmd was used since all test was written as comments in
#> code chunks, not as text in between them

#> after knitting the Rmd, git detects that the csv files are modified
#> this is strange, so I need to check what values have changed between the 
#> two versions
#> team_size.csv was not changed

library("tidyverse")

#> step 1: revert changes to the csv files:
#> Wave1+2data.csv
#> Wave1+2demographics.csv

#> step2: read in the original files
original_data1 <- readr::read_csv(here::here("Wave1+2data.csv"))
original_data2 <- readr::read_csv(here::here("Wave1+2demographics.csv"))

#> step3: knit the rmd
#> step 4: read in the modified files
current_data1 <- readr::read_csv(here::here("Wave1+2data.csv"))
current_data2 <- readr::read_csv(here::here("Wave1+2demographics.csv"))

# Wave1+2data ----
# same number of columns?
length(original_data1)
length(current_data1)
# yes
# same columns?
setdiff(colnames(original_data1),
		colnames(current_data1))
setdiff(colnames(current_data1),
		colnames(original_data1))
# yes
# same values?
original_data1 %>%
	anti_join(current_data1) %>%
	nrow()
# not same values
original_data1 %>%
	inner_join(current_data1) %>%
	nrow()
# 24 rows have the same values, but the rest of 2241 are different
original_data1 %>%
	select(all_of(colnames(original_data1)[1:6])) %>%
	anti_join(current_data1) %>%
	nrow()
# so same values for:
colnames(original_data1)[1:6]
# differences start to apepar with Month.1 ....
original_data1 %>%
	select(all_of(colnames(original_data1)[1:7])) %>%
	left_join(current_data1 %>%
			  	select(all_of(colnames(original_data1)[1:7])),
			  by = c("...1",
			  	   "Duration..in.seconds.",
			  	   "ResponseId",
			  	   "phase",
			  	   "team_name",
			  	   "domain")) %>%
	mutate(dif = Month.1.x - Month.1.y) %>%
	summarise(min(dif, na.rm = TRUE),
			  max(dif, na.rm = TRUE))
# ok, these are very small differences
# check for all months
aux_Orig <- original_data1 %>%
	select(all_of(colnames(original_data1)[1:6]),
		   contains("Month."))
aux_Curr <- current_data1 %>%
	select(all_of(colnames(original_data1)[1:6]),
		   contains("Month."))
aux_Orig <- aux_Orig %>%
	pivot_longer(cols = contains("Month."),
				 names_to = "var_name",
				 values_to = "var_value")
aux_Curr <- aux_Curr %>%
	pivot_longer(cols = contains("Month."),
				 names_to = "var_name",
				 values_to = "var_value")
aux_Orig <- aux_Orig %>%
	left_join(aux_Curr,
			  by = c("...1",
			  	   "Duration..in.seconds.",
			  	   "ResponseId",
			  	   "phase",
			  	   "team_name",
			  	   "domain",
			  	   "var_name"))
aux_Orig <- aux_Orig %>%
	mutate(dif = var_value.x - var_value.y)
aux_Orig %>%
	summarise(min(dif, na.rm = TRUE),
			  max(dif, na.rm = TRUE))
# ok, so very very small differences. 
remove(aux_Orig, aux_Curr)

# test if all is the same if I ignore the Month. columns
which(str_detect(colnames(original_data1), "compare"))
colnames(original_data1)[c(1:6, 25:79,
						   which(str_detect(colnames(original_data1), "compare")))]
original_data1 %>%
	select(all_of(colnames(original_data1)[c(1:6, 25:79, 81:83,
											 which(str_detect(colnames(original_data1), "compare")),
											 which(colnames(original_data1) %in% c("TournamentStart",
											 									  "ForecastisUpdated")))])) %>%
	anti_join(current_data1) %>%
	nrow()
same_columns <- c(1:6, 25:79, 81:83,
				  which(str_detect(colnames(original_data1), "compare")),
				  which(colnames(original_data1) %in% c("TournamentStart",
				  									  "ForecastisUpdated")))
length(same_columns)
length(unique(same_columns))
original_data1 %>%
	select(all_of(colnames(original_data1)[same_columns])) %>%
	anti_join(current_data1) %>%
	nrow()
to_check <- setdiff(c(1:length(colnames(original_data1))),
					same_columns)


original_data1 %>%
	select(all_of(colnames(original_data1)[c(1:6, to_check)])) %>%
	anti_join(current_data1) %>%
	nrow()
aux_Orig <- original_data1 %>%
	select(all_of(colnames(original_data1)[c(1:6, 
											 to_check)]))
aux_Curr <- current_data1 %>%
	select(all_of(colnames(original_data1)[c(1:6, 
											 to_check)]))
aux_Orig <- aux_Orig %>%
	pivot_longer(cols = -c("...1",
						   "Duration..in.seconds.",
						   "ResponseId",
						   "phase",
						   "team_name",
						   "domain"),
				 names_to = "var_name",
				 values_to = "var_value")
aux_Curr <- aux_Curr %>%
	pivot_longer(cols = -c("...1",
						   "Duration..in.seconds.",
						   "ResponseId",
						   "phase",
						   "team_name",
						   "domain"),
				 names_to = "var_name",
				 values_to = "var_value")
aux_Orig <- aux_Orig %>%
	left_join(aux_Curr,
			  by = c("...1",
			  	   "Duration..in.seconds.",
			  	   "ResponseId",
			  	   "phase",
			  	   "team_name",
			  	   "domain",
			  	   "var_name"))
aux_Orig <- aux_Orig %>%
	mutate(dif = var_value.x - var_value.y)
aux_Orig %>%
	summarise(min(dif, na.rm = TRUE),
			  max(dif, na.rm = TRUE))
# these difference are a bit bigger
aux_Orig %>%
	filter(dif > 0.01)
aux_Orig %>%
	filter(dif < -0.01)
# these seem to be responses that are not related to a team, and where the
# actual value is very large, so a diff of 0.01 shouldn't mean much weird, but
# it is what it is

# Wave1+2demographics.csv ----
# same number of columns?
length(original_data2)
length(current_data2)
# yes
# same columns?
setdiff(colnames(original_data2),
		colnames(current_data2))
setdiff(colnames(current_data2),
		colnames(original_data2))
# same values?
original_data2 %>%
	anti_join(current_data2) %>%
	nrow()
original_data2 %>%
	anti_join(current_data2)
# ok, only one observation is different
original_data2 %>%
	anti_join(current_data2) %>%
	select(ResponseId)
original_data2 %>%
	anti_join(current_data2) %>%
	select(ResponseId) %>%
	left_join(original_data1,
			  by = "ResponseId")
current_data2 %>%
	anti_join(original_data2) %>%
	nrow()
current_data2 %>%
	anti_join(original_data2) %>%
	select(ResponseId)
