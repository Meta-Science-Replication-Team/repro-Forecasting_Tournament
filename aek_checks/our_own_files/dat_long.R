#> this script checks if some of the datafiles contain duplicate information
#> that is: file A and file B have the same info, so keeping only one is ok

library("tidyverse")

dat_long_V1 <- read_csv(here::here("dat_long.csv"))
dat_long_V2 <- read_csv(here::here("Data Cleaning",
                                   "dat_long.csv"))
# different number of observations and variables between the two datasets

# in V1 but not in V2:
setdiff(colnames(dat_long_V1),
        colnames(dat_long_V2))
shared_cols_V1_V2 <- intersect(colnames(dat_long_V1),
                               colnames(dat_long_V2))
shared_cols_V1_V2 <- setdiff(shared_cols_V1_V2, "...1")
dat_long_V1 %>%
  select(-`...1`) %>%
  anti_join(dat_long_V2 %>%
              select(-`...1`),
            by = shared_cols_V1_V2[2:3]) %>%
  nrow()
# even for variables shared between the two datasets, values are different

# after knitting Wave 1+2 Descriptives.Rmd
dat_long_V3 <- read_csv(here::here("dat_long.csv"))
shared_cols_V1_V3 <- intersect(colnames(dat_long_V1),
                               colnames(dat_long_V3))
shared_cols_V1_V3 <- setdiff(shared_cols_V1_V3, "...1")
dat_long_V1 %>%
  select(-`...1`) %>%
  anti_join(dat_long_V3 %>%
              select(-`...1`),
            by = shared_cols_V1_V3[2:20]) %>%
  nrow()
# even for variables shared between the two datasets, values are different
shared_cols_V1_V3[2:20]
# the differences appear for covidest_1
dat_long_V1 %>%
  select(-`...1`) %>%
  select(all_of(shared_cols_V1_V3[c(2:6, 20)])) %>%
  anti_join(dat_long_V3 %>%
              select(-`...1`),
            by = shared_cols_V1_V3[c(2:6, 20)])
# check data for this one responseId R_3EfLRdOVE09AFNs

dat_long_V1 %>%
  select(-`...1`) %>%
  select(all_of(shared_cols_V1_V3[c(2:6, 20)])) %>%
  anti_join(dat_long_V3 %>%
              select(-`...1`),
            by = shared_cols_V1_V3[c(2:6, 20)]) %>%
  distinct()
# why are these rows duplicate?
dat_long_V1 %>%
  distinct() %>%
  nrow()
dat_long_V1 %>%
  select(-`...1`,
         -X) %>%
  distinct() %>%
  nrow()

dat_long_V1 %>%
  select(ResponseId) %>%
  distinct() %>%
  nrow()
dat_long_V1 %>%
  select(team_name) %>%
  distinct() %>%
  nrow()
dat_long_V1 %>%
  select(ResponseId,
         phase) %>%
  distinct() %>%
  nrow()
dat_long_V1 %>%
  group_by(phase) %>%
  summarise(n())
# so two phases 

df_check <- dat_long_V1 %>%
  select(team_name,
         phase,
         ResponseId,
         domain)
df_check <- df_check %>%
  group_by(team_name,
           phase,
           ResponseId,
           domain) %>%
  mutate(n_obs = n()) %>%
  ungroup()
# it is unclear what is one observation in these datasets
# so what uniquely identifies a row / observation

dat_long_V1 %>%
  filter(ResponseId %in% c("R_3EfLRdOVE09AFNs")) %>%
  select(-`...1`) %>%
  select(all_of(shared_cols_V1_V3[c(2:6, 20)])) %>%
  View()

dat_long_V1 %>%
  filter(ResponseId %in% c("R_3EfLRdOVE09AFNs")) %>%
  select(-`...1`) %>%
  select(all_of(shared_cols_V1_V3[c(2:6, 20)])) %>%
  left_join(dat_long_V3 %>%
              filter(ResponseId %in% c("R_3EfLRdOVE09AFNs")) %>%
              select(-`...1`) %>%
              select(all_of(shared_cols_V1_V3[c(2:6, 20)])),
            by = shared_cols_V1_V3[c(2:6)]) %>%
  View()



dat_long_V3 <- read_csv(here::here("dat_long.csv"))