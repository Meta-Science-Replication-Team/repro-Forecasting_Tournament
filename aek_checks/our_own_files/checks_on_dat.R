#> this script checks if some of the datafiles contain duplicate information
#> that is: file A and file B have the same info, so keeping only one is ok

library("tidyverse")

dat_for_analyses <- read_csv(here::here("dat_for_analyses.csv"))
dat_long <- read_csv(here::here("dat_long.csv"))

#> these files have so many columns it is difficult to even understand what's in here
#> the files are produced by `Wave 1+2 descriptives.Rmd`
#> first `dat_for_analyses` and then `dat_long`
#> so in principle I expect them to have overlapping info
#> they are saved with write.csv, which add row numbers in `...1`

length(colnames(dat_for_analyses))
length(colnames(dat_long))

#> dat_for_analyses has 159 columns, and dat_long has 144
length(intersect(colnames(dat_for_analyses),
                 colnames(dat_long)))
#> 141 of the columns are shared
length(setdiff(colnames(dat_for_analyses),
               colnames(dat_long)))
setdiff(colnames(dat_for_analyses),
        colnames(dat_long))
#> 18 columns in dat_for_analyses and not in dat_long
length(setdiff(colnames(dat_long),
               colnames(dat_for_analyses)))
setdiff(colnames(dat_long),
        colnames(dat_for_analyses))
#> and 3 in dat_long but not in dat_for_analyses
#> Month and value capture the same info as Month.1 to Month 18
#> or at least they should, because that's just rearranged data
#> value.diff is calculated, so that needs to be checked extra

# check shared columns
aux_analyses <- dat_for_analyses %>%
  select(all_of(intersect(colnames(dat_for_analyses),
                          colnames(dat_long))))
aux_long <- dat_long %>%
  select(all_of(intersect(colnames(dat_for_analyses),
                          colnames(dat_long))))
aux_analyses %>%
  distinct() %>%
  nrow()
aux_long %>%
  distinct() %>%
  nrow()

aux_analyses %>%
  select(-`...1`) %>%
  distinct() %>%
  nrow()
aux_long %>%
  select(-`...1`) %>%
  distinct() %>%
  nrow()
#> magic. once I remove the row numbers, same number of unique values

aux_long <- aux_long %>%
  select(-`...1`) %>%
  distinct()
aux_analyses <- aux_analyses %>%
  select(-`...1`)
#> check if same content
if(aux_analyses %>%
   anti_join(aux_long) %>%
   nrow()) {
  message("there are some different values")
} else {
  message("all values are exactly the same")
}
if(aux_long %>%
   anti_join(aux_analyses) %>%
   nrow()) {
  message("there are some different values")
} else {
  message("all values are exactly the same")
}
# well, if all content is the same, then no need to keep the duplicate info
df_shared_info <- aux_analyses
remove(aux_analyses,
       aux_long)

# check for unique identifiers
df_shared_info %>%
  distinct() %>%
  nrow()
df_shared_info %>%
  select(ResponseId,
         team_name) %>%
  distinct() %>%
  nrow()
df_shared_info %>%
  select(ResponseId) %>%
  distinct() %>%
  nrow()
df_shared_info %>%
  select(team_name) %>%
  distinct() %>%
  nrow()
# unclear
df_shared_info %>%
  select(X) %>%
  distinct() %>%
  nrow()
# X seems to identify each of the 2265 rows, so I'll work with that for now

aux_analyses <- dat_for_analyses %>%
  select(-`...1`)
aux_long <- aux_analyses %>%
  pivot_longer(cols = starts_with("Month"),
               names_to = "Month",
               names_prefix = "Month.",
               names_transform = as.numeric,
               values_to = "value")
# in Wave 1+2 descriptives.Rmd, they remove rows with NA values
aux_long <- aux_long %>%
  filter(!is.na(value))
# the following is based on the original code, edited for clarity
setdiff(colnames(dat_long),
        colnames(aux_long))
# all other values should match
dat_long %>%
  select(-`...1`,
         -value.dif) %>%
  anti_join(aux_long) %>%
  nrow()
aux_long %>%
  anti_join(dat_long %>%
              select(-`...1`,
                     -value.dif)) %>%
  nrow()
# perfect match, great.
# now what remains to be checked is value.diff



pct_change <- function(previous, new, as_decimal = FALSE) {
  x <- abs(((new - previous) / previous) * 100)
  if (as_decimal) x <- x / 100
  return(x)
}

domains <- c("lifesat",
             "posaffect",
             "negaffect",
             "ideoldem",
             "ideolrep",
             "polar",
             "iasian",
             "easian",
             "iafric",
             "eafric",
             "igend",
             "egend")
aux_long$value.dif <- as.numeric(NA)

# I have to use df, not tibble, because otherwise pct_change would not work
aux_analyses <- as.data.frame(aux_analyses)

for(i in 1:length(domains)) {
  # Retrieve row with correct historical value for the domain
  hist <-  aux_analyses[which(aux_analyses$domain == domains[i] &
                                aux_analyses$Method.coded == 5), ]
  
  for(n in 1:12) {
    # retrieve all rows from dat_long that match the domain + Month n and calculate the correct absolute percent difference
    histval <- hist[1, paste0("Month.", n)]
    predval <- aux_long[which(aux_long$domain == domains[i] &
                                aux_long$Month == n), "value"]
    
    aux_long[which(aux_long$domain == domains[i] &
                     aux_long$Month == n), "value.dif"] <- pct_change(histval, predval)
  }
  
}

as.vector(aux_long[which(aux_long$domain == domains[i] &
                           aux_long$Month == n), "value.dif"])
as.vector(dat_long[which(aux_long$domain == domains[i] &
                           aux_long$Month == n), "value.dif"])
summary(aux_long[which(aux_long$domain == domains[i] &
                         aux_long$Month == n), "value.dif"] -
          dat_long[which(aux_long$domain == domains[i] &
                           aux_long$Month == n), "value.dif"])
summary(aux_long[["value.dif"]] -
          dat_long[["value.dif"]])
sum(is.na(aux_long[["value.dif"]]))
sum(is.na(dat_long[["value.dif"]]))

# compare dat_long with aux_long
aux_long %>%
  select(value.dif) %>%
  mutate(value.dif = round(value.dif, digits = 6)) %>%
  anti_join(dat_long %>%
              select(value.dif) %>%
              mutate(value.dif = round(value.dif, digits = 6))) %>%
  nrow()
aux_long %>%
  select(value.dif) %>%
  mutate(value.dif = round(value.dif, digits = 4)) %>%
  anti_join(dat_long %>%
              select(value.dif) %>%
              mutate(value.dif = round(value.dif, digits = 4)))
setdiff(colnames(aux_long),
        colnames(dat_long))
setdiff(colnames(dat_long),
        colnames(aux_long))
# seem to have exactly the same columns. 

col_indices <- c(3:143)
aux_long %>%
  select(all_of(colnames(dat_long)[col_indices])) %>%
  anti_join(dat_long %>%
              select(all_of(colnames(dat_long)[col_indices]))) %>%
  nrow()
colnames(dat_long)[-col_indices]



aux_analyses <- dat_for_analyses %>%
  select(X,
         all_of(setdiff(colnames(dat_for_analyses),
                        colnames(dat_long))))
aux_long <- dat_long %>%
  select(X,
         all_of(setdiff(colnames(dat_long),
                        colnames(dat_for_analyses))))
aux_analyses <- aux_analyses %>%
  pivot_longer(cols = starts_with("Month"),
               names_to = "Month",
               names_prefix = "Month.",
               names_transform = as.numeric,
               values_to = "value")
# in Wave 1+2 descriptives.Rmd, they remove rows with NA values
aux_analyses <- aux_analyses %>%
  filter(!is.na(value))
# the following is based on the original code, edited for clarity

pct_change <- function(previous, new, as_decimal = FALSE) {
  x <- abs(((new - previous) / previous) * 100)
  if (as_decimal) x <- x / 100
  return(x)
}

domains <- c("lifesat",
             "posaffect",
             "negaffect",
             "ideoldem",
             "ideolrep",
             "polar",
             "iasian",
             "easian",
             "iafric",
             "eafric",
             "igend",
             "egend")
dat <- dat_for_analyses
i = 1
for(i in 1:length(domains)) {
  # Retrieve row with correct historical value for the domain
  hist <-  dat[which(dat$domain == domains[i] &
                       dat$Method.coded == 5), ]
  
  for(n in 1:12) {
    # retrieve all rows from dat_long that match the domain + Month n and calculate the correct absolute percent difference
    histval <- hist[1, paste0("Month.", n)]
    predval <- dat_long[which(dat_long$domain == domains[i] &
                                dat_long$Month == n), "value"]
    
    dat_long[which(dat_long$domain == domains[i] &
                     dat_long$Month == n), "value.dif"] <- pct_change(histval, predval)
  }
  
}



