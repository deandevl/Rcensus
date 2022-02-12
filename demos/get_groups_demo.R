
library(jsonlite)
library(data.table)
library(httr)
library(Rcensus)

acs1_profile_groups_dt <- Rcensus::get_groups(
  dataset = "acs/acs1/profile",
  vintage = 2019)
