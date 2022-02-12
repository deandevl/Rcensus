library(jsonlite)
library(data.table)
library(httr)
library(Rcensus)

# Get the geographies available for dataset "acs/acs1/profile" with vintage 2019
acs1_profile_geo_dt <- Rcensus::get_geography(
  dataset = "acs/acs1/profile",
  vintage = 2019
)