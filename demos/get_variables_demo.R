library(jsonlite)
library(data.table)
library(httr)
library(Rcensus)

# data.table of variables for 'acs/acs5' for 2010
acs5_var_dt <- Rcensus::get_variables(
  dataset = "acs/acs1/profile",
  vintage =  2019)

# get available variables for group "DP02" in the
#  "acs/acs1/profile" dataset for 2019
group_DP02_dt <- Rcensus::get_variables(
  dataset = "acs/acs1/profile",
  vintage = 2019,
  group = "DP02",
  brief = TRUE
)

pep_variables_dt <- Rcensus::get_variables(
  dataset = "pep/components",
  vintage = 2019,
  brief = TRUE
)