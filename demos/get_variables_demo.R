library(jsonlite)
library(data.table)
library(httr)
library(Rcensus)

# Get data.table of variables for 'acs/acs5' for 2010
acs5_var_dt <- Rcensus::get_variables(
  dataset = "acs/acs1/profile",
  vintage =  2019)

# Get available variables for group "DP02" in the
#  "acs/acs1/profile" dataset for 2019
group_var_DP02_dt <- Rcensus::get_variables(
  dataset = "acs/acs1/profile",
  vintage = 2019,
  group = "DP02",
  brief = TRUE
)

# Get available variables that have "DP02_0068" in their id name
DP02_0068_dt <- Rcensus::get_variables(
  dataset = "acs/acs1/profile",
  vintage = 2019,
  brief = TRUE,
  filter_str = "DP02_0068"
)

# Get available variables that have "DP02_0068" in their id name
# Use multiple vintages
# Note that we get entirely different variables between the two years
DP02_0068_dt <- Rcensus::get_variables(
  dataset = "acs/acs1/profile",
  vintage = 2018:2019,
  brief = TRUE,
  filter_str = "DP02_0068"
)

# Get available variables for "pep/components" for 2019
pep_comp_var_dt <- Rcensus::get_variables(
  dataset = "pep/components",
  vintage = 2019,
  brief = TRUE
)

# Get available variables for "dec/sf1" for 2010
dec_sf1_var_dt <- Rcensus::get_variables(
  dataset = "dec/sf1",
  vintage = 2010,
  brief = TRUE
)

