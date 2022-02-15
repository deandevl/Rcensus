library(jsonlite)
library(data.table)
library(httr)
library(Rcensus)
library(usmap)

# ------------------non-group variables-----------------

# Get a long version of the "pep/components" dataset
ny_queens_fips <- usmap::fips(state = "NY", county = "Queens")
queens_components_dt <- Rcensus::get_vintage_data(
  dataset = "pep/components",
  vintage = 2019,
  vars = c("BIRTHS","DEATHS","DOMESTICMIG","INTERNATIONALMIG","NATURALINC","NETMIG","RBIRTH","RDEATH","RDOMESTICMIG","RINTERNATIONALMIG"),
  region = paste0("county:", substr(ny_queens_fips,3,5)),
  regionin = paste0("state:", substr(ny_queens_fips,1,2))
)

# Get a single variable and region from "dec/sf1" dataset
sf1_pop_long_df <- Rcensus::get_vintage_data(
  dataset = "dec/sf1",
  vintage = 2010,
  vars = c("P001001"),
  region = "state"
)

# Get the state FIPS code for Kentucky
kentucky_fips <- usmap::fips("KY")
# Kentucky fips is 21

# Get estimate of number of computers among 1.7m households of Kentucky in 2019
# Returns a data.table in the "long" format by default
acs1_total_households_long_dt <- Rcensus::get_vintage_data(
  dataset = "acs/acs1/profile",
  vintage = 2019,
  vars = c("DP02_0152E", "DP02_0153E","DP02_0151E"),
  region = "state:21"
)

# Get the same data.table in the "wide" format
acs1_total_households_wide_dt <- Rcensus::get_vintage_data(
  dataset = "acs/acs1/profile",
  vintage = 2019,
  vars = c("DP02_0152E", "DP02_0153E","DP02_0151E"),
  shape = "wide",
  region = "state:21"
)

# Get the estimate and margin of error values for the above same data request
var_names <- c("DP02_0152", "DP02_0153","DP02_0151")
vars_E <- c()
vars_M <- c()
for(i in seq_along(var_names)){
  vars_E <- c(vars_E, paste0(var_names[[i]],"E"))
  vars_M <- c(vars_M, paste0(var_names[[i]],"M"))
}
acs1_total_households_est_moe_dt <- Rcensus::get_vintage_data(
  dataset = "acs/acs1/profile",
  vintage = 2019,
  vars = list(estimate = vars_E, moe = vars_M),
  region = "state:21"
)

vars <- Rcensus::get_variables(
  dataset = "acs/flows",
  vintage = 2018,
  brief = TRUE
)
db <- Rcensus::get_dataset_names(
  filter_str = "flow",
  year = 2018
)

# consolidate 6 variables into 2 variables using the "melt_meas" and "melt_values" arguments
honolulu_fips <- usmap::fips(state = "HI", county = "Honolulu")

honolulu_migration_dt <- Rcensus::get_vintage_data(
  dataset = "acs/flows",
  vintage = 2018,
  vars_init = NULL,
  vars = c("FULL1_NAME", "FULL2_NAME", "MOVEDIN", "MOVEDIN_M","MOVEDOUT", "MOVEDOUT_M","MOVEDNET","MOVEDNET_M"),
  region = paste0("county:", substr(honolulu_fips,3,5)),
  regionin = paste0("state:", substr(honolulu_fips,1,2)),
  melt_meas = list(estimate = c("MOVEDIN","MOVEDOUT","MOVEDNET"), moe = c("MOVEDIN_M","MOVEDOUT_M","MOVEDNET_M")),
  melt_values = c("MOVEDIN","MOVEDOUT","MOVEDNET")
)

# ------------------------group variables--------------------

# Get all the variables under the group "B01001" in the "acs/acs5" dataset
# Returns a data.table in the "long" format by default
B01001_long_dt <- Rcensus::get_vintage_data(
  dataset = "acs/acs5",
  vintage = 2019,
  group = "B01001",
  region = "state"
)

# Get the same data.table in the "wide" format
B01001_wide_dt <- Rcensus::get_vintage_data(
  dataset = "acs/acs5",
  vintage = 2019,
  group = "B01001",
  shape = "wide",
  region = "state"
)

# Get just the estimate values for the above same data request
B01001_est_dt <- Rcensus::get_vintage_data(
  dataset = "acs/acs5",
  vintage = 2019,
  group = "B01001",
  group_values = c("estimate"),
  region = "state"
)



