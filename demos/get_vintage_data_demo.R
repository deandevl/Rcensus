library(jsonlite)
library(data.table)
library(httr)
library(Rcensus)
library(usmap)

# Get a single variable and region from "dec/sf1" dataset
sf1_pop_long_df <- Rcensus::get_vintage_data(
  dataset = "dec/sf1",
  vintage = 2010,
  vars = c("NAME", "P001001"),
  region = "state"
)

# Get the state FIPS code for Kentucky
kentucky_fips <- usmap::fips("KY")
# Kentucky fips is 21

# Get estimate of number of computers among 1.7m households of Kentucky in 2019
acs1_profile_total_households_df <- Rcensus::get_vintage_data(
  dataset = "acs/acs1/profile",
  vintage = 2019,
  vars = c("NAME", "DP02_0152E", "DP02_0153E","DP02_0151E"),
  region = "state:21"
)

# Get all the variables under the group "B01001" in the "acs/acs5" dataset
B01001_dt <- Rcensus::get_vintage_data(
  dataset = "acs/acs5",
  vintage = 2019,
  group = "B01001",
  region = "state"
)
#Order by the "NAME" column
B01001_dt <- B01001_dt[order(NAME)]

# Get a wide version of group variable "B19001"
B19001_1yr_E_dt <- Rcensus::get_vintage_data(
  dataset = "acs/acs1",
  vintage = 2016,
  group = "B19001",
  region = "state",
  dcast_for = "NAME ~ variable",
  dcast_var = c("Estimate", "MOE")
)

# Get a long version of the "pep/components" dataset
ny_queens_fips <- usmap::fips(state = "NY", county = "Queens")
select_cols <- c("BIRTHS","DEATHS","DOMESTICMIG","INTERNATIONALMIG","NATURALINC","NETMIG","RBIRTH","RDEATH","RDOMESTICMIG","RINTERNATIONALMIG")
queens_components_dt <- Rcensus::get_vintage_data(
  dataset = "pep/components",
  vintage = 2019,
  vars = c("NAME", "GEO_ID", select_cols),
  region = paste0("county:", substr(ny_queens_fips,3,5)),
  regionin = paste0("state:", substr(ny_queens_fips,1,2)),
  melt_meas = list(select_cols)
)