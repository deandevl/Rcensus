library(jsonlite)
library(data.table)
library(httr)
library(Rcensus)
library(usmap)

# Get multiple years (2010 to 2019) of numbers of college degree holders
#   across all counties in Colorado

# Get the fips for Colorado
co_fips <- usmap::fips("CO")

# Set up the the vintage years and variables
vintages <- 2010:2019

college_var_names <- c(
  "B15002_015",
  "B15002_016",
  "B15002_017",
  "B15002_018",
  "B15002_032",
  "B15002_033",
  "B15002_034",
  "B15002_035"
)

college_vars_E <- c()
college_vars_M <- c()

for(i in seq_along(college_var_names)){
  college_vars_E <- c(college_vars_E, paste0(college_var_names[[i]],"E"))
  college_vars_M <- c(college_vars_M, paste0(college_var_names[[i]],"M"))
}

# Get the dataframe
college_by_year_dt <- Rcensus::get_multi_vintage_data(
  dataset = "acs/acs1",
  vintage_v = vintages,
  vars = list(
    estimate = college_vars_E,
    moe = college_vars_M
  ),
  region = "county:*",
  regionin = paste0("state:", co_fips)
)

# Order by "NAME", "variable", "vintage"
data.table::setorderv(college_by_year_dt, c("NAME", "variable", "vintage"))



