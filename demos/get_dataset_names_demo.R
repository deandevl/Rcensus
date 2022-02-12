library(jsonlite)
library(data.table)
library(httr)
library(Rcensus)

# Get all datasets under the Population Estimates Program (PEP)
pep_datasets_dt <- Rcensus::get_dataset_names(filter_str = "pep")