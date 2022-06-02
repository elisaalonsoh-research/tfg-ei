#------------------------------------------------------------------------------#
# Clean CPS Voting and Registration Supplement (VRS)
# Author: Elisa Alonso Herrero
# Date: 29/04/2022
# This script uses the "cpsvote" package to get the VRS data from the years 
# 2006-2018. The clean data is extracted at:
#          - cpsvoters.parquet (individual information)
#          - cpsturnout.parquet (county aggregates)
# Package documentation at: https://cran.r-project.org/web/packages/cpsvote/vignettes/basics.html
#------------------------------------------------------------------------------#

# ---- SET UP ----------------------------------------

rm(list = ls()) # Clear environment

# Load packages

pacman::p_load(here, rio, tidyr, dplyr, cpsvote)


# ---- DOWNLOAD DATA ------------------------------------

## Uncomment to download raw data and documentation

# cps_download_data(path = here("data/raw/cps_data"), years= seq(2006,2018,2))
 

# ---- SPECIFY DESIRED VARIABLES -------------------------

###### Selecting the variables to extract frOM raw files:
# The "cpsvote" already selected and consolidated some basic variables over the 
# years in "cpscols". These include some demographic info and all the 
# registration and voting variables. To add NEW variables, we need to specify 
# which column positions contain the variable in each year (may change: see doc)
# and the factor levels needed. 
#       |  new_name  | cps_name |start | end | # of levels |
#       |FAM_INCOME  |HUFAMINC  | 39   | 40  |      16     |   



all_years <- c(2006,2008,2010,2012,2014,2016,2018)

# Identifier:


# Geographic columns:

# Family Income:
income_cols <- data.frame(
  year = all_years,
  cps_name = "HUFAMINC",
  new_name = "FAM_INCOME",
  start_pos = 39,
  end_pos = 40,
  stringsAsFactors = FALSE
)

income_factors <- data.frame(
  year = c(sapply(all_years, rep, each=16)), # Replicates each year x # of factors (16)
  cps_name = "HUFAMINC",
  new_name = "FAM_INCOME",
  code = c(rep(1:16, times=7)),# Repeats 1:16 x # of years (7)
  value = rep(c("LESS THAN $5,000",
               "5,000 TO 7,499",
               "7,500 TO 9,999",
               "10,000 TO 12,499",
               "12,500 TO 14,999",
               "15,000 TO 19,999",
               "20,000 TO 24,999",
               "25,000 TO 29,999",
               "30,000 TO 34,999",
               "35,000 TO 39,999",
               "40,000 TO 49,999",
               "50,000 TO 59,999",
               "60,000 TO 74,999",
               "75,000 TO 99,999",
               "100,000 TO 149,999",
               "150,000 OR MORE"), 7), # Factor labels x # of years (7)
  stringsAsFactors = FALSE
)




# Bind custom columns with default data
my_cols <- bind_rows(cps_cols, income_cols)
my_factors <- bind_rows(cps_factors, income_factors)


# ---- IMPORT DATA -------------------------------------

cps <- cps_read(years = seq(2006,2018,2),
                dir = here("data/raw/cps_data"),
                cols = my_cols,
                names_col = "new_name",
                join_dfs = TRUE) %>%
  cps_label(factors = my_factors,
            names_col = "new_name",
            na_vals = c("-1", "BLANK", "NOT IN UNIVERSE"),
            expand_year = TRUE,
            rescale_weight = TRUE,
            toupper = TRUE) %>%
  cps_refactor(move_levels = TRUE) %>%
  cps_recode_vote(vote_col = "VRS_VOTE",
                  items = c("DON'T KNOW", "REFUSED", "NO RESPONSE")) %>%
  cps_reweight_turnout()

# ---- EXPORT DATA ------------------------------------
# export(cps_ind, here("data/processed/cps_voters.parquet"))