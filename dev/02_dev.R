# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "dplyr" )
usethis::use_package( "tidyr" )
usethis::use_package( "RSQLite" )
usethis::use_package( "ggplot2" )
usethis::use_package( "purrr" )
usethis::use_package( "stringr" )
usethis::use_package( "tibble" )
usethis::use_package( "shiny" )
usethis::use_package( "shinydashboard" )
usethis::use_package( "DBI" )
usethis::use_package( "data.table" )
usethis::use_package( "lubridate" )
usethis::use_package( "zoo" )
usethis::use_package( "plotly" )
usethis::use_package( "ggthemes" )
usethis::use_package( "janitor" )
usethis::use_package( "caTools" )

## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "analytics" ) # Name of the module
golem::add_module( name = "processing" )
golem::add_module( name = "setup" )
golem::add_module( name = "subject" )
golem::add_module( name = "upload" )

## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "summary_functions" )
# golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
# golem::add_js_file( "script" )
# golem::add_js_handler( "handlers" )
# golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
# usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
# usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("cgmshiny")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
# usethis::use_travis()
# usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

