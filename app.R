# NC Bird Atlas Data Review Shiny App - Public Version
# v1
# 10/15/2025
# Scott K. Anderson
# https://github.com/nmtarr/NCBA/ncba_data_review

if(!require(shiny)) install.packages(
  "shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages(
  "shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages(
  "tidyverse", repos = "http://cran.us.r-project.org")
if(!require(mongolite)) install.packages(
  "mongolite", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages(
  "dplyr", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages(
  "leaflet", repos = "http://cran.us.r-project.org")
if(!require(leaflegend)) install.packages(
  "leaflegend",
  repos = "http://cran.us.r-project.org"
)


############################################################################
## UI

ui <- bootstrapPate(
  navbarPage(
    theme = shinytheme("flatly")
  )
)