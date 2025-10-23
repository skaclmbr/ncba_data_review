library("DT")
library("htmltools")
library("mongolite")
library("dplyr")
source("utils.r")
spp_name <- "Acadian Flycatcher"
acfl <- get_observations(spp_name)

data(mtcars)

species_codes <- read.csv("ncba_species_codes.csv")

acfl_codes <- species_codes %>%
  filter(SPECIES == spp_name)

acfl_table <- acfl %>%
  merge(
    acfl_codes,
    by = c("BREEDING_CODE", "ECOREGION")
  ) %>%
  select(
    "SEI",
    "OBSERVATION_DATE",
    "BREEDING_CODE",
    "BREEDING_CATEGORY",
    "NC_STATUS",
    "ECOREGION",
    "JULIAN_DAY",
    "EBIRD_LINK",
    "SUITABILITY",
    "GUID"
  )
safe_date_start_jd <- min(acfl_codes$SAFE_DATE_START_JD) - 1
safe_date_end_jd <- min(acfl_codes$SAFE_DATE_END_JD)

datatable(
  acfl_table,
  colnames = c("","Checklist", "Date", "Code", "Category", "Status", "Ecoregion"),
  options = list(
    dom = 't',
    columnDefs = list(
      list(
        className = "dt-center",
        targets = c(1, 2, 3, 4, 5, 6)
        ),
      list(
        visible = TRUE,
        targets = c(1,2,3,4,5,6)
      ),
      list(
        visible = FALSE,
        targets = "_all"
      )
    )
  )
) %>%
formatStyle(
  columns = "OBSERVATION_DATE",
  valueColumns = "JULIAN_DAY",
  backgroundColor = styleInterval(
    c(safe_date_start_jd, safe_date_end_jd),
    c("red", "green", "red")
  ),
  color = styleInterval(
    c(safe_date_start_jd, safe_date_end_jd),
    c("black", "white", "black")
  )
) %>%
formatStyle(
  columns = "BREEDING_CODE",
  valueColumns = "SUITABILITY",
  backgroundColor = styleEqual(
    c("S", "U", "F"),
    c("green", "red", "yellow")
  ),
  color = styleEqual(
    c("S", "U", "F"),
    c("white", "black", "black")
  )
)
