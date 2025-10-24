library("DT")
library("htmltools")
library("mongolite")
library("dplyr")
library("ggiraph")
library("ggplot2")
if(!require(ggridges)) install.packages(
  "ggridges", repos = "http://cran.us.r-project.org")
source("utils.r")
spp_name <- "Acadian Flycatcher"
acfl <- get_observations(spp_name)



od <- acfl

gg_point <- ggplot(
  data = od,
  aes(
    x = JULIAN_DAY,
    y = BREEDING_CODE,
  )
) +
  geom_vline_interactive(
    xintercept = 100,
    # x_intercept = form_data$species_safe_date_start_jd,
    linetype = "dashed",
    color = "red",
    # size = 1,
    # aes(tooltip = "Safe Dates")
  ) +
  labs(y = "Breeding Code", x = "Julian Day") +
  geom_boxplot(
    aes(
      x = JULIAN_DAY,
      y = BREEDING_CODE,
      fill = BREEDING_CATEGORY
    ),
    show.legend = FALSE
  ) +
  scale_fill_manual(values = categorycolors) +
  geom_point_interactive(
    aes(
      x = JULIAN_DAY,
      y = BREEDING_CODE,
      tooltip = SEI,
      data_id = GUID,
      onclick = paste0(
        'Shiny.onInputChange("obs_clicked","', GUID, '")'
      ),
    ),
    show.legend = FALSE,
    position = position_jitter(
      width = 0.3,
      height = 0.3
    )
  ) +
  xlim(0, 365) +
  theme_minimal()

girafe(
  ggobj = gg_point,
  width_svg = 10,
  options = list(opts_sizing(rescale = TRUE))
)



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
