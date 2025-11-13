if(!require(shiny)) install.packages(
  "shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyjs)) install.packages(
  "shinyjs", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages(
  "shinythemes", repos = "http://cran.us.r-project.org")
if(!require(shinyauthr)) install.packages(
  "shinyauthr", repos = "http://cran.us.r-project.org")
if(!require(htmltools)) install.packages(
  "htmltools", repos = "http://cran.us.r-project.org")
if(!require(bslib)) install.packages(
  "bslib", repos = "http://cran.us.r-project.org")
if(!require(mongolite)) install.packages(
  "mongolite", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages(
  "ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages(
  "ggiraph", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages(
  "sf", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages(
  "dplyr", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages(
  "DT", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages(
  "stringr", repos = "http://cran.us.r-project.org")

if(!require(leaflet)) install.packages(
  "leaflet", repos = "http://cran.us.r-project.org")
if(!require(leaflegend)) install.packages(
  "leaflegend", repos = "http://cran.us.r-project.org")
if(!require(paletteer)) install.packages(
  "paletteer", repos = "http://cran.us.r-project.org")

source("utils.r")

# MAP CONSTANTS
nc_center_lat <- 35.5
nc_center_lng <- -79.2
nc_center_zoom <- 7
nc_block_zoom <- 13
nc_obs_zoom <- 13
ncba_blue <- "#2a3b4d"
ncba_white <- "#ffffff"

category_colors <- c("#6a51a3", "#9e9ac8", "#cbc9e2", "#F2F0F7")
breeding_category_pal <- colorFactor(
  palette = category_colors,
  domain = breeding_categories
)

## TESTING
observations <- get_observations("Yellow Warbler")

form_data <- data.frame(
  "species_status" = c(observations$status),
  "species_safe_date_start_jd" = c(observations$sd_start_julian),
  "species_safe_date_end_jd" = c(observations$sd_end_julian)
)
# form_data$all_obs_data <- observations$records

od <- observations$records
## END TESTING

ggplot(
# gg_point <- ggplot(
  data = od,
  aes(
    x = JULIAN_DAY,
    y = BREEDING_CODE,
    color = BREEDING_CATEGORY,
    fill = CHECK_FLAGGED
  ),
) +
  geom_vline(
    linetype = "dashed",
    color = "gray",
    linewidth = 1,
    xintercept = c(
      form_data$species_safe_date_start_jd,
      form_data$species_safe_date_end_jd
    )
  ) +
  labs(y = "Breeding Code", x = "Julian Day") +
  geom_point_interactive(
    aes(
      x = JULIAN_DAY,
      y = BREEDING_CODE,
      tooltip = SEI,
      data_id = GUID,
      onclick = paste0(
        'Shiny.onInputChange("obs_clicked","', GUID, '")'
      )
    ),
    show.legend = FALSE,
    position = position_jitter(
      width = 0.3,
      height = 0.3
    )
  ) +
  # scale_color_manual(values = categorycolors) +
  xlim(0, 365) +
  theme_minimal()

pblock_data_sf <- block_data_sf %>%
  filter(
    PRIORITY == 1
  )
map_plot <- ggplot() +
  geom_sf(
    data = pblock_data_sf,
    color = "#999999",
    fill = NA
  ) +
  theme(
    text = element_text(family = "segoe ui"),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

girafe(
  ggobj = gg_point,
  width_svg = 10,
  options = list(opts_sizing(rescale = TRUE))
)