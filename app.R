# NC Bird Atlas Data Review Shiny App - Public Version
# v1
# 10/15/2025
# Scott K. Anderson
# https://github.com/nmtarr/NCBA/ncba_data_review

if(!require(shiny)) install.packages(
  "shiny", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages(
  "shinythemes", repos = "http://cran.us.r-project.org")
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
if(!require(ggridges)) install.packages(
  "ggridges", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages(
  "dplyr", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages(
  "DT", repos = "http://cran.us.r-project.org")

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
# category_colors <- paletteer_d("lisa::FridaKahlo", n = 4)
category_colors <- c("#6a51a3", "#9e9ac8","#cbc9e2", "#F2F0F7")
breeding_category_pal <- colorFactor(
  palette = category_colors,
  # palette = c("#121510FF", "#6D8325FF", "#D6CFB7FF", "#E5AD4FFF"),
  # palette = paletteer_d("lisa::FridaKahlo")[1:4],
  domain = breeding_categories
)

############################################################################
## UI

ui <- fluidPage(
  useBusyIndicators(),
  theme = bs_theme(version = 5),
  navbarPage(
    "NCBA Data Review",
    windowTitle = "NCBA Data Review",
    tags$head(includeCSS("styles.css")),
    tabPanel(
      "Species",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectizeInput(
            "spp_select",
            h3("Species"),
            choices = species_list, options = list(
              placeholder = "Select species",
              onInitialize = I('function() {this.setValue("Black-throated Green Warbler"); }')
            )
          ),
          card(
            card_header("Observation Data"),
            htmlOutput(outputId = "observation_data")
          )
        ),
        mainPanel(
          layout_columns(
            card(
              card_header("Species Codes Timeline"),
              uiOutput(outputId = "boxplot_or_message")
            ),
            card(
              card_header("Observations"),
              leafletOutput(outputId = "obs_map")
            ),
            col_widths = c(8,4)
          )
        )
      )
    )

  )
)

server <- function(input, output, session) {

  # store current info
  form_data <- reactiveValues(
    species = NULL,
    block = NULL,
    checklist = NULL,
    observation = NULL,
    obs_data = NULL
  )

  #######################################################################
  ## OBS MAP
  # display species map
  output$obs_map <- renderLeaflet({
    leaflet() %>%
      setView(
        lng = nc_center_lng,
        lat = nc_center_lat,
        zoom = nc_center_zoom) %>%
      addProviderTiles(
        providers$Esri.WorldImagery,
        options = providerTileOptions(opacity = 1),
        group = "Aerial Map"
      ) %>%
      addProviderTiles(
        "OpenStreetMap.Mapnik",
        options = providerTileOptions(opacity = 1),
        group = "Street Map"
      )
  })

  #######################################################################
  ## LISTENERS
  ## listen for click on boxplot point, render table
  # change form_data
  observeEvent(
    input$obs_clicked,
    {form_data$observation <- input$obs_clicked}
  )
  ## listen for click on map
  observeEvent(
    input$obs_map_marker_click,
    {
      form_data$observation <- input$obs_map_marker_click$id
    }
  )

  # listen for changes to spp_select
  observeEvent(
    input$spp_select,
    {
      form_data$species <- input$spp_select
      form_data$observation <- NULL

      ## No Selection
      if (is.null(input$spp_select) | input$spp_select == "") {
        form_data$obs_data <- NULL

        # clear map
        leafletProxy("obs_map", session) %>%
          clearMarkers()

        # render text instead of boxplot
        output$boxplot_or_message <- renderUI(
          h3("No Data")
        )
      } else {
        # species is selected, see if there is data
        form_data$obs_data <- retrieve_observations(input$spp_select)

        if (nrow(form_data$obs_data) > 0) {
          od <- form_data$obs_data

          ###############################
          ## Add observations to the boxplot
          output$boxplot_or_message <- renderUI({renderGirafe(
            {
              od <- form_data$obs_data

              gg_point <- ggplot(
                data = od,
                aes(
                  x = julian_day,
                  y = breeding_code,
                )
              ) +
                labs(y = "Breeding Code", x = od$julian_day) +
                geom_boxplot(
                  aes(
                    x = julian_day,
                    y = breeding_code,
                    fill = breeding_category
                  ),
                  show.legend = FALSE
                ) +
                scale_fill_manual(values = categorycolors) +
                geom_point_interactive(
                  aes(
                    x = julian_day,
                    y = breeding_code,
                    tooltip = sei,
                    data_id = guid,
                    onclick = paste0(
                      'Shiny.onInputChange("obs_clicked","', guid, '")'
                    )
                  ),
                  show.legend = FALSE,
                  position = position_jitter(
                    width = 0.2,
                    height = 0.2
                  )
                ) +
                xlim(0, 365) +
                theme_minimal() +
                labs(title = form_data$species)

              girafe(
                ggobj = gg_point,
                width_svg = 10,
                options = list(opts_sizing(rescale = TRUE))
              )

            }
          )})

          ###############################
          ## Add observations to the map
          # add circles to map
          leafletProxy("obs_map", session) %>%
            clearMarkers() %>%
            addCircleMarkers(
              data = od,
              layerId = ~ od$guid,
              lat = ~ LATITUDE,
              lng = ~ LONGITUDE,
              radius = 5,
              # clusterOptions = markerClusterOptions(
              #   maxClusterRadius = 10,
              #   spiderfiyDistanceMultiplier = 2
              # ),
              opacity = 1,
              stroke = TRUE,
              color = ncba_white,
              weight = 0.9,
              fillColor = ~ breeding_category_pal(od$breeding_category),
              fillOpacity = 1,
              group = "SpeciesObservations",
              label = sprintf(
                "<strong>%s</strong><br/>%s<br/>%s<br/>%s",
                od$sei,
                od$OBSERVATION_DATE,
                od$breeding_category,
                od$breeding_code
              ) %>%
              lapply(htmltools::HTML)
            )
        } else {
          # no records found
          form_data$obs_data <- NULL

          # clear map
          leafletProxy("obs_map", session) %>%
            clearMarkers()

          # render text instead of boxplot
          output$boxplot_or_message <- renderUI(
            h3("No Data")
        )
        }
      }
    }
  )
  
  # listen for form_data changes
  observeEvent(
    form_data$observation,
    {
      if (is.null(form_data$observation)) {
        output$observation_data <- renderUI(HTML(""))

        # zoom map back out
        leafletProxy("obs_map", session) %>%
          setView(
            lat = nc_center_lat,
            lng = nc_center_lng,
            zoom = nc_center_zoom
          )

      } else {
        # retrieve obs_data
        obs_data <- retrieve_obs_data(form_data$observation)
        print(obs_data)

        # update observation data
        output$observation_data <- renderUI(
          {
            out_html <- with(
              obs_data,
              paste0(
                '<p><a href = "https://ebird.org/checklists/',
                sei, '" target="blank" title="', sei, 
                '">eBird Checklist</a> - ', ID_NCBA_BLOCK, '</p>
                <p>', LOCALITY, '</p>
                <h5>Date/Duration</h5>
                <p>', OBSERVATION_DATE, '<br/>',
                DURATION_MINUTES, ' minutes, ', EFFORT_DISTANCE_KM, ' km<br/>',
                PROTOCOL_TYPE, '</p>
                <h5>Observation Data</h5>
                <p>', NCBA_OBSERVER, '<br/>',
                BREEDING_CODE, ' (',BREEDING_CATEGORY, ')<br/>',
                ifelse(HAS_MEDIA == 1, "Has Media", ""),
                '</p><p>', SPECIES_COMMENTS, '</p>'
              )
            )

            HTML(out_html)
          }
        )

        # zoom map to obs
        latitude <- obs_data[1, "LATITUDE"]
        longitude <- obs_data[1, "LONGITUDE"]
        leafletProxy("obs_map", session) %>%
          setView(
            lat = latitude,
            lng = longitude,
            zoom = nc_obs_zoom
          ) %>%
          clearGroup("highlight") %>%
          addCircleMarkers(
            lat = latitude,
            lng = longitude,
            stroke = TRUE,
            color = "red",
            weight = 6,
            radius = 10,
            opacity = 1,
            group = "highlight"
          )
      }

    },
    ignoreNULL = FALSE
  )
}

shinyApp(ui, server)