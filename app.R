# NC Bird Atlas Data Review Shiny App - Public Version
# v1
# 10/15/2025
# Scott K. Anderson
# https://github.com/nmtarr/NCBA/ncba_data_review

if(!require(shiny)) install.packages(
  "shiny", repos = "http://cran.us.r-project.org")
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
if(!require(ggridges)) install.packages(
  "ggridges", repos = "http://cran.us.r-project.org")
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

# Helper function to create a mandatory label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

############################################################################
## UI

ui <- fluidPage(
  useBusyIndicators(),
  theme = bs_theme(version = 5),
  tags$head(
    includeCSS("styles.css"),
    tags$style(HTML(".mandatory_star { color: red; }"))
  ),
  windowTitle = "NCBA Data Review",

  sidebarLayout(
    sidebarPanel(
      width = 2,
      h3("NCBA Data Review", id = "sidebar_heading"),
      card(
        selectizeInput(
          "spp_select",
          h4("Select Species"),
          choices = species_list, options = list(
            placeholder = "Select species",
            onInitialize = I('function() {this.setValue("Alder Flycatcher"); }')
          )
        ),
        checkboxInput(
          inputId = "safe_dates_check",
          label = "Outside Safe Dates",
          value = FALSE
        ),
        checkboxInput(
          inputId = "region_check",
          label = "Outside Breeding Region",
          value = FALSE
        )
      ),
      card(
        card_header(uiOutput(outputId = "review_card_header")),
        selectizeInput(
          "breeding_code_select",
          label = labelMandatory("New Breeding Code"),
          choices = codelevels,
          multiple = FALSE,
          selected = NULL,
          options = list(placeholder = "Select Breeding Code")
        ),
        selectizeInput(
          "bba_reason_select",
          label = labelMandatory("BBA Reason"),
          choices = bba_review_reason_list,
          multiple = FALSE,
          selected = NULL,
          options = list(placeholder = "Select Reason")
        ),
        textAreaInput(
          "review_notes_text",
          "Notes",
        ),
        actionButton("update_record", "Update")
      ),
      div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
      shinyauthr::loginUI(id = "login")
    ),
    mainPanel(
      layout_columns(
        card(
          card_header("Observations List"),
          DTOutput("selected_obs"),
          actionButton("clear_observation_list", "Clear"),

          card(
            card_header("Selected Observation Detail"),
            htmlOutput(outputId = "observation_data")
          ),
        ),
        card(
          tabsetPanel(
            tabPanel(
              "Boxplot",
              girafeOutput(
                outputId = "boxplot"
              )
            ),
            tabPanel("Flagged", "")
          ),
          leafletOutput(outputId = "obs_map")),
        col_widths = c(4, 8)
      )
    )
  )
)

############################################################################
## SERVER

server <- function(input, output, session) {

  # call login module supplying data frame,
  # user and password cols and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base, # nolint: object_usage_linter.
    user_col = user, # nolint: object_usage_linter.
    pwd_col = password, # nolint: object_usage_linter.
    log_out = reactive(logout_init())
  )

  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )

  ## Functions when login changes
  observeEvent(
    credentials()$user_auth,
    {
      req(credentials()$user_auth)
      output$sidebar_heading <- renderUI(
        {
          HTML(paste("NCBA Data Review:", credentials()$info$name))
        }
      )

      output$review_card_header <- renderUI({
        HTML("Review Results")
      })
      ## INSERT CODE TO BLANK OUT GRAPHS?
    }
  )

  # store current info
  form_data <- reactiveValues(
    species = NULL, # selected species
    species_safe_date_start_jd = NULL,
    species_safe_date_end_jd = NULL,
    checklist = NULL, # selected checklist
    checklist_url = NULL, # selected checklist eBird URL
    observation = NULL, # selected observation GUID
    obs_record = NULL, # selected observation record (one row df)
    all_obs_data = NULL, # df of all obs data for selected species
    selected_obs = NULL, # df of selected observation records
    selected_obs_ids = NULL # list of selected observation ids

  )

  #######################################################################
  ## OBS MAP SETUP
  # display species map
  output$obs_map <- renderLeaflet({
    req(credentials()$user_auth) # for production
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
      req(credentials()$user_auth) # for production
      form_data$species <- input$spp_select
      form_data$observation <- NULL

      ## No Selection
      if (is.null(input$spp_select) | input$spp_select == "") {
        form_data$all_obs_data <- NULL

        # clear map
        leafletProxy("obs_map", session) %>%
          clearMarkers()

      } else {
        # species is selected, see if there is data
        form_data$all_obs_data <- get_observations(input$spp_select)

        # populate safe dates in form data
        spp_sd_record <- species_codes[
          species_codes$SPECIES == form_data$species,
        ]
        spp_sd_record <- spp_sd_record[1,]

        form_data$species_safe_date_start_jd <-
          spp_sd_record$SAFE_DATE_START_JD
        form_data$species_safe_date_end_jd <-
          spp_sd_record$SAFE_DATE_END_JD

        # plot data
        if (nrow(form_data$all_obs_data) > 0) {
          od <- form_data$all_obs_data

          ###############################
          ## Add observations to the boxplot
          output$boxplot <- renderGirafe(
            {
              req(input$spp_select)

              od <- form_data$all_obs_data

              gg_point <- ggplot(
                data = od,
                aes(
                  x = JULIAN_DAY,
                  y = BREEDING_CODE,
                )
              ) +
                labs(y = "Breeding Code", x = od$JULIAN_DAY) +
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

            }
          )

          ###############################
          ## Add observations to the map
          # add circles to map
          leafletProxy("obs_map", session) %>%
            clearMarkers() %>%
            addCircleMarkers(
              data = od,
              layerId = ~ od$GUID,
              lat = ~ LATITUDE,
              lng = ~ LONGITUDE,
              radius = 5,
              opacity = 1,
              stroke = TRUE,
              color = ncba_white,
              weight = 0.9,
              fillColor = ~ breeding_category_pal(od$BREEDING_CATEGORY),
              fillOpacity = 1,
              group = "SpeciesObservations",
              label = sprintf(
                "<strong>%s</strong><br/>%s<br/>%s<br/>%s",
                od$SEI,
                od$OBSERVATION_DATE,
                od$BREEDING_CATEGORY,
                od$BREEDING_CODE
              ) %>%
              lapply(htmltools::HTML)
            )
        } else {
          # no records found
          form_data$all_obs_data <- NULL

          # clear map
          leafletProxy("obs_map", session) %>%
            clearMarkers()

        }
      }
    }
  )

  #########################################################################
  ## Selected Observations Events

  observeEvent(
    input$boxplot_selected,
    form_data$selected_obs_ids <- input$boxplot_selected
  )

  observeEvent(
    form_data$selected_obs_ids,
    {
      # runs when selected obs is changed
      # get obs record info
      form_data$selected_obs <- get_obs_records(
        form_data$selected_obs_ids,
        form_data$species
      )

      selected_obs_list <- form_data$selected_obs %>%
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

      output$selected_obs <- renderDT({

        datatable(
          selected_obs_list,
          colnames = c(
            "","Checklist", "Date", "Code",
            "Category", "Status", "Ecoregion"
          ),
          options = list(
            dom = "t",
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
            c(
              form_data$species_safe_date_start_jd,
              form_data$species_safe_date_end_jd
            ),
            c("red", "green", "red")
          ),
          color = styleInterval(
            c(
              form_data$species_safe_date_start_jd,
              form_data$species_safe_date_end_jd
            ),
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
        }
      )

    }
  )

  observeEvent(
    input$clear_observation_list,
    {
      form_data$selected_obs <- NULL
      form_data$selected <- NULL
    }
  )


  #########################################################################
  ## Display selected Observation Details
  # listen for form_data changes
  observeEvent(
    form_data$observation,
    {
      req(credentials()$user_auth) # for production

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
        # retrieve obs_record
        obs_record <- get_obs_record(form_data$observation)
        form_data$checklist <- obs_record$SEI
        form_data$checklist_url <- paste0(
          "https://ebird.org/checklist/", obs_record$SEI
          )
        form_data$obs_record <- obs_record

        # update review results card
        output$review_record_id <- renderUI(
          {
            record_id <- paste0(
              '<a href="', form_data$checklist_url, '" target = "_blank">', 
              form_data$checklist, ' (', form_data$observation, ')</a>'
            )
            HTML(record_id)
          }
        )

        # update observation data
        output$observation_data <- renderUI(
          {
            cn <- names(obs_record)

            out_html <- with(
              obs_record,
              paste0(
                '<p><a href = "https://ebird.org/checklist/',
                SEI, '" target="blank" title="', SEI, 
                '">eBird Checklist</a> - ', ID_NCBA_BLOCK, '</p>
                <p>', ifelse("LOCALITY" %in% cn, LOCALITY,""), '</p>
                <h5>Date/Duration</h5>
                <p>', OBSERVATION_DATE, '<br/>',
                DURATION_MINUTES, ' minutes, ', EFFORT_DISTANCE_KM, ' km<br/>',
                PROTOCOL_TYPE, '</p>
                <h5>Observation Data</h5>
                <p>', NCBA_OBSERVER, '<br/>',
                BREEDING_CODE, ' (',BREEDING_CATEGORY, ')<br/>',
                ifelse(
                  "HAS_MEDIA" %in% cn,
                  ifelse(HAS_MEDIA == 1, "Has Media", ""),
                  ""
                ),
                '</p><p>', SPECIES_COMMENTS, '</p>'
              )
            )

            HTML(out_html)
          }
        )

        # zoom map to obs
        latitude <- obs_record[1, "LATITUDE"]
        longitude <- obs_record[1, "LONGITUDE"]
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
  
  #########################################################################
  ## Record Update actions
  
  # add code here to vet record before uploading to mongodb

  observeEvent(
    input$update_record,
    {
      req(credentials()$user_auth)
      error_text <- ""
      success <- TRUE


      # success <- FALSE
      # print(input$breeding_code_select)
      # print(form_data$obs_record$BREEDING_CODE)
      # # error checking for data entry form
      # if (input$breeding_code_select == NULL) {
      #   error_text <- paste0(
      #     ' <span class = "red-text">Please select a different breeding code.'
      #   )
      # } else if (input$breeding_code_select == form_data$obs_record$BREEDING_CODE) {
      #   error_text <- paste0(
      #     ' <span class = "red-text">Please select a different breeding code.'
      #   )
      # } else if (!(credentials()$user_auth)) {
      #   error_text <- paste0(
      #     ' <span class = "red-text">Please sign in.'
      #   )
      # } else {
      #   success <- TRUE
      # }

      if (success) {
        update_code <- paste0(
          '{
            "$set" : {
              "OBSERVATIONS.$[elem].NCBA_REVIEW" : {
                "REVIEWER" : "', credentials()$info$name, '",
                "REVIEW_DATE_TIME" : "', date(), '",
                "BREEDING_CODE" : "', input$breeding_code_select, '",
                "BREEDING_CATEGORY" : "',
                get_breeding_category(input$breeding_code_select),
                '", "BBA_REASON" : "', input$bba_reason_select, '",
                "NOTES" : "', input$review_notes_text, '"
              }
            }
          }'
        )
        response <- update_review_record(form_data$observation, update_code)
      }

      # output$review_card_header <- uiOutput({
      #   HTML(paste0("Review Results", error_text))
      # })
      
    }
  )


}

shinyApp(ui, server)