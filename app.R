# NC Bird Atlas Data Review Shiny App - Public Version
# v1
# 10/15/2025
# Scott K. Anderson
# https://github.com/nmtarr/NCBA/ncba_data_review

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
  useShinyjs(),
  theme = bs_theme(version = 5, "bslib_spacer" = "0.1rem"),
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
        div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
        shinyauthr::loginUI(id = "login"),
      ),
      card(
        selectizeInput(
          "spp_select",
          h4("Select Species"),
          choices = species_list, options = list(
            placeholder = "Select species",
            onInitialize = I('function() {this.setValue(""); }')
          )
        ),
        uiOutput("spp_status"),
        checkboxInput(
          "check_flagged",
          "Flagged Spp/Code",
          value = TRUE
        ),
        checkboxInput(
          "check_safe_dates",
          "Outside Safe Dates",
          value = TRUE
        ),
        checkboxInput(
          "check_region",
          "Outside Region",
          value = TRUE
        ),
        checkboxInput(
          "check_unreviewed",
          "Unreviewed Only",
          value = TRUE
        ),
        selectizeInput(
          "select_block",
          h4("Select Block"),
          choices = block_list, options = list(
            placeholder = "Select block",
            onInitialize = I('function() {this.setValue(""); }')
          )
        ),
        selectizeInput(
          "select_code",
          h4("Select Breeding Code"),
          choices = block_list, options = list(
            placeholder = "Select block",
            onInitialize = I('function() {this.setValue(""); }')
          )
        ),
      ),
      card(
        card_header("Observation Detail"),
        uiOutput("observation_data")
      )
    ),
    mainPanel(
      class = "col-sm-10",
      layout_columns(
        card(
          card_header(uiOutput("obs_list_header")),
          class = "mt-2",
          card(
            DTOutput(
              "selected_obs_table"
            )
          ),
          actionButton("clear_observation_list", "Clear List"),
          card(
            card_header(uiOutput(outputId = "review_card_header")),
            class = "smaller-text",
            uiOutput("num_selected"),
            selectizeInput(
              "breeding_code_select",
              label = labelMandatory("New Breeding Code"),
              choices = code_levels_list,
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
            disabled(actionButton("update_record", "Update"))
          )
      ),
      card(
        class = "mt-2",
        girafeOutput(outputId = "boxplot"),
        leafletOutput(outputId = "obs_map")
      ),
      col_widths = c(5, 7)
    )
  )
  )
)

############################################################################
## SERVER

server <- function(input, output, session) {

  #######################################################################
  ## LOGIN FUNCTIONS
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
      if (credentials()$user_auth) {
        user_info <- credentials()$info

        review_card_text <- paste("Review Status -", user_info$name)

        ## enable update button
        # shinyjs::enable("update_record")

      } else {
        ## remove labels
        review_card_text <- paste("Review Status - please login")


        ## disable update button
        shinyjs::disable("update_record")
      }

      output$review_card_header <- renderUI({HTML(review_card_text)})
    }
  )

  # listen for changes to input criteria
  observeEvent(
    c(
      input$check_flagged,
      input$check_safe_dates,
      input$check_region,
      input$check_unreviewed,
      input$select_block,
      input$select_code
    ),

  
  )

  # store current info
  form_data <- reactiveValues(
    species_status = NULL, #breeding, etc...
    species_safe_date_start_jd = NULL, #start safe date as julian day
    species_safe_date_end_jd = NULL, #end safe date as julian day
    all_obs_data = NULL, # df of all obs data for selected species
    selected_obs = NULL, # df of selected observation records - in table
    selected_obs_ids = NULL, # list of selected observation ids - in table

    # highlight_ids = NULL # list of highlighted records to apply review results
    # obs_record = NULL, # selected observation record (one row df)
    # checklist = NULL, # selected checklist
    # checklist_url = NULL, # selected checklist eBird URL
    # observation = NULL, # selected observation GUID
  )

  #######################################################################
  ## FUNCTIONS

  clear_all_obs_data <- function() {
    form_data$all_obs_data <- NULL
    form_data$selected_obs <- NULL
    form_data$selected_obs_ids <- NULL

    # clear table
    output$selected_obs_table <- NULL

    # clear map
    leafletProxy("obs_map", session) %>%
      clearMarkers()

    # clear boxplot
    output$boxplot <- NULL
  }

  clear_selected_obs <- function() {
    # clear records in the table
    form_data$selected_obs <- NULL
    form_data$selected_obs_ids <- NULL
    # form_data$highlight_ids <- NULL

    # clear highlighted points in boxplot
    # updateGirafe(session, "boxplot", selected = character(0))
    session$sendCustomMessage(
      type = "boxplot_selected",
      message = character(0)
    )

    # clear table
    output$selected_obs_table <- NULL

    # clear map
    leafletProxy("obs_map", session) %>%
      clearGroup("selected") %>%
      clearGroup("highlighted")
  }
  clear_highlight_list <- function() {
    # clear highlighted records in the table
    # form_data$highlight_ids <- NULL
    # clear map
    leafletProxy("obs_map", session) %>%
      clearGroup("highlighted")
  }

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
  # observeEvent(
  #   input$obs_clicked,
  #   {form_data$observation <- input$obs_clicked}
  # )
  # ## listen for click on map
  # observeEvent(
  #   input$obs_map_marker_click,
  #   {
  #     form_data$observation <- input$obs_map_marker_click$id
  #   }
  # )

  # # Display
  # # * highlight flagged observations in map and boxplot: 
  # #   * F or U species-code combos,
  # #   * outside safe dates,
  # #   * outside breedingrange
  # # * highlight blocks with history of breeding
  # # Process
  # # * highlight observations from either boxplot or map
  # #   * obs show in the observations list
  # # * select obs to change - will show in review results list
  # # * complete form, update button
  # # * display reviewed records in the boxplot/map

  # check species NC Status
  # observeEvent(
  #   form_data$species_status,
  #   input$spp_status <- renderUI({
  #     if (is.null(form_data$species_status)) {
  #       HTML("")
  #     } else {
  #       HTML(paste0(
  #         '<span class="diminish-text">',
  #         form_data$species_status,
  #         '</span>'
  #         )
  #       )
  #     }     
  #   })
  # )

  # listen for changes to spp_select
  observeEvent(
    input$spp_select,
    {
      req(credentials()$user_auth) # for production
      req(input$spp_select)
      form_data$observation <- NULL

      ## No Selection
      if (is.null(input$spp_select) | input$spp_select == "") {
        clear_all_obs_data()
        block_list <- c("")

      } else {

        # species is selected, see if there is data
        form_data$all_obs_data <- get_observations(input$spp_select)

        # populate safe dates in form data
        spp_sd_record <- species_codes[
          species_codes$SPECIES == input$spp_select,
        ]
        spp_sd_record <- spp_sd_record[1,]

        form_data$species_safe_date_start_jd <-
          spp_sd_record$SAFE_DATE_START_JD
        form_data$species_safe_date_end_jd <-
          spp_sd_record$SAFE_DATE_END_JD

        # form_data$species_status <-
        #   spp_sd_record$NC_STATUS

        # populate block list
        block_list <- unique(spp_sd_record[, "ID_NCBA_BLOCK"])
        block_list <- c("", block_list)


        # plot data
        if (nrow(form_data$all_obs_data) > 0) {
          od <- form_data$all_obs_data
          od$BREEDING_CODE <- factor(
            od$BREEDING_CODE,
            levels = code_levels_boxplot
          )

          ###############################
          ## Add observations to the boxplot
          output$boxplot <- renderGirafe(
            {


              # od <- form_data$all_obs_data

              gg_point <- ggplot(
                data = od,
                aes(
                  x = JULIAN_DAY,
                  y = BREEDING_CODE,
                )
              ) +
                geom_vline(
                  xintercept = c(
                    form_data$species_safe_date_start_jd,
                    form_data$species_safe_date_end_jd
                  ),
                  linetype = "dashed",
                  color = "gray",
                  size = 1,
                  aes(tooltip = "Safe Dates")
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
              color = ncba_blue,
              weight = 0.9,
              # fillColor = ncba_blue,
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
          block_list <- c("")
          # clear map
          leafletProxy("obs_map", session) %>%
            clearMarkers()

        }
      }
      
      # update block list select
      updateSelectizeInput(
          session = session,
          inputId = "select_block",
          choices = block_list,
          selected = ""
      )
    }
  )

  #########################################################################
  ## Selected Observations Events

  observeEvent(
    input$boxplot_selected,
    {
      # print("boxplot_selected change event")
      # print(input$boxplot_selected)

      if (is.null(input$boxplot_selected)) {
        clear_selected_obs()
      } else{
        form_data$selected_obs_ids <- input$boxplot_selected
      }
    }
  )

  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ## ADD Map highlighting function

  observeEvent(
    form_data$selected_obs_ids,
    {
      # runs when selected obs is changed
      if (length(form_data$selected_obs_ids) == 0) {

        form_data$selected_obs <- NULL

        output$selected_obs_table <- NULL

      } else {
        # get obs record info
        form_data$selected_obs <- get_obs_records(
          form_data$selected_obs_ids,
          input$spp_select
        )

        # populate observations list
        selected_obs_list <- form_data$selected_obs %>%
          mutate(
            BREEDING_CODE_CATEGORY = paste0(
              BREEDING_CODE, " (", BREEDING_CATEGORY, ")"
            )
          ) %>%
          select(
            "SEI",
            "OBSERVATION_DATE",
            "BREEDING_CODE_CATEGORY",
            "ECOREGION",
            "SPECIES_COMMENTS",
            "JULIAN_DAY",
            "EBIRD_LINK",
            "SUITABILITY",
            "GUID",
            "LATITUDE",
            "LONGITUDE"
          )

        output$selected_obs_table <- renderDT({

          datatable(
            selected_obs_list,
            selection = "single",
            colnames = c(
              "", "Checklist", "Date", "Code", "Region", "Comments"
            ),
            options = list(
              dom = "t",
              columnDefs = list(
                list(
                  className = "dt-center",
                  targets = c(1, 2, 3, 4)
                  ),
                list(
                  className = "dt-left",
                  targets = c(5)
                  ),
                list(
                  width = "100px",
                  targets = c(5)
                  ),
                list(
                  visible = TRUE,
                  targets = c(1, 2, 3, 4, 5)
                ),
                list(
                  visible = FALSE,
                  targets = "_all"
                )
              ),
              list(
                rowCallback = JS(
                  'function(row, data) {',
                  'var full_text = "SEI:" + data[0] + ",<br/>" + data[1]',
                  '$("td", row).attr("title", full_text);',
                  '}'
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
            columns = "BREEDING_CODE_CATEGORY",
            valueColumns = "SUITABILITY",
            backgroundColor = styleEqual(
              c("", "S", "U", "F"),
              c("green", "green", "red", "yellow")
            ),
            color = styleEqual(
              c("", "S", "U", "F"),
              c("white", "white", "black", "black")
            )
          ) %>%
          formatStyle(
            columns = "SPECIES_COMMENTS",
            fontSize = "0.8rem",
          )


        }) # end of output code for table

        ## map points in table
        # Get the bounding box of the points
        min_lat <- min(selected_obs_list$LATITUDE)
        max_lat <- max(selected_obs_list$LATITUDE)
        min_lng <- min(selected_obs_list$LONGITUDE)
        max_lng <- max(selected_obs_list$LONGITUDE)

        # zoom map to obs
        leafletProxy("obs_map", session) %>%
          clearGroup("selected") %>%
          addCircleMarkers(
            data = selected_obs_list,
            lat = ~ LATITUDE,
            lng = ~ LONGITUDE,
            stroke = TRUE,
            color = ncba_blue,
            weight = 4,
            radius = 10,
            opacity = 1,
            group = "selected"
          ) %>%
          fitBounds(min_lng, min_lat, max_lng, max_lat)

      } # end of if else
    } # end of function portion of observeEvent
  )

  observeEvent(
    input$clear_observation_list,
    {clear_selected_obs()}
  )

  observeEvent(
    input$clear_highlight_list,
    {clear_highlight_list()}
  )


  #########################################################################
  ## Display selected Observation Details
  # listen for form_data changes
  observeEvent(
    input$selected_obs_table_rows_selected,
    {
      req(credentials()$user_auth) # for production

      if (is.null(input$selected_obs_table_rows_selected)) {
        output$observation_data <- renderUI(HTML(""))

      } else {
        # retrieve obs_record
        highlight_row <- input$selected_obs_table_rows_selected

        obs_record <- form_data$selected_obs[highlight_row, ]
        # print(obs_record)

        # update review results card
        # output$review_record_id <- renderUI(
        #   {
        #     record_id <- paste0(
        #       '<a href="', form_data$checklist_url, '" target = "_blank">', 
        #       form_data$checklist, ' (', form_data$observation, ')</a>'
        #     )
        #     HTML(record_id)
        #   }
        # )

        # update observation data
        output$observation_data <- renderUI(
          {
            cn <- names(obs_record)
            # print(cn)

            out_html <- with(
              obs_record,
              paste0(
                '<p class="obs-detail">', NCBA_OBSERVER, '<br/>',
                BREEDING_CODE, ' (',BREEDING_CATEGORY, ')</p>',
                '<p class="obs-detail"><a href = "', EBIRD_LINK,
                '" target="blank" title="', SEI,
                '">', ID_NCBA_BLOCK, '</a> :', 
                ifelse("LOCALITY" %in% cn, LOCALITY,""), '</p>
                <p class="obs-detail">', OBSERVATION_DATE, '<br/>',
                DURATION_MINUTES, ' minutes, ', EFFORT_DISTANCE_KM, ' km<br/>',
                PROTOCOL_TYPE, '</p><p class="obs-detail">',
                ifelse(
                  "HAS_MEDIA" %in% cn,
                  ifelse(HAS_MEDIA == 1, "Has Media", "No Media"),
                  "No Known Media"
                ),
                '</p><p class = "diminish-text obs-detail">',
                SPECIES_COMMENTS, '</p>'
              )
            )

            HTML(out_html)
          }
        )
      }
    }
  )

  #       # zoom map to obs
  #       latitude <- obs_record[1, "LATITUDE"]
  #       longitude <- obs_record[1, "LONGITUDE"]
  #       leafletProxy("obs_map", session) %>%
  #         setView(
  #           lat = latitude,
  #           lng = longitude,
  #           zoom = nc_obs_zoom
  #         ) %>%
  #         clearGroup("highlight") %>%
  #         addCircleMarkers(
  #           lat = latitude,
  #           lng = longitude,
  #           stroke = TRUE,
  #           color = "red",
  #           weight = 6,
  #           radius = 10,
  #           opacity = 1,
  #           group = "highlight"
  #         )
  #     }
  #   },
  #   ignoreNULL = FALSE
  # )
  
  #########################################################################
  ## Record Update actions

  ## RECORDS TO CHANGE LIST
  # include hyperlinks to checklists, hover text for details
  # observeEvent(
  #   input$selected_obs_table_rows_selected,
  #   {
  #     highlight_rows <- input$selected_obs_table_rows_selected

  #     if (is.null(highlight_rows)) {
  #       output$num_selected <- renderUI(
  #         HTML("0 selected")
  #       )

  #     } else {

  #       output$num_selected <- renderUI(
  #         HTML(paste(
  #           length(highlight_rows),
  #           "selected"
  #           )
  #         )
  #       )

  #       # populate highlight ids

  #       # print(form_data$selected_obs_ids[highlight_rows])

  #       form_data$highlight_ids <-
  #         form_data$selected_obs_ids[highlight_rows]

  #     }
  #   }
  # )
  # add code here to vet record before uploading to mongodb

  observeEvent(
    input$update_record,
    {
      req(credentials()$user_auth)
      error_text <- ""
      success <- TRUE

      # add logic here to allow no BBA Reason if New Breeding Code == "No Change"

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
        ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ## MODIFY TO CHANGE ALL form_data$to_change records
        # get highlighted ids
        # print(form_data$highlight_ids)

        # response <- update_review_record(
        #   form_data$selected_obs_ids,
        #   update_code
        # )


        ## CLEAR FORM
        ## CLEAR TABLE
      }

      # output$review_card_header <- uiOutput({
      #   HTML(paste0("Review Results", error_text))
      # })
      
    }
  )


}

shinyApp(ui, server)