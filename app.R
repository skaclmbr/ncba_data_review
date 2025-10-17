# NC Bird Atlas Data Review Shiny App - Public Version
# v1
# 10/15/2025
# Scott K. Anderson
# https://github.com/nmtarr/NCBA/ncba_data_review

if(!require(shiny)) install.packages(
  "shiny", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages(
  "shinythemes", repos = "http://cran.us.r-project.org")
if(!require(bslib)) install.packages(
  "bslib", repos = "http://cran.us.r-project.org")
if(!require(mongolite)) install.packages(
  "mongolite", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages(
  "ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages(
  "ggiraph", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages(
  "dplyr", repos = "http://cran.us.r-project.org")

# if(!require(shinyWidgets)) install.packages(
#   "shinyWidgets", repos = "http://cran.us.r-project.org")
# if(!require(tidyverse)) install.packages(
#   "tidyverse", repos = "http://cran.us.r-project.org")
# if(!require(dplyr)) install.packages(
#   "dplyr", repos = "http://cran.us.r-project.org")
# if(!require(leaflet)) install.packages(
#   "leaflet", repos = "http://cran.us.r-project.org")
# if(!require(leaflegend)) install.packages(
#   "leaflegend",
#   repos = "http://cran.us.r-project.org"
# )

source("utils.r")


# specificy breeding codes and preferred plotting order
# this vector will need updating if any new codes are introduced via "lump".
codelevels <- c("H", "S", "S7", "M", "T", "P", "C", "B", "CN", "NB", "A", "N",
                "DD", "ON", "NE", "FS", "CF", "NY", "FY", "FL", "PE", "UN",
                "F", "O", "NC", "Obs")

############################################################################
## UI

ui <- fluidPage(
  useBusyIndicators(),
  navbarPage(
    "NCBA Data Review",
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
          )
        ),
        mainPanel(
          card(
            card_header("Species Codes Timeline"),
            girafeOutput(outputId = "spp_histogram")
          )
        )
      )
    )

  )
)

server <- function(input, output, session) {

  form_data <- reactiveValues(
    species = "Black-throated Green Warbler",
    block = NULL
  )

  observeEvent(
    input$spp_select,
    {
      form_data$species <- input$spp_select
    }
  )

  output$spp_histogram <- renderGirafe(
    {
      pipeline <- paste0('[
        {
          "$match" : {
            "PROJECT_CODE" : "EBIRD_ATL_NC",
            "PRIORITY_BLOCK" : "1",
            "OBSERVATIONS.COMMON_NAME" : "', form_data$species, '"
          }
        },
        {
          "$unwind" : {
            "path" : "$OBSERVATIONS"
          }
        },
        {
          "$match" : {
            "OBSERVATIONS.BREEDING_CODE" : {"$ne": ""},
            "OBSERVATIONS.COMMON_NAME" : "', form_data$species, '"
          }
        },
        {
          "$project" : {
            "julian_day" : "$NCBA_JULIAN_DAY",
            "breeding_code" : "$OBSERVATIONS.BREEDING_CODE",
            "sei" : "$_id",
            "_id" : 0
          }
        }
      ]')

      spp_records <- aggregate_ebd_data(pipeline)
      spp_records <- spp_records %>%
        mutate(
          ebird_link = paste0("https://ebird.org/checklist/", sei),
          breeding_code = factor(
            spp_records$breeding_code, levels = codelevels, ordered = TRUE
          )
        )
      print(head(spp_records))

      # stem(c(155, 135, 122, 195, 230))
      # stem(spp_records$julian_day)
      gg_point <- ggplot(data = spp_records) +
        labs(y = "Breeding Code", x = spp_records$julian_day) +
        geom_boxplot(aes(x = spp_records$julian_day, y = spp_records$breeding_code)) +
        geom_point_interactive(
          aes(
            x = spp_records$julian_day,
            y = spp_records$breeding_code,
            tooltip = spp_records$sei,
            data_id = spp_records$sei,
            onclick = paste0('window.open("', spp_records$ebird_link, '", "_blank")')
          ),
          show.legend = FALSE,
          position = position_jitter(
            width = 0.2,
            height = 0.2
          )
        ) +
        xlim(0, 365) +
        theme_minimal() + labs(title = form_data$species)
      
      girafe(
        ggobj = gg_point,
        width_svg = 10,
        options = list(opts_sizing(rescale = TRUE))
      )
      # h <- ggplot(
      #   spp_records, aes(x = julian_day)
      # ) +
      # geom_dotplot_interactive(
      #   aes(
      #     tooltip = sei,
      #     onclick = paste0('window.open("', ebird_link, '", "_blank")')
      #   ),
      #   stackgroups = TRUE,
      #   binwidth = 1,
      #   method = "histodot"
      #   ) +
      # labs(title = "Stacked Dot Plot of Values", x = "Value", y = "Count")

      # x <- girafe(ggobj = h)

      # if(interactive()) {
      #   print(x)
      # }
    }
  )


}

shinyApp(ui, server)