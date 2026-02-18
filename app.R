library(shiny)
library(bslib)
library(bsicons)
library(azmetr)
library(brand.yml)

# azmet <- az_15min()

#' TODO:
#' - Make default station selected on startup
#' - use location select input module
#' - add sparklines to value boxes and make them expandable
#' - PWA stuff
#' - cookies


station_info
station_choices <- station_info$meta_station_id
names(station_choices) <- station_info$meta_station_name
ui <- page_fluid(
  theme = bs_theme(brand = "_brand.yml"),
  actionButton(
    inputId = "open_picker",
    label = span(
      bs_icon("geo-alt"),
      textOutput("selected_station", container = span)
    ),
    class = "btn-outline-secondary btn-sm"
  ),
  value_box(
    "Current Temperature",
    value = "64ºF",
    showcase = bs_icon("thermometer"),
    theme = "primary"
  ),

  value_box(
    "Precipitation",
    value = '0"',
    showcase = bs_icon("cloud"),
    theme = "primary"
  )
)

server <- function(input, output, session) {
  observeEvent(input$open_picker, {
    showModal(modalDialog(
      title = "Choose a station",
      selectInput(
        "station",
        "Station",
        choices = station_choices,
        selected = "az01"
      )
    ))
  })

  output$selected_station <- renderText({
    names(station_choices[station_choices == input$station])
  })
}

shinyApp(ui, server)