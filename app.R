library(shiny)
library(bslib)
library(bsicons)
library(azmetr)
library(brand.yml)
library(dplyr)

# azmet <- az_15min()

#' TODO:
#' - Make default station selected on startup
#' - Use cookie to remember last station choice
#' - Get rid of "dismiss" button from modal and instead close upon choosing a
#'   station (or touching outside of modal)
#' - Maybe don't use a modal at all?  (put select input directly above value boxes)
#' - Add sparkline type plots to value boxes
#' - Make value boxes expandable with more detailed visualization
#' - Make into a PWA
#' - Add refresh button or swipe down to refresh data

# station_info
# station_choices <- station_info$meta_station_id
# names(station_choices) <- station_info$meta_station_name

station_choices <- azmetr::station_info |>
  select(
    choice = meta_station_name,
    value = meta_station_id,
    lat = latitude,
    lon = longitude
  ) |>
  filter(choice != "Test") |>
  arrange(choice)

ui <- page_fillable(
  theme = bs_theme(brand = "_brand.yml"),
  # prevent elements from taking up full space of screen vertically
  fillable_mobile = FALSE,

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
      location_select_ui(
        "loc_module",
        "Select a station:",
        station_choices,
        selected = "az01"
      )
    ))
  })

  selected_location <- location_select_server(
    "loc_module",
    station_choices
  )

  output$selected_station <- renderText({
    station_choices |> filter(value == selected_location()) |> pull(choice)
  })
}

shinyApp(ui, server)