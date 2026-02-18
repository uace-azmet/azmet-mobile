library(shiny)
library(bslib)
library(bsicons)
library(azmetr)

# azmet <- az_15min()

station_info
station_choices <- station_info$meta_station_id
names(station_choices) <- station_info$meta_station_name
ui <- page_fluid(
  actionButton(
    inputId = "open_picker",
    label = span(bs_icon("pin"), textOutput("selected_station"))
  ),
  value_box(
    "Current Temperature",
    value = "64ºF",
    showcase = bs_icon("thermometer")
  ),
  value_box(
    "Precipitation",
    value = '0"',
    showcase = bs_icon("cloud")
  )
)

server <- function(input, output, session) {
observeEvent(input$open_picker, {
  showModal(modalDialog(
    title = "Choose a station",
    selectInput("station", "Station", choices = station_choices)
  ))
})
output$selected_station <- renderText({
  names(station_choices[station_choices == input$station])
})
}

shinyApp(ui, server)