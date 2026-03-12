library(shiny)
library(bslib)
library(bsicons)
library(thematic)
thematic_shiny()

library(azmetr)
library(dplyr)
library(ggplot2)
library(lubridate)


# For now just get data for all sites on app load
data <- az_15min(start = now() - hours(6), end = now())

df <- tibble(
  hour = floor_date(now(), "hour") - hours(5:0),
  temp = 60 + runif(6, -5, 5)
)

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
  theme = bs_theme(),
  padding = "10px",
  # prevent elements from taking up full space of screen vertically
  fillable_mobile = FALSE,
  fillable = FALSE,
  # Logo
  img(
    src = "https://www.azmet.arizona.edu/sites/default/files/AZMet_1.png",
    width = "300px"
  ),
  # TODO: maybe location selector goes at the bottom of the screen?
  # Location selector
  actionButton(
    inputId = "open",
    label = span(
      bs_icon("geo-alt"),
      textOutput("selected_station", container = span)
    ),
    class = "btn-primary btn-m"
  ),
  # Temperature card
  card(
    full_screen = TRUE,
    id = "temp_card",
    card_header(
      class = "bg-primary text-white",
      div(
        style = "font-size: 1.1rem; font-weight: 500;",
        "🌡️ Temperature"
      )
    ),
    card_body(
      class = "bg-light text-center p-2",
      div(
        style = "font-size: 3rem; font-weight: 300;",
        textOutput("temp_current", inline = TRUE)
      ),
      conditionalPanel(
        condition = "input.temp_card_full_screen",
        plotOutput("temp_plot")
      )
    )
  )
  # # Temperature as value box
  # value_box(
  #   id = "temp_card",
  #   title = "Current Temperature",
  #   value = textOutput("temp_current", inline = TRUE),
  #   showcase = bs_icon("thermometer"),
  #   showcase_layout = showcase_left_center(),
  #   conditionalPanel(
  #     condition = "input.temp_card_full_screen",
  #     plotOutput("temp_plot")
  #   ),
  #   theme = "primary",
  #   full_screen = TRUE
  # )
)

server <- function(input, output, session) {
  # Set a default station
  # TODO: use cookies for this
  station <- reactiveVal("az01")

  # Create modal to contain picker with location button
  observeEvent(input$open, {
    showModal(
      # TODO: it would be nice if making a selection closed the modal
      modalDialog(
        location_select_ui(
          "loc_module",
          "Select a station:",
          station_choices,
          selected = station()
        ),
        footer = NULL,
        easyClose = TRUE
      )
    )
  })

  # Get results of location selection
  station_id_choice <- location_select_server(
    "loc_module",
    station_choices
  )

  # When a choice is made, update the default station
  observeEvent(station_id_choice(), {
    station(isolate(station_id_choice()))
  })

  # Print the station name for the modal button.
  output$selected_station <- renderText({
    station_choices |> filter(value == station()) |> pull(choice)
  })

  station_data <- reactive({
    data |> filter(meta_station_id == station())
  })

  # Temperature outputs (both regular and fullscreen)
  output$temp_current <- renderText({
    paste0(station_data() |> slice_tail(n = 1) |> pull(temp_airC), "°C")
  })

  output$temp_plot <- renderPlot({
    p <- ggplot(station_data(), aes(x = datetime, y = temp_airC)) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 3) +
      scale_y_continuous(labels = \(x) paste(x, "ºC")) +
      scale_x_datetime(date_breaks = "hours", date_labels = "%I:%M %p") +
      theme(axis.title = element_blank())
    plot(p)
  })
}

shinyApp(ui = ui, server = server)