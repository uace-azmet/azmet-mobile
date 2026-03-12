library(shiny)
library(bslib)
library(bsicons)
library(thematic)
thematic_shiny()

library(azmetr)
library(dplyr)
library(ggplot2)
library(lubridate)


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
  # Title and location selector
  # div(
  #   class = "d-flex justify-content-between align-items-center",
  #   style = "background-color: white; padding: 1rem; border-radius: 0.25rem; box-shadow: 0 0.125rem 0.25rem rgba(0,0,0,0.075);",
  #   layout_columns(
  #     col_widths = c(7, 5),

  #     img(
  #       src = "https://azmet.arizona.edu/sites/default/files/AZMet_1.png",
  #       width = "50%"
  #     ),
  #     location_select_ui(
  #       "loc_module",
  #       "Select a station:",
  #       station_choices,
  #       selected = "az01"
  #     )
  #   )
  # ),
  img(src = "https://www.azmet.arizona.edu/sites/default/files/AZMet_1.png"),
  actionButton(
    inputId = "open",
    label = span(
      bs_icon("geo-alt"),
      textOutput("selected_station", container = span)
    ),
    class = "btn-outline-secondary btn-sm"
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
      div(
        class = "text-dark",
        style = "font-size: 0.9rem;",
        textOutput("temp_feels_like", inline = TRUE)
      ),
      conditionalPanel(
        condition = "input.temp_card_full_screen",
        plotOutput("temp_plot")
      )
    )
  ),
  verbatimTextOutput("input_text")
)

server <- function(input, output, session) {
  # Set a default station
  # TODO: use cookies for this
  station <- reactiveVal("az01")

  # Create modal to contain picker with location button
  observeEvent(input$open, {
    showModal(modalDialog(
      location_select_ui(
        "loc_module",
        "Select a station:",
        station_choices,
        selected = station()
      )
    ))
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

  # Temperature outputs (both regular and fullscreen)
  output$temp_current <- output$temp_current_full <- renderText({
    paste0(60, "°F")
  })

  output$temp_feels_like <- output$temp_feels_like_full <- renderText({
    paste0("Feels like ", 61, "°F")
  })

  output$temp_plot <- renderPlot({
    p <- ggplot(df, aes(x = hour, y = temp)) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 3) +
      scale_x_datetime(date_breaks = "hours", date_labels = "%R") +
      labs(x = "Time", y = "Temperature (°F)")
    plot(p)
  })
  output$input_text <- renderPrint({
    list(
      station_id_choice(),
      station(),
      input$loc_module,
      input$loc_module_select,
      input$loc_module_loc,
      input$loc_module_loc_lat,
      input$loc_module_loc_lon
    )
  })
}

shinyApp(ui = ui, server = server)