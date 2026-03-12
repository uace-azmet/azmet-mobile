library(shiny)
library(bslib)
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
  div(
    class = "d-flex justify-content-between align-items-center",
    style = "background-color: white; padding: 1rem; border-radius: 0.25rem; box-shadow: 0 0.125rem 0.25rem rgba(0,0,0,0.075);",
    layout_columns(
      col_widths = c(7, 5),

      img(
        src = "https://azmet.arizona.edu/sites/default/files/AZMet_1.png",
        width = "50%"
      ),
      location_select_ui(
        "loc_module",
        "Select a station:",
        station_choices,
        selected = "az01"
      )
    )
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
  )
)

server <- function(input, output, session) {
  station_id_choice <- location_select_server(
    "loc_module",
    station_choices
  )

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
  output$text <- renderText({
    "hello!"
  })
}

shinyApp(ui = ui, server = server)