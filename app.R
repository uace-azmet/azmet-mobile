library(shiny)
library(bslib)

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
    id = "temp_card",
    onclick = "openCard('temp')",
    style = "cursor: pointer;",
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
      # plotOutput("temperature_plot")
    )
  ),

  # Full-screen overlay for temperature card
  # div(
  card(
    id = "temp_fullscreen",
    class = "modal bg-light",
    style = "display: none;", # Don't show initially, gets changed by onclick event
    card_header(
      class = "bg-primary text-white justify-content-between align-items-center",
      div(
        style = "font-size: 1.3rem; font-weight: 500;",
        "🌡️ Temperature Details"
      ),
      actionButton(
        "close_temp",
        "×",
        class = "btn btn-link text-white",
        style = "font-size: 2rem; text-decoration: none; padding: 0; line-height: 1;",
        onclick = "closeCard('temp')"
      )
    ),
    card_body(
      # FIXME: don't know why the plot doesn't show up
      plotOutput("temperature_plot")
    )
  ),

  # JavaScript for card interactions
  tags$head(
    tags$script(HTML(
      "
      function openCard(cardType) {
        document.getElementById(cardType + '_fullscreen').style.display = 'block';
        document.body.style.overflow = 'hidden';
      }
      
      function closeCard(cardType) {
        document.getElementById(cardType + '_fullscreen').style.display = 'none';
        document.body.style.overflow = 'auto';
      }
    "
    ))
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

  output$temperature_plot <- renderPlot({
    p <- ggplot(df, aes(x = hour, y = temp)) +
      geom_line(color = "#0066CC", linewidth = 1.5) +
      geom_point(color = "#0066CC", size = 3) +
      scale_x_datetime(date_breaks = "hours", date_labels = "%R") +
      labs(x = "Time", y = "Temperature (°F)") +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
      )
    plot(p)
  })
}

shinyApp(ui = ui, server = server)