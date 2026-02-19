# pak::pak("ColinFay/geoloc")
library(geoloc)
library(dplyr)

#' `selectInput()` with geolocation button (UI module)
#'
#' A select drop-down input with a location button next to it to choose the
#' input closest to the user (if they have location services allowed).
#'
#' @param id
#' @param label passed to [shiny::selectInput()].
#' @param locations_df a data frame with the columns `choice`, `value`, `lat`,
#'   and `lon`.
#' @param selected passed to [shiny::selectInput()]; once of the `value`
#'   options in `locations_df`.
location_select_ui <- function(
  id,
  label = "Select Location:",
  locations_df,
  selected = NULL
) {
  ns <- NS(id)

  # Create named vector for choices
  # The value will be "lat,lon" and the name will be the choice
  choices <- setNames(
    # paste0(locations_df$lat, ",", locations_df$lon),
    locations_df$value,
    locations_df$choice
  )

  div(
    style = "
      display: flex;
      justify-content: flex-start;
      align-items: center;
      gap: 5px;
    ",
    div(
      # style = "flex-grow: 1;", #let the drop-down grow to take up the whole space
      style = "flex-basis: 20rem;", #let the drop-down grow to a max of 20rem
      selectInput(
        ns("select"),
        label,
        choices = choices,
        selected = selected
      )
    ),
    div(
      style = "padding-top: 15px;", # centers the button with the drop-down (roughly)
      geoloc::button_geoloc(
        ns("loc"),
        icon("location-dot"),
        class = "btn btn-sm"
      )
    )
  )
}

#' `inputSelect()` with geolocation button (server module)
#'
#' @param id same ID as provided to `location_select_ui()`.
#' @param locations_df same data frame as provided to `locations_select_ui()`.
location_select_server <- function(id, locations_df) {
  moduleServer(id, function(input, output, session) {
    # Update choice when location is received
    observe({
      req(input$loc_lat, input$loc_lon)

      user_lat <- as.numeric(input$loc_lat)
      user_lon <- as.numeric(input$loc_lon)

      # Find nearest location using Haversine distance
      distances <- sapply(1:nrow(locations_df), function(i) {
        lat <- locations_df$lat[i]
        lon <- locations_df$lon[i]

        # Haversine formula
        dlat <- (lat - user_lat) * pi / 180
        dlon <- (lon - user_lon) * pi / 180
        a <- sin(dlat / 2)^2 +
          cos(user_lat * pi / 180) * cos(lat * pi / 180) * sin(dlon / 2)^2
        c <- 2 * atan2(sqrt(a), sqrt(1 - a))
        6371 * c # Distance in km
      })

      nearest_idx <- which.min(distances)
      nearest_location <- locations_df[nearest_idx, ]

      # Update the input
      updateSelectInput(
        session,
        "select",
        selected = nearest_location$value
      )
    }) |>
      # run when the location is updated (not just when the button is pressed)
      bindEvent(
        input$loc_lat,
        input$loc_lon,
        input$loc
      )

    # # Return the selected value as a reactive
    reactive(input$select)
  })
}
