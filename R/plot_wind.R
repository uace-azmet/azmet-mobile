# library(azmetr)
# library(ggplot2)
# library(dplyr)
# library(lubridate)
# data <- az_15min(start = now() - hours(3), end = now())

plot_wind <- function(data) {
  ggplot(
    data,
    aes(x = wind_vector_dir, y = 0, yend = wind_spd_mps, alpha = datetime)
  ) +
    geom_segment(arrow = arrow(length = unit(0.02, units = "npc"))) +
    scale_alpha_datetime(
      date_breaks = "15 min",
      date_labels = "%a %I:%M %p",
      timezone = "MST"
    ) +
    scale_x_continuous(
      breaks = c(
        "N" = 0,
        "NE" = 45,
        "E" = 90,
        "SE" = 135,
        "S" = 180,
        "SW" = 225,
        "W" = 270,
        "NW" = 315
      )
    ) +
    scale_y_continuous(labels = function(x) paste(x, "m/s")) +
    coord_radial(
      theta = "x",
      thetalim = c(0, 359),
      expand = FALSE,
      r.axis.inside = TRUE
    ) +
    # theme_minimal() +
    guides(
      alpha = guide_legend(
        NULL,
        reverse = TRUE,
        ncol = 2,
        position = "bottom"
      )
    ) +
    theme(axis.title = element_blank())
}
