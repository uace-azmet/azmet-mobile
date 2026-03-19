degrees_to_cardinal <- function(x) {
  case_when(
    x == 360 | x >= 0 & x < 45 ~ "North",
    x >= 45 & x < 90 ~ "North East",
    x >= 90 & x < 135 ~ "East",
    x >= 135 & x < 180 ~ "South East",
    x >= 180 & x < 225 ~ "South",
    x >= 225 & x < 270 ~ "South West",
    x >= 270 & x < 315 ~ "West",
    x >= 315 & x < 360 ~ "North West"
  )
}
