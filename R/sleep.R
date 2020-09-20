suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(here)
  library(CausalImpact)
  library(zoo)
  library(anytime)
})


#' Get Nightly Sleep Quality Data
#'
#' @param root (chr) root directory
#' @import readr
#'
get_sleep_data <- function(root, rolling_average_window = 1) {
  readr::read_csv(
    file.path(root, "data", "sleep-scores.csv"),
    col_types = cols(dt = col_date(format = ""),
                     sleep_score = col_double())
  ) %>% 
  arrange(dt) %>%
  dplyr::mutate(sleep_score = rollmean(x=sleep_score, rolling_average_window, fill = NA))
}

#'
#'
#' @import readr
get_weather_data <- function(root, min_date, max_date){
  readr::read_csv(
    file.path(root, "data", "weather.csv"),
    col_types = cols(
      .default = col_double(),
      dt_iso = col_character(),
      city_name = col_character(),
      sea_level = col_logical(),
      grnd_level = col_logical(),
      weather_main = col_character(),
      weather_description = col_character(),
      weather_icon = col_character()
    ))%>%
    dplyr::mutate(dt = anytime::anydate(x = dt, tz = "EST")) %>%
    group_by(dt) %>%
    summarise(
      min_daily_temp     = min(temp_min, na.rm = TRUE),
      max_daily_temp     = max(temp_max, na.rm = TRUE),
      min_daily_humidity = min(humidity, na.rm = TRUE),
      max_daily_humidity = max(humidity, na.rm = TRUE),
      .groups="drop"
    ) %>% dplyr::filter(
      dt >= min_date 
    ) %>% dplyr::filter(
      dt <= max_date
    )
}


#' Get combined data for causal analysis on sleep data
#'
#' @import dplyr
#' @import zoo
#'
get_sleep_data_for_causal_impact <- function(root, moving_avg_days){
  sleep <- get_sleep_data(root, moving_avg_days)
  
  min_date <- min(sleep$dt)
  max_date <- max(sleep$dt)
  
  weather  <- get_weather_data(root, min_date, max_date)
  
  joined <- sleep %>%
    dplyr::inner_join(weather, by = "dt") %>%
    dplyr::select(c("dt", "sleep_score", names(weather))) %>% 
    tidyr::drop_na()
  
  # select required elements
  sleep_score  <- joined[["sleep_score"]]
  covariate_ts <- dplyr::select(joined, -c("sleep_score", "dt"))
  dates        <- as.Date(lubridate::date(joined[["dt"]]))
  
  zoo_sleep <- zoo::zoo(
    cbind(sleep_score, covariate_ts),
    dates
  )
  return(zoo_sleep)
}
