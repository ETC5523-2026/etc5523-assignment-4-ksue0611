#' Summarise Penguin Body Measurements
#'
#' Calculate average body mass and flipper length
#' for selected penguin species.
#'
#' @param data A data frame containing penguin measurements.
#' @param species A character vector of species to include.
#'
#' @return A data frame with the number of penguins,
#'   mean body mass and mean flipper length by species.
#'
#' @export
summarise_penguins <- function(data, species) {
  
  data |>
    dplyr::filter(.data$species %in% .env$species) |>
    dplyr::group_by(.data$species) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_mass = mean(.data$body_mass_g, na.rm = TRUE),
      mean_flipper = mean(
        .data$flipper_length_mm,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
}