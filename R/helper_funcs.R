#' Process Seed Input for the Bootwar Game
#'
#' This function processes the seed input for the Bootwar game.
#' It converts the seed to an integer if it's not NA. If the seed is NA, it
#' returns NULL.
#'
#' @param seed_input Numeric or NA. The input seed value from the Shiny app.
#'
#' @return Integer or NULL. If the input is not NA, it returns the integer value
#' of the seed. Otherwise, it returns NULL.
#'
#' @keywords internal
process_seed <- function(seed_input) {
  if (!is.na(seed_input)) {
    as.integer(seed_input)
  } else {
    NULL
  }
}
