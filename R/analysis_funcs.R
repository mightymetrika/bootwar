#' Analyze Game Results and Determine Winner
#'
#' This function analyzes the results of the game using both nonparametric
#' bootstrap with pooled resampling and classical t-tests. It then determines
#' the winner based on the bootstrap results and effect size.
#'
#' @param plyr_vv A numeric vector storing the values of the cards dealt to the
#'    player.
#' @param comp_vv A numeric vector storing the values of the cards dealt to the
#'    computer.
#' @param mode A character string indicating the type of test. Valid options are
#'    "t" for independent t-test
#' and "pt" for paired t-test. Default is "t".
#' @param conf.level A confidence level for \code{npboottprm::nonparboot},
#'    \code{stats::t.test}. The confidence level is also used to set the alpha
#'    level to alpha = 1 - conf.level
#' @param ... Additional arguments passed to the \code{npboottprm::nonparboot}
#'    function.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{bootstrap_results}: A list containing results from the bootstrap test.
#'   \item \code{classical_results}: A list containing results from the classical t-test.
#'   \item \code{winner}: A character string indicating the winner ("Player Wins", "Computer Wins", or "Draw").
#' }
#'
#' @examples
#' # Analyze a sample game
#' plyr_values <- c(4, 3, 2, 1)
#' comp_values <- c(1, 2, 3, 4)
#' game_results <- analyze_game(plyr_values, comp_values, nboot = 1000,
#'                              mode = "t", seed = 150)
#'
#' @export
analyze_game <- function(plyr_vv, comp_vv, mode = "t", conf.level = 0.95, ...) {
  # Validate mode
  if (!mode %in% c("t", "pt")) {
    stop("Invalid mode. Please use 't' or 'pt'.")
  }

  # Convert vectors to a data frame based on mode
  if (mode == "t") {
    game_data <- data.frame(
      x = c(plyr_vv, comp_vv),
      grp = c(rep("plyr", length(plyr_vv)), rep("comp", length(comp_vv)))
    )
    # Get bootstrap results
    boot_results <- npboottprm::nonparboot(data = game_data, x = "x", grp = "grp",
                                           test = mode, conf.level = conf.level, ...)
    # Get classical t-test results
    classical_results <- stats::t.test(x ~ grp, data = game_data,
                                       conf.level = conf.level)
  } else { # mode == "pt"
    if (length(comp_vv) != length(plyr_vv)) {
      stop("For paired t-test, player and comp values should have the same length.")
    }
    game_data <- data.frame(
      x = plyr_vv,
      y = comp_vv
    )
    # Get bootstrap results
    boot_results <- npboottprm::nonparboot(data = game_data, x = "x", y = "y",
                                           test = mode, conf.level = conf.level, ...)
    # Get classical t-test results
    classical_results <- stats::t.test(game_data$x, game_data$y, paired = TRUE,
                                       conf.level = conf.level)
  }

  # Determine winner
  winner <- NULL
  if (boot_results$p.value <= 1 - conf.level) {
    if (boot_results$effect.size > 0) { # Assuming positive effect size means player wins
      winner <- "Player Wins"
    } else {
      winner <- "Computer Wins"
    }
  } else {
    winner <- "Draw"
  }

  # Return results as a list
  result <- list(
    bootstrap_results = boot_results,
    classical_results = classical_results,
    winner = winner
  )

  return(result)
}
