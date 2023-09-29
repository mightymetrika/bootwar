#' Calculate Scores and Effect Size
#'
#' This function computes the sum and mean of the player's and computer's values
#' and calculates the effect size based on the given mode (`t` or `pt`).
#'
#' @param player_values A numeric vector representing the values of the player's
#'    cards.
#' @param comp_values A numeric vector representing the values of the computer's
#'    cards.
#' @param mode A character string representing the mode of the game, either 't'
#'    for independent t-test or 'pt' for paired t-test.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{player_sum}: Sum of player's values.
#'   \item \code{player_mean}: Mean of player's values.
#'   \item \code{comp_sum}: Sum of computer's values.
#'   \item \code{comp_mean}: Mean of computer's values.
#'   \item \code{effect_size}: Calculated effect size based on the given mode.
#' }
#'
#' @examples
#' # Calculate scores for a simple game
#' player_vals <- c(2.5, 3.0, 4.5)
#' comp_vals <- c(3.5, 2.0, 4.0)
#' scores <- score_keeper(player_vals, comp_vals, mode = "t")
#'
#' @export
score_keeper <- function(player_values, comp_values, mode) {
  # Check for valid mode
  if (!mode %in% c('t', 'pt')) {
    stop("Invalid mode. Please use 't' or 'pt'.")
  }

  # Calculate basic stats
  player_sum <- sum(player_values)
  player_mean <- mean(player_values)

  comp_sum <- sum(comp_values)
  comp_mean <- mean(comp_values)

  # Calculate effect size
  effect_size <- NULL
  if (mode == 't') {
    effect_size <- player_mean - comp_mean
  } else if (mode == 'pt') {
    if (length(player_values) != length(comp_values)) {
      stop("For paired t-test, player and comp values should have the same length.")
    }
    effect_size <- mean(player_values - comp_values)
  }

  # Return results as a list
  result <- list(
    player_sum = player_sum,
    player_mean = player_mean,
    comp_sum = comp_sum,
    comp_mean = comp_mean,
    effect_size = effect_size
  )

  return(result)
}

#' Play a Round of the Card Game
#'
#' This function simulates a single round of the card game, where both the
#' computer and the player are dealt a card. The function returns the updated
#' state of the game after the round.
#'
#' @param cdeck A dataframe representing the current deck of cards.
#' @param plyr_cv A character vector storing the cards dealt to the player so far.
#' @param plyr_vv A numeric vector storing the values of the cards dealt to the
#'    player so far.
#' @param plyr_ic A character vector storing the image cards dealt to the player. Default is NULL.
#' @param comp_cv A character vector storing the cards dealt to the computer so
#'    far.
#' @param comp_vv A numeric vector storing the values of the cards dealt to the
#'    computer so far.
#' @param comp_ic A character vector storing the image cards dealt to the computer. Default is NULL.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{updated_deck}: A dataframe representing the updated deck of cards after the round.
#'   \item \code{plyr_cv}: Updated character vector of cards dealt to the player.
#'   \item \code{plyr_vv}: Updated numeric vector of values of cards dealt to the player.
#'   \item \code{plyr_ic}: Updated character vector of image cards dealt to the player.
#'   \item \code{comp_cv}: Updated character vector of cards dealt to the computer.
#'   \item \code{comp_vv}: Updated numeric vector of values of cards dealt to the computer.
#'   \item \code{comp_ic}: Updated character vector of image cards dealt to the computer.
#' }
#'
#' @examples
#' # Simulate a round of the game with a sample deck
#' deck <- mmcards::shuffle_deck()
#' plyr_cards <- character(0)
#' plyr_values <- numeric(0)
#' comp_cards <- character(0)
#' comp_values <- numeric(0)
#' round_result <- play_round(deck, plyr_cv = plyr_cards, plyr_vv = plyr_values,
#'                            comp_cv = comp_cards, comp_vv = comp_values)
#'
#' @export
play_round <- function(cdeck, plyr_cv, plyr_vv, plyr_ic = NULL, comp_cv, comp_vv, comp_ic = NULL) {

  # Deal card to player
  card_plyr <- mmcards::deal_card(cdeck)
  plyr_cv <- c(plyr_cv, as.character(card_plyr$dealt_card$card))
  plyr_vv <- c(plyr_vv, card_plyr$dealt_card$value)
  if(inherits(card_plyr$updated_deck, "ImgDeck")){
    plyr_ic <- c(plyr_ic, card_plyr$dealt_card$icard)
  } else {
    plyr_ic <- c(plyr_ic, NULL)
  }


  # Update deck after comp's card is dealt
  shd <- card_plyr$updated_deck

  # Deal card to comp
  card_comp <- mmcards::deal_card(shd)
  comp_cv <- c(comp_cv, as.character(card_comp$dealt_card$card))
  comp_vv <- c(comp_vv, card_comp$dealt_card$value)
  if (inherits(card_comp$updated_deck, "ImgDeck")){
    comp_ic <- c(comp_ic, card_comp$dealt_card$icard)
  } else {
    comp_ic <- c(comp_ic, NULL)
  }


  # Return updated state
  return(list(updated_deck = card_comp$updated_deck,
              plyr_cv = plyr_cv, plyr_vv = plyr_vv, plyr_ic = plyr_ic,
              comp_cv = comp_cv, comp_vv = comp_vv, comp_ic = comp_ic))
}

