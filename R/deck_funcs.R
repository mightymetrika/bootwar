#' Shuffle a Deck of Cards
#'
#' This function shuffles a given deck of cards. If the provided deck is a function,
#' it generates values based on the anonymous function before shuffling.
#' If not, it directly shuffles the provided deck.
#'
#' @param deck_of_cards A dataframe representing the deck of cards or a function
#'    that generates values for the deck. Defaults to a predefined deck named `deck`.
#' @param seed An optional seed for reproducibility.
#'
#' @return A shuffled dataframe of cards. The dataframe will have a class attribute
#'         of "data.frame", "shuffled deck", and either "deck", "anonymous deck"
#'         or "interleaved deck" depending on the input.
#'
#' @examples
#' # Using a predefined deck
#' shuffled <- shuffle_deck(seed = 123)
#'
#' # Using an anonymous function
#' fun_deck <- function(x) { stats::runif(52, 1, 52) }
#' shuffled <- shuffle_deck(deck_of_cards = fun_deck, seed = 123)
#'
#' # Using an anonymous function to create an interleaved deck
#' shuffled <- shuffle_deck(deck_of_cards = function(x) {list(stats::rnorm(26, 1, 2),
#'                                                            stats::rnorm(26, 1.5, 1.5))},
#'                          seed = 100)
#'
#' @export
shuffle_deck <- function(deck_of_cards = deck, seed) {

  # Switch out globals
  if (is.function(deck_of_cards)) deck <- NULL

  # Set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Check if deck_of_cards is a function
  if (is.function(deck_of_cards)) {
    # Generate the values based on the anonymous function
    decks <- deck_of_cards(NULL)

    # If the result is a list with two elements, treat them as separate decks
    if (is.list(decks) && length(decks) == 2) {

      # Check if the two decks are of the same length
      if (length(decks[[1]]) != length(decks[[2]])) {
        stop("Both decks must be of the same length for interleaving!")
      }

      # Create separate decks for player and computer
      A_deck <- data.frame(card = paste("A", 1:length(decks[[1]]), sep = "_"), value = decks[[1]])
      B_deck <- data.frame(card = paste("B", 1:length(decks[[2]]), sep = "_"), value = decks[[2]])

      # Shuffle each deck
      A_deck <- A_deck[sample(nrow(A_deck)), ]
      B_deck <- B_deck[sample(nrow(B_deck)), ]

      # Interleave the decks
      interleaved_deck <- rbind(A_deck, B_deck)[order(rep(1:nrow(A_deck), 2)), ]

      class(interleaved_deck) <- c("data.frame", "shuffled deck", "interleaved deck")
      return(interleaved_deck)

    } else {
      # If it's just one deck, treat it as before
      values <- decks
      ordered_indices <- order(values, decreasing = FALSE)
      ordered_deck <- data.frame(card = 1:length(values), value = values[ordered_indices])

      # Shuffle the ordered deck
      shuffled_deck <- ordered_deck[sample(nrow(ordered_deck)), ]
      class(shuffled_deck) <- c("data.frame", "shuffled deck", "anonymous deck")
      return(shuffled_deck)
    }
  } else {
    # Shuffle the provided deck
    shuffled_deck <- deck_of_cards[sample(nrow(deck_of_cards)), ]
    class(shuffled_deck) <- c("data.frame", "shuffled deck", "deck")
    return(shuffled_deck)
  }
}


#' Deal a Card from the Deck
#'
#' This function deals the top card from a given deck and updates the deck by
#' removing the dealt card. If the deck is empty, the function will return an
#' error indicating there are no more cards to deal.
#'
#' @param current_deck A dataframe representing the current deck of cards.
#'
#' @return A list containing two elements:
#' \itemize{
#'   \item \code{dealt_card}: A dataframe containing the dealt card.
#'   \item \code{updated_deck}: A dataframe representing the updated deck after dealing.
#' }
#'
#' @examples
#' # Create a sample deck
#' deck <- data.frame(card = c("A", "B", "C"), value = c(1, 2, 3))
#'
#' # Deal a card from the sample deck
#' result <- deal_card(deck)
#' dealt <- result$dealt_card
#' remaining <- result$updated_deck
#'
#' @export
deal_card <- function(current_deck) {
  if (nrow(current_deck) == 0) {
    stop("No more cards in the deck!")
  }

  card_to_deal <- current_deck[1, ]
  updated_deck <- current_deck[-1, ]  # Remove the top card

  return(list(dealt_card = card_to_deal, updated_deck = updated_deck))
}
