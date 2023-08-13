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
#'         of "data.frame", "shuffled deck", and either "deck" or "anonymous deck"
#'         depending on the input.
#'
#' @examples
#' # Using a predefined deck
#' shuffled <- shuffle_deck(seed = 123)
#'
#' # Using an anonymous function
#' fun_deck <- function(x) { runif(52, 1, 52) }
#' shuffled <- shuffle_deck(deck_of_cards = fun_deck, seed = 123)
#'
#' @export
shuffle_deck <- function(deck_of_cards = deck, seed) {

  # Set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Check if deck_of_cards is a function
  if (is.function(deck_of_cards)) {
    # Generate the values based on the anonymous function
    values <- deck_of_cards(NULL)

    # Order the values
    ordered_indices <- order(values, decreasing = FALSE)

    # Create a dataframe with card (rank order) and value columns
    ordered_deck <- data.frame(card = 1:length(values), value = values[ordered_indices])

    # Shuffle the ordered deck
    shuffled_deck <- ordered_deck[sample(nrow(ordered_deck)), ]
    class(shuffled_deck) <- c("data.frame", "shuffled deck", "anonymous deck")
  } else {
    # Shuffle the provided deck
    shuffled_deck <- deck_of_cards[sample(nrow(deck_of_cards)), ]
    class(shuffled_deck) <- c("data.frame", "shuffled deck", "deck")
  }

  # Return deck
  return(shuffled_deck)
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
