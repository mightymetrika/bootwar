# shuffle_deck <- function(deck_of_cards = deck, seed){
#
#   # Set seed for reproducibility
#   if (!is.null(seed)) {
#     set.seed(seed)
#   }
#
#   # Shuffle deck
#   shuffled_deck <- deck_of_cards[sample(nrow(deck_of_cards)), ]
#
#   # Return deck
#   return(shuffled_deck)
#
# }

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

deal_card <- function(current_deck) {
  if (nrow(current_deck) == 0) {
    stop("No more cards in the deck!")
  }

  card_to_deal <- current_deck[1, ]
  updated_deck <- current_deck[-1, ]  # Remove the top card

  return(list(dealt_card = card_to_deal, updated_deck = updated_deck))
}
