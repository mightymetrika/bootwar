# Create the deck
suits <- c('C', 'D', 'H', 'S')
ranks <- c('2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K', 'A')
deck <- expand.grid(rank = ranks, suit = suits)
deck$card <- paste0(deck$rank, deck$suit)

# Turn suit to factor
deck$suit <- factor(deck$suit,
                    levels = c("C", "D", "H", "S"),
                    labels = c("C", "D", "H", "S"))

# Order deck
deck <- deck[order(deck$rank, deck$suit), ]

# Add values to cards
deck$value <- seq(2, 14.75, by = 0.25)

# Turn card to factor using the current order
deck$card <- factor(deck$card, levels = deck$card)

# Save data
usethis::use_data(deck, overwrite = TRUE)
