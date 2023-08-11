test_that("shuffled deck works with deck", {

  # Decks equal with same seed
  shd1 <- shuffle_deck(seed = 100)
  shd2 <- shuffle_deck(seed = 100)


  expect_equal(shd1$value[[1]], shd2$value[[1]])
  expect_equal(shd1$value[[13]], shd2$value[[13]])
  expect_equal(shd1$value[[26]], shd2$value[[26]])
  expect_equal(shd1$value[[39]], shd2$value[[39]])
  expect_equal(shd1$value[[52]], shd2$value[[52]])
})

test_that("shuffled deck works with anonymous deck", {

  # Decks equal with same seed
  shd1 <- shuffle_deck(deck_of_cards = function(x)stats::rnorm(52, mean = 8.375, sd = 3.788689),
                       seed = 100)

  expect_equal(round(mean(shd1$value), 2), 8.51)
})

test_that("deal works with a shuffled deck", {

  # Deal two cards from the same deck
  shd1 <- shuffle_deck(seed = 100)
  rshd1 <- nrow(shd1)
  card <- deal_card(shd1)
  shd1 <- card$updated_deck
  rshd2 <- nrow(shd1)
  card2 <- deal_card(shd1)

  # Test that we are holding card values
  expect_equal(card$dealt_card$value, 4.25)
  expect_equal(card2$dealt_card$value, 11.25)

  # Test that our deck is shrinking
  expect_equal(rshd1, rshd2 + 1)
})
