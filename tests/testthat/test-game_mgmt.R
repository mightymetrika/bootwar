test_that("score keeper works as expected", {

  # Set up vectors
  comp_cv <- vector(mode = "character")
  comp_vv <- vector(mode = "numeric")
  plyr_cv <- vector(mode = "character")
  plyr_vv <- vector(mode = "numeric")


  # Return a shuffled_deck
  shd <- shuffle_deck(seed = 150)

  #Round 1

  # Deal first card to comp
  card <- deal_card(shd)
  comp_cv <- c(comp_cv, card$dealt_card$card)
  comp_vv <- c(comp_vv, card$dealt_card$value)

  # Deal first card to plyr
  card <- deal_card(card$updated_deck)
  plyr_cv <- c(plyr_cv, card$dealt_card$card)
  plyr_vv <- c(plyr_vv, card$dealt_card$value)

  # Get round 1 scores
  scores <- score_keeper(plyr_vv, comp_vv, mode = "t")

  expect_equal(scores$effect_size, 2.75)

  # Round 2

  card <- deal_card(card$updated_deck)
  comp_cv <- c(comp_cv, card$dealt_card$card)
  comp_vv <- c(comp_vv, card$dealt_card$value)

  card <- deal_card(card$updated_deck)
  plyr_cv <- c(plyr_cv, card$dealt_card$card)
  plyr_vv <- c(plyr_vv, card$dealt_card$value)

  # Get round 2 scores
  scores <- score_keeper(plyr_vv, comp_vv, mode = "t")

  expect_equal(scores$effect_size, 1.125)

})

test_that("play round works as expected", {

  # Set up vectors
  comp_cv <- vector(mode = "character")
  comp_vv <- vector(mode = "numeric")
  plyr_cv <- vector(mode = "character")
  plyr_vv <- vector(mode = "numeric")


  # Return a shuffled_deck
  ideck <- shuffle_deck(seed = 150)

  # Round 1
  rres <- play_round(cdeck = ideck, comp_cv = comp_cv, comp_vv = comp_vv,
                     plyr_cv = plyr_cv, plyr_vv = plyr_vv)

  # Get Round 1 scores
  scores <- score_keeper(rres$plyr_vv, rres$comp_vv, mode = "t")
  expect_equal(scores$effect_size, 2.75)

  # Round 2
  rres <- play_round(cdeck = rres$updated_deck, comp_cv = rres$comp_cv, comp_vv = rres$comp_vv,
                     plyr_cv = rres$ply_cv, plyr_vv = rres$plyr_vv)

  # Get round 2 scores
  scores <- score_keeper(rres$plyr_vv, rres$comp_vv, mode = "t")

  expect_equal(scores$effect_size, 1.125)

})
