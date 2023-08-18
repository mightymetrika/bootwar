test_that("analyze_game works as expected", {
  # Set up vectors
  comp_cv <- vector(mode = "character")
  comp_vv <- vector(mode = "numeric")
  plyr_cv <- vector(mode = "character")
  plyr_vv <- vector(mode = "numeric")


  # Return a shuffled_deck
  ideck <- shuffle_deck(seed = 150)

  # Play 5 Rounds
  rres <- play_round(cdeck = ideck, comp_cv = comp_cv, comp_vv = comp_vv,
                     plyr_cv = plyr_cv, plyr_vv = plyr_vv)
  rres <- play_round(cdeck = rres$updated_deck, comp_cv = rres$comp_cv, comp_vv = rres$comp_vv,
                     plyr_cv = rres$plyr_cv, plyr_vv = rres$plyr_vv)
  rres <- play_round(cdeck = rres$updated_deck, comp_cv = rres$comp_cv, comp_vv = rres$comp_vv,
                     plyr_cv = rres$plyr_cv, plyr_vv = rres$plyr_vv)
  rres <- play_round(cdeck = rres$updated_deck, comp_cv = rres$comp_cv, comp_vv = rres$comp_vv,
                     plyr_cv = rres$plyr_cv, plyr_vv = rres$plyr_vv)
  rres <- play_round(cdeck = rres$updated_deck, comp_cv = rres$comp_cv, comp_vv = rres$comp_vv,
                     plyr_cv = rres$plyr_cv, plyr_vv = rres$plyr_vv)

  # Analyze Game Results for independent t-test
  gres <- analyze_game(comp_vv = rres$comp_vv, plyr_vv = rres$plyr_vv, mode = "t",
                       nboot = 1000, seed = 150)

  expect_equal(gres$winner, "Draw")
  expect_equal(length(gres), 3)
  expect_equal(round(gres$bootstrap_results$p.value, 2), 0.91)
  expect_equal(round(gres$classical_results$p.value, 2), 0.91)
  expect_equal(gres$bootstrap_results$effect.size,
               score_keeper(rres$plyr_vv, rres$comp_vv, mode = "t")$effect_size)

  # Analyze Game Results for paired t-test
  gres <- analyze_game(comp_vv = rres$comp_vv, plyr_vv = rres$plyr_vv, mode = 'pt',
                       nboot = 1000, seed = 150)

  expect_equal(gres$winner, "Draw")
  expect_equal(length(gres), 3)
  expect_equal(round(gres$bootstrap_results$p.value, 2), 0.87)
  expect_equal(round(gres$classical_results$p.value, 2), 0.87)
  expect_equal(gres$bootstrap_results$effect.size,
               score_keeper(rres$plyr_vv, rres$comp_vv, mode = "t")$effect_size)
})

test_that("analyze_game with 7 rounds", {
  # Set up vectors
  comp_cv <- vector(mode = "character")
  comp_vv <- vector(mode = "numeric")
  plyr_cv <- vector(mode = "character")
  plyr_vv <- vector(mode = "numeric")


  # Return a shuffled_deck
  ideck <- shuffle_deck(seed = 250)

  # Play 5 Rounds
  rres <- play_round(cdeck = ideck, comp_cv = comp_cv, comp_vv = comp_vv,
                     plyr_cv = plyr_cv, plyr_vv = plyr_vv)
  rres <- play_round(cdeck = rres$updated_deck, comp_cv = rres$comp_cv, comp_vv = rres$comp_vv,
                     plyr_cv = rres$ply_cv, plyr_vv = rres$plyr_vv)
  rres <- play_round(cdeck = rres$updated_deck, comp_cv = rres$comp_cv, comp_vv = rres$comp_vv,
                     plyr_cv = rres$ply_cv, plyr_vv = rres$plyr_vv)
  rres <- play_round(cdeck = rres$updated_deck, comp_cv = rres$comp_cv, comp_vv = rres$comp_vv,
                     plyr_cv = rres$ply_cv, plyr_vv = rres$plyr_vv)
  rres <- play_round(cdeck = rres$updated_deck, comp_cv = rres$comp_cv, comp_vv = rres$comp_vv,
                     plyr_cv = rres$ply_cv, plyr_vv = rres$plyr_vv)
  rres <- play_round(cdeck = rres$updated_deck, comp_cv = rres$comp_cv, comp_vv = rres$comp_vv,
                     plyr_cv = rres$ply_cv, plyr_vv = rres$plyr_vv)
  rres <- play_round(cdeck = rres$updated_deck, comp_cv = rres$comp_cv, comp_vv = rres$comp_vv,
                     plyr_cv = rres$ply_cv, plyr_vv = rres$plyr_vv)

  # Analyze Game Results for independent t-test
  gres <- analyze_game(comp_vv = rres$comp_vv, plyr_vv = rres$plyr_vv, mode = "t",
                       nboot = 1000, seed = 350)

  expect_equal(gres$winner, "Draw")
  expect_equal(length(gres), 3)
  expect_equal(round(gres$bootstrap_results$p.value, 2), 0.67)
  expect_equal(round(gres$classical_results$p.value, 2), 0.66)
  expect_equal(gres$bootstrap_results$effect.size,
               score_keeper(rres$plyr_vv, rres$comp_vv, mode = "t")$effect_size)

  # Analyze Game Results for paired t-test
  gres <- analyze_game(comp_vv = rres$comp_vv, plyr_vv = rres$plyr_vv, mode = 'pt',
                       nboot = 1000, seed = 350)

  expect_equal(gres$winner, "Draw")
  expect_equal(length(gres), 3)
  expect_equal(round(gres$bootstrap_results$p.value, 2), 0.66)
  expect_equal(round(gres$classical_results$p.value, 2), 0.64)
  expect_equal(gres$bootstrap_results$effect.size,
               score_keeper(rres$plyr_vv, rres$comp_vv, mode = "t")$effect_size)
})

