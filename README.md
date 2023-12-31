
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bootwar

<!-- badges: start -->
<!-- badges: end -->

‘bootwar’ aims to help users build their intuition about nonparametric
bootstrap tests with pooled resampling, especially for independent and
paired t-tests (Dwivedi, Mallawaarachchi, and Alvarado, 2017).

Inspired by the card game War, which has straightforward rules but can
be time-consuming, ‘bootwar’ is a variant with smaller sample sizes.
Here, after playing a user-defined number of rounds, a winner is
determined through nonparametric bootstrap tests with pooled resampling
as implemented in the
[npboottprm](https://CRAN.R-project.org/package=npboottprm) R package.

## Installation

You can install the released version of ‘bootwar’ from
[CRAN](https://CRAN.R-project.org):

``` r
install.packages("bootwar")
```

Install the development version of bootwar from GitHub using
[devtools](https://devtools.r-lib.org/):

``` r
# install.packages("devtools")
devtools::install_github("mightymetrika/bootwar")
```

## Example

This example demonstrates how to play a five-round game of bootwar:

### Initialization:

``` r
# Load bootwar
library(bootwar)

# Set up vectors for computer and player's cards and values
comp_cv <- vector(mode = "character")
comp_vv <- vector(mode = "numeric")
plyr_cv <- vector(mode = "character")
plyr_vv <- vector(mode = "numeric")

# Shuffle a 52 card standard deck from the 'mmcards' package
sdeck <- mmcards::shuffle_deck(seed = 150)
head(sdeck)
#>    rank suit card value
#> 35   10    H  10H 10.50
#> 25    K    D   KD 13.25
#> 31    6    H   6H  6.50
#> 5     6    C   6C  6.00
#> 27    2    H   2H  2.50
#> 44    6    S   6S  6.75
```

### Play the First Round:

``` r
rres <- play_round(cdeck = sdeck,
                   plyr_cv = plyr_cv, plyr_vv = plyr_vv,
                   comp_cv = comp_cv, comp_vv = comp_vv)
```

### Continue the Game for Four More Rounds:

``` r
for (i in 1:4) {
  rres <- play_round(cdeck = rres$updated_deck,
                     plyr_cv = rres$plyr_cv, plyr_vv = rres$plyr_vv,
                     comp_cv = rres$comp_cv, comp_vv = rres$comp_vv)
}

# Ensure 10 cards have been dealt
nrow(rres$updated_deck)
#> [1] 42
```

### Analyze the Game:

Use analyze_game() to evaluate the results. In this example, we’ll apply
the paired t-test, but users can opt for the independent t-test by
setting mode to ‘t’. Modify the conf.level parameter to change the
confidence interval (think of this as a level which controls the amount
of draws).

``` r
gres <- analyze_game(plyr_vv = rres$plyr_vv, comp_vv = rres$comp_vv,
                     mode = "pt", nboot = 1000, seed = 150, conf.level = 0.05)

# Display game results
gres$winner
#> [1] "Computer Wins"
gres$bootstrap_results$effect.size
#> [1] -0.25
gres$bootstrap_results$ci.effect.size
#> 47.5% 52.5% 
#> -0.15  0.05
gres$bootstrap_results$p.value
#> [1] 0.838
```

For a more streamlined gameplay experience, the bootwar() function
launches an interactive Shiny web application.

Users can also customize the game using an anonymous function for deck
definition, as showcased in the anonymous-bootwar vignette. Users can
also customize the game by using an anonymous function to define a
different deck for each player, as showcased in the interleaved-bootwar
vignette.

## References

Dwivedi AK, Mallawaarachchi I, Alvarado LA (2017). “Analysis of small
sample size studies using nonparametric bootstrap test with pooled
resampling method.” Statistics in Medicine, 36 (14), 2187-2205.
