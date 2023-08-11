#' Deck of Cards
#'
#' A 52 card deck of playing cards with suit ranking.
#'
#' @format ## `deck`
#' A data frame with 52 rows and 4 columns:
#' \describe{
#'   \item{rank}{A factor representing card rank taking values 2 - A}
#'   \item{suit}{A card suit with ranked order Club (C), Diamond (D), Heart (H), and Spade (S)}
#'   \item{card}{A card}
#'   \item{value}{A card value ranging from 2.00 (2C) to 14.75 (AS)}
#' }
#' @source <Standard Deck of Playing Cards>
"deck"
