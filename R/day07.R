#' Day 07: Camel Cards
#'
#' [Camel Cards](https://adventofcode.com/2023/day/7)
#'
#' @name day07
#' @rdname day07
#' @details
#'
#' **Part One**
#'
#' Your all-expenses-paid trip turns out to be a one-way, five-minute ride
#' in an [airship](https://en.wikipedia.org/wiki/Airship){target="_blank"}.
#' (At least it\'s a [*cool*
#' airship]{title="Please only read this sentence while listening to 'The Airship Blackjack' from the Final Fantasy 6 soundtrack."}!)
#' It drops you off at the edge of a vast desert and descends back to
#' Island Island.
#'
#' \"Did you bring the parts?\"
#'
#' You turn around to see an Elf completely covered in white clothing,
#' wearing goggles, and riding a large
#' [camel](https://en.wikipedia.org/wiki/Dromedary){target="_blank"}.
#'
#' \"Did you bring the parts?\" she asks again, louder this time. You
#' aren\'t sure what parts she\'s looking for; you\'re here to figure out
#' why the sand stopped.
#'
#' \"The parts! For the sand, yes! Come with me; I will show you.\" She
#' beckons you onto the camel.
#'
#' After riding a bit across the sands of Desert Island, you can see what
#' look like very large rocks covering half of the horizon. The Elf
#' explains that the rocks are all along the part of Desert Island that is
#' directly above Island Island, making it hard to even get there.
#' Normally, they use big machines to move the rocks and filter the sand,
#' but the machines have broken down because Desert Island recently stopped
#' receiving the *parts* they need to fix the machines.
#'
#' You\'ve already assumed it\'ll be your job to figure out why the parts
#' stopped when she asks if you can help. You agree automatically.
#'
#' Because the journey will take a few days, she offers to teach you the
#' game of *Camel Cards*. Camel Cards is sort of similar to
#' [poker](https://en.wikipedia.org/wiki/List_of_poker_hands){target="_blank"}
#' except it\'s designed to be easier to play while riding a camel.
#'
#' In Camel Cards, you get a list of *hands*, and your goal is to order
#' them based on the *strength* of each hand. A hand consists of *five
#' cards* labeled one of `A`, `K`, `Q`, `J`, `T`, `9`, `8`, `7`, `6`, `5`,
#' `4`, `3`, or `2`. The relative strength of each card follows this order,
#' where `A` is the highest and `2` is the lowest.
#'
#' Every hand is exactly one *type*. From strongest to weakest, they are:
#'
#' -   *Five of a kind*, where all five cards have the same label: `AAAAA`
#' -   *Four of a kind*, where four cards have the same label and one card
#'     has a different label: `AA8AA`
#' -   *Full house*, where three cards have the same label, and the
#'     remaining two cards share a different label: `23332`
#' -   *Three of a kind*, where three cards have the same label, and the
#'     remaining two cards are each different from any other card in the
#'     hand: `TTT98`
#' -   *Two pair*, where two cards share one label, two other cards share a
#'     second label, and the remaining card has a third label: `23432`
#' -   *One pair*, where two cards share one label, and the other three
#'     cards have a different label from the pair and each other: `A23A4`
#' -   *High card*, where all cards\' labels are distinct: `23456`
#'
#' Hands are primarily ordered based on type; for example, every *full
#' house* is stronger than any *three of a kind*.
#'
#' If two hands have the same type, a second ordering rule takes effect.
#' Start by comparing the *first card in each hand*. If these cards are
#' different, the hand with the stronger first card is considered stronger.
#' If the first card in each hand have the *same label*, however, then move
#' on to considering the *second card in each hand*. If they differ, the
#' hand with the higher second card wins; otherwise, continue with the
#' third card in each hand, then the fourth, then the fifth.
#'
#' So, `33332` and `2AAAA` are both *four of a kind* hands, but `33332` is
#' stronger because its first card is stronger. Similarly, `77888` and
#' `77788` are both a *full house*, but `77888` is stronger because its
#' third card is stronger (and both hands have the same first and second
#' card).
#'
#' To play Camel Cards, you are given a list of hands and their
#' corresponding *bid* (your puzzle input). For example:
#'
#'     32T3K 765
#'     T55J5 684
#'     KK677 28
#'     KTJJT 220
#'     QQQJA 483
#'
#' This example shows five hands; each hand is followed by its *bid*
#' amount. Each hand wins an amount equal to its bid multiplied by its
#' *rank*, where the weakest hand gets rank 1, the second-weakest hand gets
#' rank 2, and so on up to the strongest hand. Because there are five hands
#' in this example, the strongest hand will have rank 5 and its bid will be
#' multiplied by 5.
#'
#' So, the first step is to put the hands in order of strength:
#'
#' -   `32T3K` is the only *one pair* and the other hands are all a
#'     stronger type, so it gets rank *1*.
#' -   `KK677` and `KTJJT` are both *two pair*. Their first cards both have
#'     the same label, but the second card of `KK677` is stronger (`K` vs
#'     `T`), so `KTJJT` gets rank *2* and `KK677` gets rank *3*.
#' -   `T55J5` and `QQQJA` are both *three of a kind*. `QQQJA` has a
#'     stronger first card, so it gets rank *5* and `T55J5` gets rank *4*.
#'
#' Now, you can determine the total winnings of this set of hands by adding
#' up the result of multiplying each hand\'s bid with its rank (`765` \*
#' 1 + `220` \* 2 + `28` \* 3 + `684` \* 4 + `483` \* 5). So the *total
#' winnings* in this example are *`6440`*.
#'
#' Find the rank of every hand in your set. *What are the total winnings?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data.
#' @param joker Flag to indicate if jokers ('J') are scored differently.
#' @return For Part One, `f07a(x)` returns .... For Part Two,
#'   `f07b(x)` returns ....
#' @export
#' @examples
#' f07a(example_data_07())
#' f07b(example_data_07())
f07a <- function(x, joker = FALSE) {
  deck <- split_deck(x)
  deck$hand_split <- lapply(deck$hand, split_hand)
  deck$strength <- sapply(deck$hand_split, hand_strength, joker = joker)
  sorted_hands <- lapply_df(unique(deck$strength),
                            function(i) {
                              aux <- deck[deck$strength == i, ]
                              aux$id <- seq_len(nrow(aux))
                              if (nrow(aux) == 1)
                                return(aux)
                              aux_2 <- lapply_df(aux$hand_split,
                                                 cards_strength,
                                                 joker = joker)
                              aux_3 <- aux[order(aux_2[, 1], # sort by 1st card
                                                 aux_2[, 2], # sort by 2nd card
                                                 aux_2[, 3], # sort by 3rd card
                                                 aux_2[, 4], # sort by 4th card
                                                 aux_2[, 5], # sort by 5th card
                                                 decreasing = TRUE), ]
                              aux_3$id <- seq_len(nrow(aux))
                              return(aux_3)
                            })
  sorted_hands_2 <- sorted_hands[order(sorted_hands$strength,
                                       decreasing = TRUE), ]
  sum(sorted_hands_2$bid * rev(seq_along(sorted_hands_2$hand)))
}


#' @rdname day07
#' @export
f07b <- function(x) {
  return(f07a(x, joker = TRUE))
}

#' Calculate the strength of individual cards, see
#' [day 7 - 2023](https://adventofcode.com/2023/day/7)
#'
#' @param x Vector with cards (strings).
#' @param joker Flag to indicate if jokers ('J') are scored differently.
#'
#' @return Card scores
#' @export
cards_strength <- function(x, joker = FALSE) {
  cards <- c('A', 'K', 'Q', 'J', 'T', 9:2)
  if (joker)
    cards <- c('A', 'K', 'Q', 'T', 9:2, 'J')
  idx <- sapply(x, \(x) which(cards %in% x))
  rev(seq_along(cards))[idx]
}

#' Calculate the strength of a group of cards / a hand, see
#' [day 7 - 2023](https://adventofcode.com/2023/day/7)
#'
#' @param x Vector with cards (strings).
#' @param joker Flag to indicate if jokers ('J') are scored differently.
#'
#' @return Hand score
#' @export
hand_strength <- function(x, joker = FALSE) {
  tab_hand <- table(x) # find frequency of cards
  wildcard <- 0
  # check if there are jokers
  if (joker && "J" %in% names(tab_hand)) {
    wildcard <- getElement(tab_hand, "J") # extract number of jokers
    if (wildcard == 5)
      return(7)
    # Remove joker counts
    tab_hand <- tab_hand[-which(names(tab_hand) == "J")]
  }

  if ((5 - wildcard) %in% tab_hand) { # Five of a kind
    return(7)
  } else if ((4 - wildcard) %in% tab_hand) { # Four of a kind
    return(6)
  }

  # Full house
  if (wildcard == 0 & all(c(2, 3) %in% tab_hand)) {
    return(5)
  } else if (wildcard == 1) {
    if (all((c(2 - wildcard, 3)) %in% tab_hand) |
        sum(tab_hand == 2) == 2) {
      return(5)
    }
  } else if (wildcard == 2) {
    if (all(c(1, 2) %in% tab_hand)) {
      return(5)
    }
  }

  if ((3 - wildcard) %in% tab_hand) { # Three of a kind
    return(4)
  } else if ((2 - wildcard) %in% table(tab_hand)) { # Two pair
    return(3)
  } else if ((2 - wildcard) %in% tab_hand) { # One pair
    return(2)
  } else if ((5 - wildcard) %in% table(tab_hand)) { # High card
    return(1)
  }
  return(0)
}

#' Split string with hands of cards, see
#' [day 7 - 2023](https://adventofcode.com/2023/day/7)
#'
#' @param x String with hands of cards.
#'
#' @return Data frame with hands
#' @export
split_deck <- function(x) {
  lapply_df(x, function(l) {
    line <- strsplit(l, " ")[[1]]
    data.frame(
      hand = line[1],
      bid = as.numeric(line[2])
    )
  })
}

#' Split string with cards, see
#' [day 7 - 2023](https://adventofcode.com/2023/day/7)
#'
#' @param x String with cards.
#'
#' @return Vector of strings for each card
#' @export
split_hand <- function(x) {
  strsplit(x, "")[[1]]
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day07
#' @export
example_data_07 <- function(example = 1) {
  l <- list(
    a = c(
      "32T3K 765",
      "T55J5 684",
      "KK677 28",
      "KTJJT 220",
      "QQQJA 483"
    )
  )
  l[[example]]
}
