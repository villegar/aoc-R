#' Day 08: Haunted Wasteland
#'
#' [Haunted Wasteland](https://adventofcode.com/2023/day/8)
#'
#' @name day08
#' @rdname day08
#' @details
#'
#' **Part One**
#'
#' You\'re still riding a camel across Desert Island when you spot a
#' sandstorm quickly approaching. When you turn to warn the Elf, she
#' disappears before your eyes! To be fair, she had just finished warning
#' you about *ghosts* a few minutes ago.
#'
#' One of the camel\'s pouches is labeled \"maps\" - sure enough, it\'s
#' full of documents (your puzzle input) about how to navigate the desert.
#' At least, you\'re pretty sure that\'s what they are; one of the
#' documents contains a list of left/right instructions, and the rest of
#' the documents seem to describe some kind of *network* of labeled nodes.
#'
#' It seems like you\'re meant to use the *left/right* instructions to
#' *navigate the network*. Perhaps if you have the camel follow the same
#' instructions, you can escape the haunted wasteland!
#'
#' After examining the maps for a bit, two nodes stick out: `AAA` and
#' `ZZZ`. You feel like `AAA` is where you are now, and you have to follow
#' the left/right instructions until you reach `ZZZ`.
#'
#' This format defines each *node* of the network individually. For
#' example:
#'
#'     RL
#'
#'     AAA = (BBB, CCC)
#'     BBB = (DDD, EEE)
#'     CCC = (ZZZ, GGG)
#'     DDD = (DDD, DDD)
#'     EEE = (EEE, EEE)
#'     GGG = (GGG, GGG)
#'     ZZZ = (ZZZ, ZZZ)
#'
#' Starting with `AAA`, you need to *look up the next element* based on the
#' next left/right instruction in your input. In this example, start with
#' `AAA` and go *right* (`R`) by choosing the right element of `AAA`,
#' *`CCC`*. Then, `L` means to choose the *left* element of `CCC`, *`ZZZ`*.
#' By following the left/right instructions, you reach `ZZZ` in *`2`*
#' steps.
#'
#' Of course, you might not find `ZZZ` right away. If you run out of
#' left/right instructions, repeat the whole sequence of instructions as
#' necessary: `RL` really means `RLRLRLRLRLRLRLRL...` and so on. For
#' example, here is a situation that takes *`6`* steps to reach `ZZZ`:
#'
#'     LLR
#'
#'     AAA = (BBB, BBB)
#'     BBB = (AAA, ZZZ)
#'     ZZZ = (ZZZ, ZZZ)
#'
#' Starting at `AAA`, follow the left/right instructions. *How many steps
#' are required to reach `ZZZ`?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f08a(x)` returns .... For Part Two,
#'   `f08b(x)` returns ....
#' @export
#' @examples
#' f08a(example_data_08(1))
#' f08a(example_data_08(2))
#' f08b(example_data_08(3))
f08a <- function(x) {
  # extract network route
  route <- trimws(x[1])
  # parse network map
  parsed_network <- parse_network(x)
  # find numbers of steps from 'AAA' to 'ZZZ'
  traverse_map(route, parsed_network, "AAA", "ZZZ")$steps
}

#' @rdname day08
#' @export
f08b <- function(x) {
  # extract network route
  route <- trimws(x[1])
  # parse network map
  parsed_network <- parse_network(x)
  # extract starting points, start with 'A'
  starts <- parsed_network$entry[grepl("A$", parsed_network$entry)]
  # extract ending points / targets, end with 'Z'
  targets <- parsed_network$entry[grepl("Z$", parsed_network$entry)]
  # find the number of steps required for each starting point
  steps <- sapply(seq_along(starts), function(i) {
    traverse_map(route, parsed_network, starts[i], targets)$steps
  })
  # find the Least Common Multiple of the steps, that is, how many times
  # the network has to be traversed for all the starting points to reach
  # a node ending in 'Z' at the same time.
  Reduce(lcm, steps)
}

#' Traverse network map, see
#' [day 8 - 2023](https://adventofcode.com/2023/day/8)
#'
#' @param route String with route (e.g., 'LR').
#' @param parsed_network Data frame with parsed network map.
#' @param start String with starting point.
#' @param target Vector of strings with targets/end points.
#'
#' @return List with various elements resulting from traversing the network.
#' @export
traverse_map <- function(route, parsed_network, start, target) {
  labels <- c(start)
  steps <- 0
  while (!any(start %in% target)) {
    for (i in seq_len(nchar(route))) {
      direction <- substr(route, start = i, stop = i)
      aux <- parsed_network[parsed_network$entry == start, ]
      if (direction == "L") {
        start <- aux$left
      } else {
        start <- aux$right
      }
      labels <- c(labels, start)
    }
    steps <- steps + nchar(route)
  }
  list(start = labels[1],
       target = target,
       steps = steps,
       labels = labels)
}

#' Parse network map, see
#' [day 8 - 2023](https://adventofcode.com/2023/day/8)
#'
#' @param x String with network map.
#'
#' @return Data frame with parsed network map.
#' @export
parse_network <- function(x) {
  lapply_df(x, function(path) {
    if (nchar(path) != 16)
      return(data.frame())
    aux <- trimws(strsplit(path, "=")[[1]])
    aux_2 <- trimws(strsplit(aux[2], ",")[[1]])
    data.frame(
      entry = aux[1],
      left = gsub("\\(", "", aux_2[1]),
      right = gsub("\\)", "", aux_2[2])
    )
  })
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day08
#' @export
example_data_08 <- function(example = 1) {
  l <- list(
    a = c(
      "RL",
      "",
      "AAA = (BBB, CCC)",
      "BBB = (DDD, EEE)",
      "CCC = (ZZZ, GGG)",
      "DDD = (DDD, DDD)",
      "EEE = (EEE, EEE)",
      "GGG = (GGG, GGG)",
      "ZZZ = (ZZZ, ZZZ)"
    ),
    b = c(
      "LLR",
      "",
      "AAA = (BBB, BBB)",
      "BBB = (AAA, ZZZ)",
      "ZZZ = (ZZZ, ZZZ)"
    ),
    c = c(
      "LR",
      "",
      "11A = (11B, XXX)",
      "11B = (XXX, 11Z)",
      "11Z = (11B, XXX)",
      "22A = (22B, XXX)",
      "22B = (22C, 22C)",
      "22C = (22Z, 22Z)",
      "22Z = (22B, 22B)",
      "XXX = (XXX, XXX)"
    )
  )
  l[[example]]
}
