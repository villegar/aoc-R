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
#' f08b()
f08a <- function(x) {
  route <- trimws(x[1])
  parsed_network <- parse_network(x)
  parsed_network_sorted <- parsed_network[order(parsed_network$entry), ]
  possible_paths <- find_possible_paths(parsed_network_sorted)
  aux <- possible_paths[names(possible_paths) == "ZZZ"]
  if (grepl(pattern = "\\|", aux)) {
    aux_2 <- strsplit(aux, "\\|R")[[1]]
    aux <- c(paste0(aux_2, collapse = ""), paste0("R", aux_2[-1]))
  }
  aux_2 <- aux[sapply(aux, grepl, x = route)]
  if (route == aux_2)
      return(nchar(aux_2))
  replacements <- nchar(aux_2)
  while (TRUE) {
    reg_match <- regexpr(paste0(aux_2), route)
    if (reg_match > 0)
      replacements <- replacements + nchar(aux_2)

    route <- gsub(aux_2, "", route)
    if (nchar(route) == 0)
      break
    if (nchar(route) < nchar(aux_2)) {
      if (reg_match == 1) {
        route <- paste0("L", route)
      } else {
        route <- paste0(route, "R")
      }
    }
  }
  replacements
}

find_possible_paths <- function(x) {
  # create empty matrix of paths
  paths_mat <- matrix("", nrow = nrow(x), ncol = nrow(x))
  # extract unique entries
  entries <- sort(x$entry)
  colnames(paths_mat) <-  entries
  rownames(paths_mat) <-  entries
  for (i in seq_len(nrow(paths_mat))) {
    # left
    j_l <- which(x[i, 2] == entries)
    if (i != j_l)
      paths_mat[i, j_l] <- paste0(paths_mat[i, j_l], "L")
    # right
    j_r <- which(x[i, 3] == entries)
    if (j_l == j_r && i != j_l) {
      paths_mat[i, j_r] <- paste0(paths_mat[i, j_r], "|R")
    } else if (i != j_r)
      paths_mat[i, j_r] <- paste0(paths_mat[i, j_r], "R")
  }

  aaa_idx <- 1 # which(x$entry == "AAA")
  idx <- nchar(paths_mat[aaa_idx, ]) > 0
  origin <- paths_mat[aaa_idx, idx]

  origin_df <- data.frame(
    entry = entries[idx],
    start = origin
  )

  for (i in seq_len(nrow(origin_df))) {
    next_entry <- origin_df[i, ]$entry
    while(TRUE) {
      aux <- paths_mat[next_entry, ]
      aux_2 <- aux[nchar(aux) > 0]
      if (length(aux_2) < 1)
        break
      if ("L" %in% aux_2) { # left
        j <- which(names(aux_2[aux_2 %in% "L"]) == entries)
        paths_mat[aaa_idx, j] <- paste0(origin_df[i, ]$start, "L")
      }
      if ("R" %in% aux_2) { # right
        j <- which(names(aux_2[aux_2 %in% "R"]) == entries)
        paths_mat[aaa_idx, j] <- paste0(origin_df[i, ]$start, "R")
      }
      break
    }
  }
  return(paths_mat[aaa_idx, nchar(paths_mat[aaa_idx, ]) > 0])
}

follow_trail <- function(x, new_entry) {
  aux <- x[next_entry, ]
  aux_2 <- aux[nchar(aux) > 0]
  out <- data.frame(
    left = "",
    right = "",
  )
  if (length(aux_2) < 1)
    break
  if ("L" %in% aux_2) { # left
    j <- which(names(aux_2[aux_2 %in% "L"]) == entries)
    x[aaa_idx, j] <- paste0(origin_df[i, ]$start, "L")
  }
  if ("R" %in% aux_2) { # right
    j <- which(names(aux_2[aux_2 %in% "R"]) == entries)
    x[aaa_idx, j] <- paste0(origin_df[i, ]$start, "R")
  }
}

#' @rdname day08
#' @export
f08b <- function(x) {

}


f08_helper <- function(x) {

}

parse_network <- function(x) {
  lapply_df(x, function(path) {
    if (nchar(path) != 16)
      return(data.frame())
    aux <- trimws(strsplit(path, "=")[[1]])
    aux_2 <- trimws(strsplit(aux[2], ",")[[1]])
    # entry <- regmatches(path, regexpr("^(.*?) ", path))
    # left <- regmatches(path, regexpr("\\((.*?),", path))
    # right <- regmatches(path, regexpr(",(.*?)\\)", path))
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
    )
  )
  l[[example]]
}
