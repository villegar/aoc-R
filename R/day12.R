#' Day 12: Hot Springs
#'
#' [Hot Springs](https://adventofcode.com/2023/day/12)
#'
#' @name day12
#' @rdname day12
#' @details
#'
#' **Part One**
#'
#' You finally reach the hot springs! You can see steam rising from
#' secluded areas attached to the primary, ornate building.
#'
#' As you turn to enter, the [researcher](11) stops you. \"Wait - I thought
#' you were looking for the hot springs, weren\'t you?\" You indicate that
#' this definitely looks like hot springs to you.
#'
#' \"Oh, sorry, common mistake! This is actually the
#' [onsen](https://en.wikipedia.org/wiki/Onsen){target="_blank"}! The hot
#' springs are next door.\"
#'
#' You look in the direction the researcher is pointing and suddenly notice
#' the [massive metal helixes]{title="I love this joke. I'm not sorry."}
#' towering overhead. \"This way!\"
#'
#' It only takes you a few more steps to reach the main gate of the massive
#' fenced-off area containing the springs. You go through the gate and into
#' a small administrative building.
#'
#' \"Hello! What brings you to the hot springs today? Sorry they\'re not
#' very hot right now; we\'re having a *lava shortage* at the moment.\" You
#' ask about the missing machine parts for Desert Island.
#'
#' \"Oh, all of Gear Island is currently offline! Nothing is being
#' manufactured at the moment, not until we get more lava to heat our
#' forges. And our springs. The springs aren\'t very springy unless
#' they\'re hot!\"
#'
#' \"Say, could you go up and see why the lava stopped flowing? The springs
#' are too cold for normal operation, but we should be able to find one
#' springy enough to launch *you* up there!\"
#'
#' There\'s just one problem - many of the springs have fallen into
#' disrepair, so they\'re not actually sure which springs would even be
#' *safe* to use! Worse yet, their *condition records of which springs are
#' damaged* (your puzzle input) are also damaged! You\'ll need to help them
#' repair the damaged records.
#'
#' In the giant field just outside, the springs are arranged into *rows*.
#' For each row, the condition records show every spring and whether it is
#' *operational* (`.`) or *damaged* (`#`). This is the part of the
#' condition records that is itself damaged; for some springs, it is simply
#' *unknown* (`?`) whether the spring is operational or damaged.
#'
#' However, the engineer that produced the condition records also
#' duplicated some of this information in a different format! After the
#' list of springs for a given row, the size of each *contiguous group of
#' damaged springs* is listed in the order those groups appear in the row.
#' This list always accounts for every damaged spring, and each number is
#' the entire size of its contiguous group (that is, groups are always
#' separated by at least one operational spring: `####` would always be
#' `4`, never `2,2`).
#'
#' So, condition records with no unknown spring conditions might look like
#' this:
#'
#'     #.#.### 1,1,3
#'     .#...#....###. 1,1,3
#'     .#.###.#.###### 1,3,1,6
#'     ####.#...#... 4,1,1
#'     #....######..#####. 1,6,5
#'     .###.##....# 3,2,1
#'
#' However, the condition records are partially damaged; some of the
#' springs\' conditions are actually *unknown* (`?`). For example:
#'
#'     ???.### 1,1,3
#'     .??..??...?##. 1,1,3
#'     ?#?#?#?#?#?#?#? 1,3,1,6
#'     ????.#...#... 4,1,1
#'     ????.######..#####. 1,6,5
#'     ?###???????? 3,2,1
#'
#' Equipped with this information, it is your job to figure out *how many
#' different arrangements* of operational and broken springs fit the given
#' criteria in each row.
#'
#' In the first line (`???.### 1,1,3`), there is exactly *one* way separate
#' groups of one, one, and three broken springs (in that order) can appear
#' in that row: the first three unknown springs must be broken, then
#' operational, then broken (`#.#`), making the whole row `#.#.###`.
#'
#' The second line is more interesting: `.??..??...?##. 1,1,3` could be a
#' total of *four* different arrangements. The last `?` must always be
#' broken (to satisfy the final contiguous group of three broken springs),
#' and each `??` must hide exactly one of the two broken springs. (Neither
#' `??` could be both broken springs or they would form a single contiguous
#' group of two; if that were true, the numbers afterward would have been
#' `2,3` instead.) Since each `??` can either be `#.` or `.#`, there are
#' four possible arrangements of springs.
#'
#' The last line is actually consistent with *ten* different arrangements!
#' Because the first number is `3`, the first and second `?` must both be
#' `.` (if either were `#`, the first number would have to be `4` or
#' higher). However, the remaining run of unknown spring conditions have
#' many different ways they could hold groups of two and one broken
#' springs:
#'
#'     ?###???????? 3,2,1
#'     .###.##.#...
#'     .###.##..#..
#'     .###.##...#.
#'     .###.##....#
#'     .###..##.#..
#'     .###..##..#.
#'     .###..##...#
#'     .###...##.#.
#'     .###...##..#
#'     .###....##.#
#'
#' In this example, the number of possible arrangements for each row is:
#'
#' -   `???.### 1,1,3` - *`1`* arrangement
#' -   `.??..??...?##. 1,1,3` - *`4`* arrangements
#' -   `?#?#?#?#?#?#?#? 1,3,1,6` - *`1`* arrangement
#' -   `????.#...#... 4,1,1` - *`1`* arrangement
#' -   `????.######..#####. 1,6,5` - *`4`* arrangements
#' -   `?###???????? 3,2,1` - *`10`* arrangements
#'
#' Adding all of the possible arrangement counts together produces a total
#' of *`21`* arrangements.
#'
#' For each row, count all of the different arrangements of operational and
#' broken springs that meet the given criteria. *What is the sum of those
#' counts?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f12a(x)` returns .... For Part Two,
#'   `f12b(x)` returns ....
#' @export
#' @examples
#' f12a(example_data_12(2))
#' f12b(example_data_12(2))
f12a <- function(x) {
  parsed_records <- parse_condition_records(x)
  combinations <- lapply_df(apply(parsed_records, 1, check_records), \(x) x)
  sum(combinations, na.rm = TRUE)
}

parse_condition_records <- function(x, times = 1) {
  lapply_df(seq_along(x), function(i) {
    tmp <- strsplit(x[[i]], "\\s")[[1]]
    format_1 <- strsplit(tmp[1], "")[[1]]
    format_2 <- as.numeric(strsplit(tmp[2], ",")[[1]])
    list(id = i,
         f1 = rep(format_1, times),
         f2 = rep(format_2, times)
    )
  })
}

map_records <- function() {
  c("." = "operational",
    "#" = "damaged",
    "?" = "unknown")
}

check_records <- function(x) {
  if (!any(x$f1 == "?")) {
    return(1)
  } else {
    unknown_entries <- which(x$f1 == "?")
    damaged_entries <- which(x$f1 == "#")
    # total - present in pattern
    missing_damaged_entries <- sum(x$f2) - sum(x$f1 == "#")
    # create possible combinations
    combinations <- t(combn(unknown_entries, missing_damaged_entries))

    valid_combinations <- apply(combinations, 1, function(i) {
      x$f1[i] <- "#"
      x$f1[x$f1 == "?"] <- "."
      aux <- strsplit(paste0(x$f1, collapse = ""), "\\.")[[1]]
      aux_2 <- sapply(aux, nchar)
      return(paste0(aux_2[aux_2 > 0], collapse = ","))
      })
    return(sum(valid_combinations == paste0(x$f2, collapse = ",")))
  }
  return(0)
}

#' @rdname day12
#' @export
f12b <- function(x) {
  x <- example_data_12(2)
  parsed_records <- parse_condition_records(x, times = 5)
  combinations <- lapply_df(apply(parsed_records, 1, check_records), \(x) x)
  sum(combinations, na.rm = TRUE)
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day12
#' @export
example_data_12 <- function(example = 1) {
  l <- list(
    a1 = c(
      "#.#.### 1,1,3",
      ".#...#....###. 1,1,3",
      ".#.###.#.###### 1,3,1,6",
      "####.#...#... 4,1,1",
      "#....######..#####. 1,6,5",
      ".###.##....# 3,2,1"
    ),
    a2 = c(
      "???.### 1,1,3",
      ".??..??...?##. 1,1,3",
      "?#?#?#?#?#?#?#? 1,3,1,6",
      "????.#...#... 4,1,1",
      "????.######..#####. 1,6,5",
      "?###???????? 3,2,1"
    )
  )
  l[[example]]
}
