#' Day 09: Mirage Maintenance
#'
#' [Mirage Maintenance](https://adventofcode.com/2023/day/9)
#'
#' @name day09
#' @rdname day09
#' @details
#'
#' **Part One**
#'
#' You ride the camel through the sandstorm and stop where the ghost\'s
#' maps told you to stop. [The sandstorm subsequently subsides, somehow
#' seeing you standing at an
#' *oasis*!]{title="The sound of a sandstorm slowly settling."}
#'
#' The camel goes to get some water and you stretch your neck. As you look
#' up, you discover what must be yet another giant floating island, this
#' one made of metal! That must be where the *parts to fix the sand
#' machines* come from.
#'
#' There\'s even a [hang
#' glider](https://en.wikipedia.org/wiki/Hang_gliding){target="_blank"}
#' partially buried in the sand here; once the sun rises and heats up the
#' sand, you might be able to use the glider and the hot air to get all the
#' way up to the metal island!
#'
#' While you wait for the sun to rise, you admire the oasis hidden here in
#' the middle of Desert Island. It must have a delicate ecosystem; you
#' might as well take some ecological readings while you wait. Maybe you
#' can report any environmental instabilities you find to someone so the
#' oasis can be around for the next sandstorm-worn traveler.
#'
#' You pull out your handy *Oasis And Sand Instability Sensor* and analyze
#' your surroundings. The OASIS produces a report of many values and how
#' they are changing over time (your puzzle input). Each line in the report
#' contains the *history* of a single value. For example:
#'
#'     0 3 6 9 12 15
#'     1 3 6 10 15 21
#'     10 13 16 21 30 45
#'
#' To best protect the oasis, your environmental report should include a
#' *prediction of the next value* in each history. To do this, start by
#' making a new sequence from the *difference at each step* of your
#' history. If that sequence is *not* all zeroes, repeat this process,
#' using the sequence you just generated as the input sequence. Once all of
#' the values in your latest sequence are zeroes, you can extrapolate what
#' the next value of the original history should be.
#'
#' In the above dataset, the first history is `0 3 6 9 12 15`. Because the
#' values increase by `3` each step, the first sequence of differences that
#' you generate will be `3 3 3 3 3`. Note that this sequence has one fewer
#' value than the input sequence because at each step it considers two
#' numbers from the input. Since these values aren\'t *all zero*, repeat
#' the process: the values differ by `0` at each step, so the next sequence
#' is `0 0 0 0`. This means you have enough information to extrapolate the
#' history! Visually, these sequences can be arranged like this:
#'
#'     0   3   6   9  12  15
#'       3   3   3   3   3
#'         0   0   0   0
#'
#' To extrapolate, start by adding a new zero to the end of your list of
#' zeroes; because the zeroes represent differences between the two values
#' above them, this also means there is now a placeholder in every sequence
#' above it:
#'
#'     0   3   6   9  12  15   B
#'       3   3   3   3   3   A
#'         0   0   0   0   0
#'
#' You can then start filling in placeholders from the bottom up. `A` needs
#' to be the result of increasing `3` (the value to its left) by `0` (the
#' value below it); this means `A` must be *`3`*:
#'
#'     0   3   6   9  12  15   B
#'       3   3   3   3   3   3
#'         0   0   0   0   0
#'
#' Finally, you can fill in `B`, which needs to be the result of increasing
#' `15` (the value to its left) by `3` (the value below it), or *`18`*:
#'
#'     0   3   6   9  12  15  18
#'       3   3   3   3   3   3
#'         0   0   0   0   0
#'
#' So, the next value of the first history is *`18`*.
#'
#' Finding all-zero differences for the second history requires an
#' additional sequence:
#'
#'     1   3   6  10  15  21
#'       2   3   4   5   6
#'         1   1   1   1
#'           0   0   0
#'
#' Then, following the same process as before, work out the next value in
#' each sequence from the bottom up:
#'
#'     1   3   6  10  15  21  28
#'       2   3   4   5   6   7
#'         1   1   1   1   1
#'           0   0   0   0
#'
#' So, the next value of the second history is *`28`*.
#'
#' The third history requires even more sequences, but its next value can
#' be found the same way:
#'
#'     10  13  16  21  30  45  68
#'        3   3   5   9  15  23
#'          0   2   4   6   8
#'            2   2   2   2
#'              0   0   0
#'
#' So, the next value of the third history is *`68`*.
#'
#' If you find the next value for each history in this example and add them
#' together, you get *`114`*.
#'
#' Analyze your OASIS report and extrapolate the next value for each
#' history. *What is the sum of these extrapolated values?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f09a(x)` returns .... For Part Two,
#'   `f09b(x)` returns ....
#' @export
#' @examples
#' f09a(example_data_09())
#' f09b(example_data_09())
f09a <- function(x) {
  parsed_histories <- lapply(x, parse_numbers)
  extrapolated_values <- extrapolate_values(parsed_histories)
  sum(extrapolated_values)
}

#' @rdname day09
#' @export
f09b <- function(x) {
  parsed_histories <- lapply(x, parse_numbers)
  extrapolated_values <- extrapolate_values(parsed_histories, TRUE)
  sum(extrapolated_values)
}


#' Extrapolate values from sequence, see
#' [day 9 - 2023](https://adventofcode.com/2023/day/9)
#'
#' @param x List with parsed histories (rows of numbers).
#' @param reverse Boolean flag to indicate if the history should be flipped.
#'
#' @return Numeric value with the predicted value.
#' @export
extrapolate_values <- function(x, reverse = FALSE) {
  sapply(seq_along(x), function(i) {
    mat <- matrix(NA, ncol = length(x[[i]]), nrow = length(x[[i]]))
    if (reverse) {
      mat[1, ] <- rev(x[[i]])
    } else {
      mat[1, ] <- x[[i]]
    }
    for (j in seq_len(nrow(mat))[-1]) {
      new_diff <- diff(mat[j - 1, ], 1)
      mat[j, ] <- c(new_diff, NA)
      idx_na <- is.na(new_diff)
      if (all(idx_na) | (sum(!idx_na) > 0 & all(new_diff[!idx_na] == 0)))
        break
    }
    sum(diag(mat[, rev(seq_len(ncol(mat)))]), na.rm = TRUE)
  })
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day09
#' @export
example_data_09 <- function(example = 1) {
  l <- list(
    a = c(
      "0 3 6 9 12 15",
      "1 3 6 10 15 21",
      "10 13 16 21 30 45"
    )
  )
  l[[example]]
}
