#' Day 01: Trebuchet?!
#'
#' [Trebuchet?!](https://adventofcode.com/2023/day/1)
#'
#' @name day01
#' @rdname day01
#' @details
#'
#' **Part One**
#'
#' Something is wrong with global snow production, and you\'ve been
#' selected to take a look. The Elves have even given you a map; on it,
#' they\'ve used stars to mark the top fifty locations that are likely to
#' be having problems.
#'
#' You\'ve been doing this long enough to know that to restore snow
#' operations, you need to check all *fifty stars* by December 25th.
#'
#' Collect stars by solving puzzles. Two puzzles will be made available on
#' each day in the Advent calendar; the second puzzle is unlocked when you
#' complete the first. Each puzzle grants *one star*. Good luck!
#'
#' You try to ask why they can\'t just use a [weather machine](/2015/day/1)
#' (\"not powerful enough\") and where they\'re even sending you (\"the
#' sky\") and why your map looks mostly blank (\"you sure ask a lot of
#' questions\")
#' [and]{title="My hope is that this abomination of a run-on sentence somehow conveys the chaos of being hastily loaded into a trebuchet."}
#' hang on did you just say the sky (\"of course, where do you think snow
#' comes from\") when you realize that the Elves are already loading you
#' into a
#' [trebuchet](https://en.wikipedia.org/wiki/Trebuchet){target="_blank"}
#' (\"please hold still, we need to strap you in\").
#'
#' As they\'re making the final adjustments, they discover that their
#' calibration document (your puzzle input) has been *amended* by a very
#' young Elf who was apparently just excited to show off her art skills.
#' Consequently, the Elves are having trouble reading the values on the
#' document.
#'
#' The newly-improved calibration document consists of lines of text; each
#' line originally contained a specific *calibration value* that the Elves
#' now need to recover. On each line, the calibration value can be found by
#' combining the *first digit* and the *last digit* (in that order) to form
#' a single *two-digit number*.
#'
#' For example:
#'
#'     1abc2
#'     pqr3stu8vwx
#'     a1b2c3d4e5f
#'     treb7uchet
#'
#' In this example, the calibration values of these four lines are `12`,
#' `38`, `15`, and `77`. Adding these together produces *`142`*.
#'
#' Consider your entire calibration document. *What is the sum of all of
#' the calibration values?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f01a(x)` returns .... For Part Two,
#'   `f01b(x)` returns ....
#' @export
#' @examples
#' f01a(example_data_01())
#' f01b(example_data_01(2))
f01a <- function(x) {
  # extract first and last digits
  first_digit <- sub("^\\D*(\\d).*$", "\\1", x)
  last_digit <- sub("^.*(\\d)\\D*$", "\\1", x)
  # call helper function to calculate sum
  f01_helper(first_digit, last_digit)
}


#' @rdname day01
#' @export
f01b <- function(x) {
  # extract first and last digits
  first_digit <- sapply(x, lookup_digit)
  last_digit <- sapply(x, lookup_digit, backwards = TRUE)
  # call helper function to calculate sum
  f01_helper(first_digit, last_digit)
}


#' Combines two vectors with digits and returns the sum
#'
#' @param first_digit Vector with first digits.
#' @param last_digit Vector with last digits.
#'
#' @return Sum of the combination of both vectors
#' @export
#'
#' @examples
#' f01_helper("1", "2") # should return 12
f01_helper <- function(first_digit, last_digit) {
  # combine first and last digits
  cali_vals <- paste0(first_digit, last_digit)
  # convert to numeric (default to zero)
  cali_vals_num <- sapply(c(cali_vals, "a"), function(x) {
    tryCatch(as.numeric(x),
             error = function(e) { 0 },
             warning = function(w) {0 })
  })
  # sum the calibration values
  sum(cali_vals_num, na.rm = TRUE)
}

#' Reverse a string
#'
#' @param x Original string.
#'
#' @return Reversed string
#' @export
#'
#' @examples
#' reverse_string("ouch")
reverse_string <- function(x) {
  intToUtf8(rev(utf8ToInt(x)))
}

#' Lookup digits in a string
#'
#' Lookup digits in a string, either text (e.g., 'one', 'two', ...) or numerical
#' characters. Once it finds the first digit it returns it.
#'
#' @param x Input string with digits.
#' @param backwards Flag to indicate the direction of search, if `TRUE`, then
#'     look up digits backwards, otherwise, just find the first digit in the
#'     string.
#'
#' @return String with numeric digit
#' @export
#'
#' @examples
#' lookup_digit("m9qvkqlgfhtwo3seven4seven")
#' lookup_digit("m9qvkqlgfhtwo3seven4seven", TRUE)
lookup_digit <- function(x, backwards = FALSE) {
  lookup_df <- data.frame(
    label = c("one", "two", "three", "four", "five",
              "six", "seven", "eight", "nine"),
    value = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  )

  # check the direction of the search
  if (backwards) {
    x <- sapply(x, reverse_string)
    lookup_df$label <- sapply(lookup_df$label, reverse_string)
  }

  last_chr <- 1
  last_chr_suffix <- FALSE
  new_str <- ""
  i <- 1
  while (i <= nchar(x)) {
    txt <- substr(x, last_chr, i)
    has_digits <- grepl("[0-9]+", txt)
    is_suffix <- any(startsWith(lookup_df$label, txt))
    is_num <- txt %in% lookup_df$label
    if (has_digits) {
      new_str <- gsub("\\D", "", txt)
      break
    } else if (is_num) {
      new_str <- lookup_df$value[lookup_df$label == txt]
      break
    } else if (!is_suffix) {
      if (last_chr_suffix) {
        last_chr_suffix <- FALSE
        i <- i - 1 # don't move to the next character
        last_chr <- i
      } else {
        last_chr <- last_chr + 1 # move to the next character
      }
    } else {
      last_chr_suffix <- TRUE # flag that current character is a valid suffix
    }
    i <- i + 1 # move to the next character
  }

  return(new_str)
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day01
#' @export
example_data_01 <- function(example = 1) {
  l <- list(
    a = c(
      "1abc2",
      "pqr3stu8vwx",
      "a1b2c3d4e5f",
      "treb7uchet"
    ),
    b = c(
      "two1nine",
      "eightwothree",
      "abcone2threexyz",
      "xtwone3four",
      "4nineeightseven2",
      "zoneight234",
      "7pqrstsixteen"
    )
  )
  l[[example]]
}
