load_test_data <- function(day = "00") {

  input_path <- file.path("test-data", paste0("input", day, ".txt"))

  readLines(input_path, warn = FALSE)

}

load_real_data <- function(day = "00") {

  input_path <- system.file(
    paste0("input", day, ".txt"),
    package = "aocR"
  )

  readLines(input_path, warn = FALSE)

}

#' Wrapper for `lapply` that returns a data frame
#'
#' @inheritParams base::lapply
#' @inheritDotParams base::lapply
#' @param bind_fx Binding function for the output (e.g., `rbind` or `cbind`).
#'
#' @return Data frame with the output of `lapply`
#' @export
lapply_df <- function(X, FUN, ..., bind_fx = rbind) {
  out_df <- do.call(bind_fx, lapply(X, FUN, ...))
  rownames(out_df) <- NULL
  out_df
}

#' Wrapper for `by` that returns a data frame
#'
#' @inheritParams base::by
#' @inheritDotParams base::by
#' @param bind_fx Binding function for the output (e.g., `rbind` or `cbind`).
#'
#' @return Data frame with the output of `by`
#' @export
by_df <- function(data, INDICES, FUN, ..., simplify = TRUE, bind_fx = rbind) {
  do.call(bind_fx, by(data, INDICES, FUN, ..., simplify = simplify))
}

#' Get adjacent indices
#'
#' @param x Origin index.
#' @param max_x Max value.
#' @param k Numeric value with the range of the indices.
#'
#' @return Vector with indices
#' @export
#'
#' @examples
#' get_indices(1, 10)
#' get_indices(1, 10, 5)
get_indices <- function(x, max_x, k = 1) {
  idx <- (x - k):(x + k)
  idx[idx > 0 & idx <= max_x]
}

#' Get adjacent elements from a 2D object
#'
#' @param x 2D object.
#' @param i Reference row.
#' @param j Reference column.
#'
#' @return Matrix with adjacent elements
#' @export
get_adjacent_elements <- function(x, i, j) {
  x[get_indices(i, nrow(x)), get_indices(j, ncol(x))]
}

#' Get adjacent numeric elements from a 2D object in the same row
#'
#' @param x 2D object.
#' @param i Reference row.
#' @param j Reference column.
#'
#' @return Matrix with adjacent elements
#' @export
get_adjacent_elements_row <- function(x, i, j) {
  row_values <- x[i, ]
  idx <- !grepl("\\D", row_values) & !is.na(row_values)
  last <- last_true_element(idx, j)
  first <- first_true_element(idx, j)
  if (any(is.na(first), is.na(last)))
    return(NA)
  tryCatch(row_values[sort(min(first, last):max(first, last))],
           error = function(e) {
             message(x)
             message(i, ", ", j)
           })
}

#' Get the first `TRUE` consecutive element from reference point
#'
#' @param x Vector with logical values.
#' @param i Reference point
#'
#' @return Numeric value with the index for the first `TRUE` consecutive element
#' @export
first_true_element <- function(x, i) {
  if (i > 0) {
    if (x[i])
      return(first_true_element(x, i - 1))
    return(i + 1)
  }
  return(i + 1)
}

#' Get the last `TRUE` consecutive element from reference point
#'
#' @param x Vector with logical values.
#' @param i Reference point
#'
#' @return Numeric value with the index for the last `TRUE` consecutive element
#' @export
last_true_element <- function(x, i) {
  if (i <= length(x)) {
    if (x[i])
      return(last_true_element(x, i + 1))
    return(i - 1)
  }
  return(i - 1)
}

#' Convert matrix subscripts to a linear subscript
#'
#' @param x 2D object.
#' @param i Reference row.
#' @param j Reference column.
#'
#' @return Numeric value with linear subscript.
#' @export
sub2ind <- function(x, i, j) {
  (i - 1) * nrow(x) + j
}

#' Convert a linear subscript to matrix subscripts
#'
#' @param x 2D object.
#' @param ind Numeric value with linear subscript.
#'
#' @return 2D array with row and column subscripts.
#' @export
ind2sub <- function(x, ind) {
  data.frame(i = row(x)[ind], j = col(x)[ind])
}

#' Parse numbers from text section
#'
#' @param x String of characters with target text.
#' @param header String with the header of the target section.
#'
#' @return Numeric vector.
#' @export
#'
#' @examples
#' parse_numbers("Distance:  9  40  200", "Distance:")
parse_numbers <- function(x, header = "") {
  section <- regmatches(x, regexpr(paste0(header, "[0-9 \n]+"), x))
  section_2 <- trimws(gsub("\\s+", " ", gsub("\\D", " ", section)))
  as.numeric(strsplit(section_2, " ")[[1]])
}

#' Reverse string
#'
#' @param x String to be reversed.
#'
#' @return Reversed string.
#' @export
#'
#' @examples
#' rev_str("abc")
rev_str <- function(x) {
  intToUtf8(rev(utf8ToInt(x)))
}

#' Rotate 2D array 90 deg clockwise
#'
#' @param x Input array.
#'
#' @return Rotated array
#' @export
rotate <- function(x) {
  t(apply(x, 2, rev))
}

#' Rotate 2D array 90 deg anti-clockwise
#'
#' @param x Input array.
#'
#' @return Rotated array
#' @export
rotate_rev <- function(x) {
  apply(t(x), 2, rev)
}

