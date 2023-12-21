#' Day 13: Point of Incidence
#'
#' [Point of Incidence](https://adventofcode.com/2023/day/13)
#'
#' @name day13
#' @rdname day13
#' @details
#'
#' **Part One**
#'
#' With your help, the hot springs team locates an appropriate spring which
#' launches you neatly and precisely up to the edge of *Lava Island*.
#'
#' There\'s just one problem: you don\'t see any *lava*.
#'
#' You *do* see a lot of ash and igneous rock; there are even what look
#' like gray mountains scattered around. After a while, you make your way
#' to a nearby cluster of mountains only to discover that the valley
#' between them is completely full of large *mirrors*. Most of the mirrors
#' seem to be aligned in a consistent way; perhaps you should head in that
#' direction?
#'
#' As you move through the valley of mirrors, you find that several of them
#' have fallen from the large metal frames keeping them in place. The
#' mirrors are extremely flat and shiny, and many of the fallen mirrors
#' have lodged into the ash at strange angles. Because the terrain is all
#' one color, it\'s hard to tell where it\'s safe to walk or where you\'re
#' about to run into a mirror.
#'
#' You note down the patterns of ash (`.`) and rocks (`#`) that you see as
#' you walk (your puzzle input); perhaps by carefully analyzing these
#' patterns, you can figure out where the mirrors are!
#'
#' For example:
#'
#'     #.##..##.
#'     ..#.##.#.
#'     ##......#
#'     ##......#
#'     ..#.##.#.
#'     ..##..##.
#'     #.#.##.#.
#'
#'     #...##..#
#'     #....#..#
#'     ..##..###
#'     #####.##.
#'     #####.##.
#'     ..##..###
#'     #....#..#
#'
#' To find the reflection in each pattern, you need to find a perfect
#' reflection across either a horizontal line between two rows or across a
#' vertical line between two columns.
#'
#' In the first pattern, the reflection is across a vertical line between
#' two columns; arrows on each of the two columns point at the line between
#' the columns:
#'
#'     123456789
#'         ><
#'     #.##..##.
#'     ..#.##.#.
#'     ##......#
#'     ##......#
#'     ..#.##.#.
#'     ..##..##.
#'     #.#.##.#.
#'         ><
#'     123456789
#'
#' In this pattern, the line of reflection is the vertical line between
#' columns 5 and 6. Because the vertical line is not perfectly in the
#' middle of the pattern, part of the pattern (column 1) has nowhere to
#' reflect onto and can be ignored; every other column has a reflected
#' column within the pattern and must match exactly: column 2 matches
#' column 9, column 3 matches 8, 4 matches 7, and 5 matches 6.
#'
#' The second pattern reflects across a horizontal line instead:
#'
#'     1 #...##..# 1
#'     2 #....#..# 2
#'     3 ..##..### 3
#'     4v#####.##.v4
#'     5^#####.##.^5
#'     6 ..##..### 6
#'     7 #....#..# 7
#'
#' This pattern reflects across the horizontal line between rows 4 and 5.
#' Row 1 would reflect with a hypothetical row 8, but since that\'s not in
#' the pattern, row 1 doesn\'t need to match anything. The remaining rows
#' match: row 2 matches row 7, row 3 matches row 6, and row 4 matches row
#' 5.
#'
#' To *summarize* your pattern notes, add up *the number of columns* to the
#' left of each vertical line of reflection; to that, also add *100
#' multiplied by the number of rows* above each horizontal line of
#' reflection. In the above example, the first pattern\'s vertical line has
#' `5` columns to its left and the second pattern\'s horizontal line has
#' `4` rows above it, a total of *`405`*.
#'
#' Find the line of reflection in each of the patterns in your notes. *What
#' number do you get after summarizing all of your notes?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f13a(x)` returns .... For Part Two,
#'   `f13b(x)` returns ....
#' @export
#' @examples
#' f13a(example_data_13())
#' f13b(example_data_13())
f13a <- function(x) {
  parsed_patterns <- parse_patterns(x)
  mid_points <- lapply_df(parsed_patterns, find_mid_point_reflection)
  sum(mid_points$score, na.rm = TRUE)
}


#' @rdname day13
#' @export
f13b <- function(x) {
  parsed_patterns <- parse_patterns(x)
  mid_points <- lapply_df(parsed_patterns, find_mid_point_reflection, diff_count = 1)
  sum(mid_points$score, na.rm = TRUE)
}

#' Parse patterns, see
#' [day 13 - 2023](https://adventofcode.com/2023/day/13)
#'
#' @param x String with patterns.
#'
#' @return List with 2D matrices for each pattern.
#' @export
parse_patterns <- function(x) {
  new_pattern <- list()
  last_index <- 1
  for (i in seq_along(x)) {
    if (x[i] == "") {
      new_pattern <- c(new_pattern, list(x[last_index:(i - 1)]))
      last_index <- i + 1
    }
  }
  # add last pattern
  new_pattern <- c(new_pattern, list(x[last_index:i]))
  lapply(new_pattern, \(x) lapply_df(x, \(y) strsplit(y, "")[[1]]))
}

#' Find mid point for the reflection, see
#' [day 13 - 2023](https://adventofcode.com/2023/day/13)
#'
#' @param x Matrix with pattern.
#' @param diff_count Number of differences allowed.
#'
#' @return Data frame with score for each pattern.
#' @export
find_mid_point_reflection <- function(x, diff_count = 0) {
  # vertical
  idx_x <- seq_len(ncol(x) - 1)
  col <- 0
  for (i in idx_x) {
    reflection_cols <- min(ncol(x) - i, i)
    cols_before <- x[, (i - reflection_cols + 1):i]
    cols_after <- x[, (i + 1):(i + reflection_cols)]
    # find the number of different elements
    if (reflection_cols == 1) {
      diff_elements <- sum(cols_before != cols_after)
    } else {
      diff_elements <-
        sum(cols_before[, rev(seq_len(reflection_cols))] != cols_after)
    }
    if (diff_count == diff_elements) {
      col <- i
      break
    }
  }

  # horizontal
  idx_y <- seq_len(nrow(x) - 1)
  row <- 0
  for (i in idx_y) {
    reflection_rows <- min(nrow(x) - i, i)
    rows_before <- x[(i - reflection_rows + 1):i, ]
    rows_after <- x[(i + 1):(i + reflection_rows), ]
    # find the number of different elements
    if (reflection_rows == 1) {
      diff_elements <- sum(rows_before != rows_after)
    } else {
      diff_elements <-
        sum(rows_before[rev(seq_len(reflection_rows)), ] != rows_after)
    }
    if (diff_count == diff_elements) {
      row <- i
      break
    }
  }
  return(data.frame(col = col, row = row, score = col + row * 100))
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day13
#' @export
example_data_13 <- function(example = 1) {
  l <- list(
    a = c(
      "#.##..##.",
      "..#.##.#.",
      "##......#",
      "##......#",
      "..#.##.#.",
      "..##..##.",
      "#.#.##.#.",
      "",
      "#...##..#",
      "#....#..#",
      "..##..###",
      "#####.##.",
      "#####.##.",
      "..##..###",
      "#....#..#"
    )
  )
  l[[example]]
}
