#' Day 03: Gear Ratios
#'
#' [Gear Ratios](https://adventofcode.com/2023/day/3)
#'
#' @name day03
#' @rdname day03
#' @details
#'
#' **Part One**
#'
#' You and the Elf eventually reach a [gondola
#' lift](https://en.wikipedia.org/wiki/Gondola_lift){target="_blank"}
#' station; he says the gondola lift will take you up to the *water
#' source*, but this is as far as he can bring you. You go inside.
#'
#' It doesn\'t take long to find the gondolas, but there seems to be a
#' problem: they\'re not moving.
#'
#' \"Aaah!\"
#'
#' You turn around to see a slightly-greasy Elf with a wrench and a look of
#' surprise. \"Sorry, I wasn\'t expecting anyone! The gondola lift isn\'t
#' working right now; it\'ll still be a while before I can fix it.\" You
#' offer to help.
#'
#' The engineer explains that an engine part seems to be missing from the
#' engine, but nobody can figure out which one. If you can *add up all the
#' part numbers* in the engine schematic, it should be easy to work out
#' which part is missing.
#'
#' The engine schematic (your puzzle input) consists of a visual
#' representation of the engine. There are lots of numbers and symbols you
#' don\'t really understand, but apparently *any number adjacent to a
#' symbol*, even diagonally, is a \"part number\" and should be included in
#' your sum. (Periods (`.`) do not count as a symbol.)
#'
#' Here is an example engine schematic:
#'
#'     467..114..
#'     ...*......
#'     ..35..633.
#'     ......#...
#'     617*......
#'     .....+.58.
#'     ..592.....
#'     ......755.
#'     ...$.*....
#'     .664.598..
#'
#' In this schematic, two numbers are *not* part numbers because they are
#' not adjacent to a symbol: `114` (top right) and `58` (middle right).
#' Every other number is adjacent to a symbol and so *is* a part number;
#' their sum is *`4361`*.
#'
#' Of course, the actual engine schematic is much larger. *What is the sum
#' of all of the part numbers in the engine schematic?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f03a(x)` returns .... For Part Two,
#'   `f03b(x)` returns ....
#' @export
#' @examples
#' f03a(example_data_03())
#' f03b(example_data_03())
f03a <- function(x) {
  IGNORED_CHRS <- c("\\.")
  REPLACEMENT <- " "
  # replace `IGNORED_CHRS` with `REPLACEMENT`
  schematic_wo_invalid_chrs <- replace_chrs(x, IGNORED_CHRS, REPLACEMENT)
  # convert schematic input into data frame
  parsed_schema <- parse_schematic(schematic_wo_invalid_chrs)
  # replace NAs
  parsed_schema_wo_nas <-
    apply(parsed_schema, 2, gsub, pattern = REPLACEMENT, replacement = NA)
  # map schematic to find numbers adjacent to valid symbols
  mapped_schematic <- map_schematic(parsed_schema_wo_nas)
  # find the sum of part numbers
  sum(mapped_schematic$number, na.rm = TRUE)
}

#' @name day03
#' @rdname day03
#' @details
#'
#' **Part Two**
#'
#' The engineer finds the missing part and installs it in the engine! As
#' the engine springs to life, you jump in the closest gondola, finally
#' ready to ascend to the water source.
#'
#' You don\'t seem to be going very fast, though. Maybe something is still
#' wrong? Fortunately, the gondola has a phone labeled \"help\", so you pick
#' it up and the engineer answers.
#'
#' Before you can explain the situation, she suggests that you
#' look out the window. There stands the engineer, holding a phone in one
#' hand and waving with the other. You\'re going so slowly that you
#' haven\'t even left the station. You exit the gondola.
#'
#' The missing part wasn\'t the only issue - one of the gears in the engine
#' is wrong. A gear is any `*` symbol that is adjacent to exactly two part
#' numbers. Its gear ratio is the result of multiplying those two numbers
#' together.
#'
#' This time, you need to find the gear ratio of every gear and add them all up
#' so that the engineer can figure out which gear needs to be replaced.
#'
#' Consider the same engine schematic again:
#'
#'     467..114..
#'     ...*......
#'     ..35..633.
#'     ......#...
#'     617*......
#'     .....+.58.
#'     ..592.....
#'     ......755.
#'     ...$.*....
#'     .664.598..
#'
#' In this schematic, there are two gears. The first is in the top left; it has
#' part numbers 467 and 35, so its gear ratio is 16345. The second gear is in
#' the lower right; its gear ratio is 451490. (The `*` adjacent to 617 is not
#' a gear because it is only adjacent to one part number.) Adding up all of
#' the gear ratios produces 467835. What is the sum of all of the gear ratios
#' in your engine schematic?
#' @export
f03b <- function(x) {
  IGNORED_CHRS <- c("\\.", "\\/", "%", "&", "-", "\\+", "#", "\\$", "\\=", "@")
  REPLACEMENT <- " "
  # replace `IGNORED_CHRS` with `REPLACEMENT`
  schematic_wo_invalid_chrs <- replace_chrs(x, IGNORED_CHRS, REPLACEMENT)
  # convert schematic input into data frame
  parsed_schema <- parse_schematic(schematic_wo_invalid_chrs)
  # replace NAs
  parsed_schema_wo_nas <-
    apply(parsed_schema, 2, gsub, pattern = REPLACEMENT, replacement = NA)
  # map schematic to find numbers adjacent to gear symbol, `*`
  mapped_schematic_gears <- map_schematic_gears(parsed_schema_wo_nas)
  # find the sum of the gear rations
  sum(mapped_schematic_gears$gear_ratio, na.rm = TRUE)
}

#' Replace characters
#'
#' @param x String.
#' @param chrs Vector with characters to be replaced.
#' @param replacement String with the replacement string.
#'
#' @return New string with characters replaced.
#' @export
#'
#' @examples
#' replace_chrs("467..114..")
replace_chrs <- function(x, chrs = "\\.", replacement = " ") {
  sapply(x, gsub,
         pattern = paste0(chrs, collapse = "|"),
         replacement = replacement)
}

#' Parse schematic string, see
#' [day 3 - 2023](https://adventofcode.com/2023/day/3)
#'
#' @param x String with schematic.
#'
#' @return Data frame with schematic
#' @export
#'
#' @examples
#' parse_schematic("467..114..")
parse_schematic <- function(x) {
  do.call(rbind, sapply(x, strsplit, split = ""))
}

#' Map schematic to extract numeric values adjacent to symbol, see
#' [day 3 - 2023](https://adventofcode.com/2023/day/3)
#'
#' @param x Data frame with parsed schematic.
#'
#' @return Data frame with part numbers.
#' @export
map_schematic <- function(x) {
  # find indices of non-numeric characters
  idxy <- matrix(grepl("\\D", x), ncol = ncol(x))
  # find matrix subscripts of those elements (if any)
  idxy_mat <- get_matrix_subscripts(x, idxy)
  # get adjacent elements for each matrix subscript pair
  adjacent_elements <- lapply(seq_len(nrow(idxy_mat)), function(i) {
    get_adjacent_elements(x, idxy_mat[i, 1], idxy_mat[i, 2])
  })
  # find adjacent digits (for each numeric entry)
  aux <- lapply(seq_along(adjacent_elements), function(i) {
    # extract the i-th matrix with adjacent elements
    adj_el_ith <- adjacent_elements[[i]]
    # extract global indices (in reference to the large data frame)
    idx <- get_indices(idxy_mat[i, 1], nrow(x))
    idy <- get_indices(idxy_mat[i, 2], ncol(x))
    # create boolean matrix with numeric entries
    adj_el_ith_num <- matrix(!is.na(adj_el_ith) & !grepl("\\D", adj_el_ith),
                             ncol = ncol(adj_el_ith))
    if (!any(adj_el_ith_num))
      return(data.frame())
    # get matrix subscripts
    adj_el_ith_num_idx <- get_matrix_subscripts(adj_el_ith_num, adj_el_ith_num)
    # map the "local" indices to global (schematic data frame) equivalent
    adj_el_ith_num_idx_schema <- lapply(seq_len(nrow(adj_el_ith_num_idx)),
                                        function(i) {
                                          data.frame(
                                            i = idy[adj_el_ith_num_idx[i, 2]],
                                            j = idx[adj_el_ith_num_idx[i, 1]]
                                          )
                                        })
    # extract numeric elements in each row from the adjacent elements
    adjacent_elements_rows <-
      lapply(seq_along(adj_el_ith_num_idx_schema),
             function(i) {
               get_adjacent_elements_row(x,
                                         adj_el_ith_num_idx_schema[[i]]$j,
                                         adj_el_ith_num_idx_schema[[i]]$i
                                         )
               }
             )
    # create numeric strings from the results
    numbers <- sapply(adjacent_elements_rows, paste0, collapse = "")
    # replace NAs
    numbers_wo_na <- sapply(numbers, gsub, pattern = "NA", replacement = "0")
    # combine results into a data frame
    out_df <- data.frame(row = idx[adj_el_ith_num_idx[ ,1]],
                         number = as.numeric(numbers_wo_na))
    # extract unique values
    do.call(rbind, by(out_df, out_df$row, unique))
  })
  out_df <- do.call(rbind, aux)
  rownames(out_df) <- NULL
  out_df
}

#' Map schematic to extract numeric values adjacent to the gear symbol, `*`,
#' see [day 3 - 2023](https://adventofcode.com/2023/day/3#part2)
#'
#' @param x Data frame with parsed schematic.
#'
#' @return Data frame with gear ratios.
#' @export
map_schematic_gears <- function(x) {
  # find indices of non-numeric characters
  idxy <- matrix(grepl("\\D", x), ncol = ncol(x))
  # find matrix subscripts of those elements (if any)
  idxy_mat <- get_matrix_subscripts(x, idxy)
  # get adjacent elements for each matrix subscript pair
  adjacent_elements <- lapply(seq_len(nrow(idxy_mat)), function(i) {
    get_adjacent_elements(x, idxy_mat[i, 1], idxy_mat[i, 2])
  })
  # find adjacent digits (for each numeric entry)
  aux <- lapply(seq_along(adjacent_elements), function(i) {
    # extract the i-th matrix with adjacent elements
    adj_el_ith <- adjacent_elements[[i]]
    # extract global indices (in reference to the large data frame)
    idx <- get_indices(idxy_mat[i, 1], nrow(x))
    idy <- get_indices(idxy_mat[i, 2], ncol(x))
    # create boolean matrix with numeric entries
    adj_el_ith_num <- matrix(!is.na(adj_el_ith) & !grepl("\\D", adj_el_ith),
                             ncol = ncol(adj_el_ith))
    if (!any(adj_el_ith_num))
      return(data.frame())
    # get matrix subscripts
    adj_el_ith_num_idx <- get_matrix_subscripts(adj_el_ith_num, adj_el_ith_num)
    # map the "local" indices to global (schematic data frame) equivalent
    adj_el_ith_num_idx_schema <- lapply(seq_len(nrow(adj_el_ith_num_idx)),
                                        function(i) {
                                          data.frame(
                                            i = idy[adj_el_ith_num_idx[i, 2]],
                                            j = idx[adj_el_ith_num_idx[i, 1]]
                                          )
                                        })
    # extract numeric elements in each row from the adjacent elements
    adjacent_elements_rows <-
      lapply(seq_along(adj_el_ith_num_idx_schema),
             function(i) {
               get_adjacent_elements_row(x,
                                         adj_el_ith_num_idx_schema[[i]]$j,
                                         adj_el_ith_num_idx_schema[[i]]$i
               )
             }
      )
    # create numeric strings from the results
    numbers <- sapply(adjacent_elements_rows, paste0, collapse = "")
    # replace NAs
    numbers_wo_na <- sapply(numbers, gsub, pattern = "NA", replacement = "0")
    # combine results into a data frame
    out_df <- data.frame(row = idx[adj_el_ith_num_idx[ ,1]],
                         number = as.numeric(numbers_wo_na))
    # extract unique values
    unique_df <- do.call(rbind, by(out_df, out_df$row, unique))
    if (nrow(unique_df) == 2) {
      summary_df <- data.frame(
        gear_id = paste0(unique_df$number, collapse = "-"),
        gear_ratio = prod(unique_df$number, na.rm = TRUE)
      )
      return(summary_df)
      # unique_df$gear_id <- paste0(unique_df$number, collapse = "-")
      # unique_df$gear_ratio <- prod(unique_df$number, na.rm = TRUE)
      # return(unique_df)
    }
    return(data.frame())
  })
  out_df <- do.call(rbind, aux)
  rownames(out_df) <- NULL
  out_df
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day03
#' @export
example_data_03 <- function(example = 1) {
  l <- list(
    a = c(
      "467..114..",
      "...*......",
      "..35..633.",
      "......#...",
      "617*......",
      ".....+.58.",
      "..592.....",
      "......755.",
      "...$.*....",
      ".664.598.."
    )
  )
  l[[example]]
}
