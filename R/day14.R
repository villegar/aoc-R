#' Day 14: Parabolic Reflector Dish
#'
#' [Parabolic Reflector Dish](https://adventofcode.com/2023/day/14)
#'
#' @name day14
#' @rdname day14
#' @details
#'
#' **Part One**
#'
#' You reach the place where all of the mirrors were pointing: a massive
#' [parabolic reflector
#' dish](https://en.wikipedia.org/wiki/Parabolic_reflector){target="_blank"}
#' [attached]{title="Why, where do you attach YOUR massive parabolic reflector dishes?"}
#' to the side of another large mountain.
#'
#' The dish is made up of many small mirrors, but while the mirrors
#' themselves are roughly in the shape of a parabolic reflector dish, each
#' individual mirror seems to be pointing in slightly the wrong direction.
#' If the dish is meant to focus light, all it\'s doing right now is
#' sending it in a vague direction.
#'
#' This system must be what provides the energy for the lava! If you focus
#' the reflector dish, maybe you can go where it\'s pointing and use the
#' light to fix the lava production.
#'
#' Upon closer inspection, the individual mirrors each appear to be
#' connected via an elaborate system of ropes and pulleys to a large metal
#' platform below the dish. The platform is covered in large rocks of
#' various shapes. Depending on their position, the weight of the rocks
#' deforms the platform, and the shape of the platform controls which ropes
#' move and ultimately the focus of the dish.
#'
#' In short: if you move the rocks, you can focus the dish. The platform
#' even has a control panel on the side that lets you *tilt* it in one of
#' four directions! The rounded rocks (`O`) will roll when the platform is
#' tilted, while the cube-shaped rocks (`#`) will stay in place. You note
#' the positions of all of the empty spaces (`.`) and rocks (your puzzle
#' input). For example:
#'
#'     O....#....
#'     O.OO#....#
#'     .....##...
#'     OO.#O....O
#'     .O.....O#.
#'     O.#..O.#.#
#'     ..O..#O..O
#'     .......O..
#'     #....###..
#'     #OO..#....
#'
#' Start by tilting the lever so all of the rocks will slide *north* as far
#' as they will go:
#'
#'     OOOO.#.O..
#'     OO..#....#
#'     OO..O##..O
#'     O..#.OO...
#'     ........#.
#'     ..#....#.#
#'     ..O..#.O.O
#'     ..O.......
#'     #....###..
#'     #....#....
#'
#' You notice that the support beams along the north side of the platform
#' are *damaged*; to ensure the platform doesn\'t collapse, you should
#' calculate the *total load* on the north support beams.
#'
#' The amount of load caused by a single rounded rock (`O`) is equal to the
#' number of rows from the rock to the south edge of the platform,
#' including the row the rock is on. (Cube-shaped rocks (`#`) don\'t
#' contribute to load.) So, the amount of load caused by each rock in each
#' row is as follows:
#'
#'     OOOO.#.O.. 10
#'     OO..#....#  9
#'     OO..O##..O  8
#'     O..#.OO...  7
#'     ........#.  6
#'     ..#....#.#  5
#'     ..O..#.O.O  4
#'     ..O.......  3
#'     #....###..  2
#'     #....#....  1
#'
#' The total load is the sum of the load caused by all of the *rounded
#' rocks*. In this example, the total load is *`136`*.
#'
#' Tilt the platform so that the rounded rocks all roll north. Afterward,
#' *what is the total load on the north support beams?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f14a(x)` returns .... For Part Two,
#'   `f14b(x)` returns ....
#' @export
#' @examples
#' f14a(example_data_14(1))
#' f14b(example_data_14(1))
f14a <- function(x) {
  parsed_platform <- lapply_df(x, \(x) strsplit(x, "")[[1]])
  tilted_platform <- tilt_platform(parsed_platform)
  total_load(tilted_platform)
}

#' Tilt platform, see
#' [day 14 - 2023](https://adventofcode.com/2023/day/14)
#'
#'
#' @param x Parsed platform, matrix.
#' @param direction Integer with tilting direction: 1 (North), 2 (West),
#'     3 (South) and 4 (East).
#'
#' @return Tilted platform, matrix.
#' @export
tilt_platform <- function(x, direction = 1) {
  if (direction %in% c(1, 3)) { # North and South
    # extract columns from the platform
    x_vecs <- apply(x, 2, \(x) paste0(x, collapse = ""))
    if (direction == 3)
      x_vecs <- sapply(x_vecs, rev_str)
  } else { # East and West
    # extract rows from the platform
    x_vecs <- apply(x, 1, \(x) paste0(x, collapse = ""))
    if (direction == 4)
      x_vecs <- sapply(x_vecs, rev_str)
  }
  # tilt platform
  x_tilted <- sapply(x_vecs, function(str) {
    # split by cube-shaped rocks
    str_2 <- strsplit(gsub("#", " #", str), "#")[[1]]
    # "roll" round rocks
    str_3 <- sapply(str_2, function(s2) {
      s3 <- strsplit(s2, "")[[1]]
      paste0(
        paste0(rep("O", times = sum(s3 == "O")), collapse = ""),
        gsub("O", "", gsub(" ", "#", s2)),
        collapse = ""
      )
    })
    paste0(str_3, collapse = "")
  })
  # break platform into tiles
  if (direction == 1) # North
    return(t(lapply_df(x_tilted, \(x) strsplit(x, "")[[1]])))
  if (direction == 2) # West
    return(lapply_df(x_tilted, \(x) strsplit(x, "")[[1]]))
  if (direction == 3) # South
    return(t(lapply_df(sapply(x_tilted, rev_str), \(x) strsplit(x, "")[[1]])))
  if (direction == 4) # East
  return(lapply_df(sapply(x_tilted, rev_str), \(x) strsplit(x, "")[[1]]))
}

#' Calculate total load, see
#' [day 14 - 2023](https://adventofcode.com/2023/day/14)
#'
#' @param x Tilted platform, matrix.
#'
#' @return Total load.
#' @export
total_load <- function(x) {
  rounded_rocks <- apply(x, 1, \(x) sum(x == "O"))
  sum(rev(seq_len(nrow(x))) * rounded_rocks)
}

#' @rdname day14
#' @export
f14b <- function(x) {
  cycles <- 1E3
  parsed_platform <- lapply_df(x, \(x) strsplit(x, "")[[1]])
  tilted_platform <- parsed_platform
  # hsh_tb <- hashtab()
  # sethash(hsh_tb, get_key(tilted_platform), 0)
  for (i in seq_len(cycles)) {
    tilted_platform <- tilt_cycle(tilted_platform)
    # new_key <- get_key(tilted_platform)
    # existing_hash <- gethash(hsh_tb, new_key, NULL)
    # sethash(hsh_tb, new_key, i)
    # if (!is.null(existing_hash)) {
    #   message("Hash found: ", i)
    #   print(existing_hash)
    #   print(new_key)
    #   break
    # }
  }
  total_load(tilted_platform)
}

#' Tilting cycle, see
#' [day 14 - 2023](https://adventofcode.com/2023/day/14)
#'
#' @param x Parsed platform, matrix.
#'
#' @return Tilted platform, matrix.
#' @export
tilt_cycle <- function(x) {
  tilt_platform(
    tilt_platform(
      tilt_platform(
        tilt_platform(x, direction = 1), # North
        direction = 2 # West
      ),
      direction = 3 # South
    ),
    direction = 4 # East
  )
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day14
#' @export
example_data_14 <- function(example = 1) {
  l <- list(
    a = c("O....#....", "O.OO#....#", ".....##...", "OO.#O....O",
          ".O.....O#.", "O.#..O.#.#", "..O..#O..O", ".......O..", "#....###..",
          "#OO..#....")
  )
  l[[example]]
}
