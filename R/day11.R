#' Day 11: Cosmic Expansion
#'
#' [Cosmic Expansion](https://adventofcode.com/2023/day/11)
#'
#' @name day11
#' @rdname day11
#' @details
#'
#' **Part One**
#'
#' You continue following signs for \"Hot Springs\" and eventually come
#' across an
#' [observatory](https://en.wikipedia.org/wiki/Observatory){target="_blank"}.
#' The Elf within turns out to be a researcher studying cosmic expansion
#' using the giant telescope here.
#'
#' He doesn\'t know anything about the missing machine parts; he\'s only
#' visiting for this research project. However, he confirms that the hot
#' springs are the next-closest area likely to have people; he\'ll even
#' take you straight there once he\'s done with today\'s observation
#' analysis.
#'
#' Maybe you can help him with the analysis to speed things up?
#'
#' The researcher has collected a bunch of data and compiled the data into
#' a single giant *image* (your puzzle input). The image includes *empty
#' space* (`.`) and *galaxies* (`#`). For example:
#'
#'     ...#......
#'     .......#..
#'     #.........
#'     ..........
#'     ......#...
#'     .#........
#'     .........#
#'     ..........
#'     .......#..
#'     #...#.....
#'
#' The researcher is trying to figure out the sum of the lengths of the
#' *shortest path between every pair of galaxies*. However, there\'s a
#' catch: the universe expanded in the time it took the light from those
#' galaxies to reach the observatory.
#'
#' Due to something involving gravitational effects, *only some space
#' expands*. In fact, the result is that *any rows or columns that contain
#' no galaxies* should all actually be twice as big.
#'
#' In the above example, three columns and two rows contain no galaxies:
#'
#'        v  v  v
#'      ...#......
#'      .......#..
#'      #.........
#'     >..........<
#'      ......#...
#'      .#........
#'      .........#
#'     >..........<
#'      .......#..
#'      #...#.....
#'        ^  ^  ^
#'
#' These rows and columns need to be *twice as big*; the result of cosmic
#' expansion therefore looks like this:
#'
#'     ....#........
#'     .........#...
#'     #............
#'     .............
#'     .............
#'     ........#....
#'     .#...........
#'     ............#
#'     .............
#'     .............
#'     .........#...
#'     #....#.......
#'
#' Equipped with this expanded universe, the shortest path between every
#' pair of galaxies can be found. It can help to assign every galaxy a
#' unique number:
#'
#'     ....1........
#'     .........2...
#'     3............
#'     .............
#'     .............
#'     ........4....
#'     .5...........
#'     ............6
#'     .............
#'     .............
#'     .........7...
#'     8....9.......
#'
#' In these 9 galaxies, there are *36 pairs*. Only count each pair once;
#' order within the pair doesn\'t matter. For each pair, find any shortest
#' path between the two galaxies using only steps that move up, down, left,
#' or right exactly one `.` or `#` at a time. (The shortest path between
#' two galaxies is allowed to pass through another galaxy.)
#'
#' For example, here is one of the shortest paths between galaxies `5` and
#' `9`:
#'
#'     ....1........
#'     .........2...
#'     3............
#'     .............
#'     .............
#'     ........4....
#'     .5...........
#'     .##.........6
#'     ..##.........
#'     ...##........
#'     ....##...7...
#'     8....9.......
#'
#' This path has length *`9`* because it takes a minimum of *nine steps* to
#' get from galaxy `5` to galaxy `9` (the eight locations marked `#` plus
#' the step onto galaxy `9` itself). Here are some other example shortest
#' path lengths:
#'
#' -   Between galaxy `1` and galaxy `7`: 15
#' -   Between galaxy `3` and galaxy `6`: 17
#' -   Between galaxy `8` and galaxy `9`: 5
#'
#' In this example, after expanding the universe, the sum of the shortest
#' path between all 36 pairs of galaxies is *`374`*.
#'
#' Expand the universe, then find the length of the shortest path between
#' every pair of galaxies. *What is the sum of these lengths?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f11a(x)` returns .... For Part Two,
#'   `f11b(x)` returns ....
#' @export
#' @examples
#' f11a(example_data_11())
#' f11b(example_data_11())
f11a <- function(x) {
  parsed_galaxy <- parse_galaxy_v2(x, expansion = 2)
  # find galaxy locations, '#'
  galaxy_locations <- find_locations(parsed_galaxy)
  # find all paths (too slow)
  galaxy_paths <- find_all_paths_faster(galaxy_locations)
  # find sum of paths
  sum(galaxy_paths$steps)
}


#' @rdname day11
#' @export
f11b <- function(x) {
  parsed_galaxy <- parse_galaxy_v2(x, expansion = 1E6)
  # find galaxy locations, '#'
  galaxy_locations <- find_locations(parsed_galaxy)
  # find all paths (too slow)
  galaxy_paths <- find_all_paths_faster(galaxy_locations)
  # find sum of paths
  sum(galaxy_paths$steps)
}


parse_galaxy <- function(x, expansion = 2) {
  galaxy <- lapply_df(x, \(x) strsplit(x, "")[[1]])
  # expand horizontally
  idx <- which(apply(galaxy, 2, \(x) all(x == ".")))
  galaxy_exp_h <- galaxy[, sort(c(seq_len(ncol(galaxy)),
                                  rep(idx, times = expansion - 1)))]
  # expand vertically
  idx <- which(apply(galaxy_exp_h, 1, \(x) all(x == ".")))
  galaxy_exp_v <- galaxy_exp_h[sort(c(seq_len(nrow(galaxy_exp_h)),
                                      rep(idx, times = expansion - 1))), ]
  # return expanded galaxy
  return(galaxy_exp_v)
}

parse_galaxy_v2 <- function(x, expansion = 2) {
  galaxy <- lapply_df(x, \(x) strsplit(x, "")[[1]])
  # for large expansions, it's not feasible to repeat rows and cols,
  # instead it would be better to note the positions and add an offset
  # expand horizontally (offset)
  idx <- which(apply(galaxy, 2, \(x) all(x == ".")))
  if (length(idx) > 0) {
    galaxy_mapping_h <- data.frame(
      idx = idx,
      offset = expansion,
      direction = "h"
    )
  } else {
    galaxy_mapping_h <- data.frame()
  }
  # expand vertically
  idx <- which(apply(galaxy, 1, \(x) all(x == ".")))
  if (length(idx) > 0) {
    galaxy_mapping_v <- data.frame(
      idx = idx,
      offset = expansion,
      direction = "v"
    )
  } else {
    galaxy_mapping_v <- data.frame()
  }
  list(
    galaxy = galaxy,
    expansion = lapply_df(list(galaxy_mapping_h, galaxy_mapping_v), \(x) x)
  )
}

shortest_path <- function(a, b) {
  # galaxies in the same row
  if (a$i == b$i) {
    return(abs(a$j - b$j))
  } else if (a$j == b$j) { # galaxies in the same column
    return(abs(a$i - b$i))
  } else { # galaxies in different row and column
    return(abs(a$i - b$i) + abs(a$j - b$j))
  }
}

find_locations <- function(x) {
  locations_ind <- which(x$galaxy == "#")
  # find matrix indices
  locations_df <- lapply_df(locations_ind, ind2sub, x = x$galaxy)
  # include expansion / offset
  ## expand horizontally
  if ("h" %in% x$expansion$direction) {
    idx_j <- x$expansion$idx[x$expansion$direction == "h"]
    for (j in rev(idx_j)) {
      locations_df$j[locations_df$j > j] <-
        locations_df$j[locations_df$j > j] + x$expansion$offset[1] - 1
    }
  }
  ## expand vertically
  if ("v" %in% x$expansion$direction) {
    idx_i <- x$expansion$idx[x$expansion$direction == "v"]
    for (i in rev(idx_i)) {
      locations_df$i[locations_df$i > i] <-
        locations_df$i[locations_df$i > i] + x$expansion$offset[1] - 1
    }
  }
  locations_df <- locations_df[order(locations_df$i, locations_df$j), ]
  locations_df$id <- seq_along(locations_ind)
  return(locations_df)
}

find_intermediate_paths <- function(x) {
  # by row
  aux_x <- table(x$i)
  x_sub_x <- lapply_df(as.numeric(names(aux_x[aux_x > 1])),
                     \(i) x[x$i == i, ])

  lapply_df(unique(x_sub_x$i),
            \(i) find_all_paths(x_sub_x[x_sub_x$i == i, ]))
  # by column
  aux_y <- table(x$j)
  x_sub_y <- lapply_df(as.numeric(names(aux_y[aux_y > 1])),
                     \(j) x[x$j == j, ])
  lapply_df(unique(x_sub_y$j),
            \(j) find_all_paths(x_sub_y[x_sub_y$j == j, ]))
}

find_all_paths <- function(x) {
  all_paths <- lapply_df(x$id, function(i) {
    lapply_df(x$id[x$id > i], function(j) {
      steps <- shortest_path(x[x$id == i, ],
                             x[x$id == j, ])
      data.frame(id_i = i, id_j = j, steps = steps)
    })
  })
  return(all_paths)
}

find_all_paths_faster <- function(x) {
  idx <- expand.grid(i = x$id, j = x$id)
  idx <- idx[idx$i > idx$j, ]
  rownames(idx) <- seq_len(nrow(idx))
  lapply_df(seq_len(nrow(idx)), function(k) {
    steps <- shortest_path(x[x$id == idx$i[k], ],
                           x[x$id == idx$j[k], ])
    data.frame(id_i = idx$i[k], id_j = idx$j[k], steps = steps)
  })
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day11
#' @export
example_data_11 <- function(example = 1) {
  l <- list(
    a = c(
      "...#......",
      ".......#..",
      "#.........",
      "..........",
      "......#...",
      ".#........",
      ".........#",
      "..........",
      ".......#..",
      "#...#....."
    )
  )
  l[[example]]
}
