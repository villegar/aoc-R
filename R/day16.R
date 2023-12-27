#' Day 16: The Floor Will Be Lava
#'
#' [The Floor Will Be Lava](https://adventofcode.com/2023/day/16)
#'
#' @name day16
#' @rdname day16
#' @details
#'
#' **Part One**
#'
#' With the beam of light completely focused *somewhere*, the reindeer
#' leads you deeper still into the Lava Production Facility. At some point,
#' you realize that the steel facility walls have been replaced with cave,
#' and the doorways are just cave, and the floor is cave, and you\'re
#' pretty sure this is actually just a giant cave.
#'
#' Finally, as you approach what must be the heart of the mountain, you see
#' a bright light in a cavern up ahead. There, you discover that the
#' [beam]{title="Not anymore, there's a blanket!"} of light you so
#' carefully focused is emerging from the cavern wall closest to the
#' facility and pouring all of its energy into a contraption on the
#' opposite side.
#'
#' Upon closer inspection, the contraption appears to be a flat,
#' two-dimensional square grid containing *empty space* (`.`), *mirrors*
#' (`/` and `\`), and *splitters* (`|` and `-`).
#'
#' The contraption is aligned so that most of the beam bounces around the
#' grid, but each tile on the grid converts some of the beam\'s light into
#' *heat* to melt the rock in the cavern.
#'
#' You note the layout of the contraption (your puzzle input). For example:
#'
#'     .|...\....
#'     |.-.\.....
#'     .....|-...
#'     ........|.
#'     ..........
#'     .........\
#'     ..../.\\..
#'     .-.-/..|..
#'     .|....-|.\
#'     ..//.|....
#'
#' The beam enters in the top-left corner from the left and heading to the
#' *right*. Then, its behavior depends on what it encounters as it moves:
#'
#' -   If the beam encounters *empty space* (`.`), it continues in the same
#'     direction.
#' -   If the beam encounters a *mirror* (`/` or `\`), the beam is
#'     *reflected* 90 degrees depending on the angle of the mirror. For
#'     instance, a rightward-moving beam that encounters a `/` mirror would
#'     continue *upward* in the mirror\'s column, while a rightward-moving
#'     beam that encounters a `\` mirror would continue *downward* from the
#'     mirror\'s column.
#' -   If the beam encounters the *pointy end of a splitter* (`|` or `-`),
#'     the beam passes through the splitter as if the splitter were *empty
#'     space*. For instance, a rightward-moving beam that encounters a `-`
#'     splitter would continue in the same direction.
#' -   If the beam encounters the *flat side of a splitter* (`|` or `-`),
#'     the beam is *split into two beams* going in each of the two
#'     directions the splitter\'s pointy ends are pointing. For instance, a
#'     rightward-moving beam that encounters a `|` splitter would split
#'     into two beams: one that continues *upward* from the splitter\'s
#'     column and one that continues *downward* from the splitter\'s
#'     column.
#'
#' Beams do not interact with other beams; a tile can have many beams
#' passing through it at the same time. A tile is *energized* if that tile
#' has at least one beam pass through it, reflect in it, or split in it.
#'
#' In the above example, here is how the beam of light bounces around the
#' contraption:
#'
#'     >|<<<\....
#'     |v-.\^....
#'     .v...|->>>
#'     .v...v^.|.
#'     .v...v^...
#'     .v...v^..\
#'     .v../2\\..
#'     <->-/vv|..
#'     .|<<<2-|.\
#'     .v//.|.v..
#'
#' Beams are only shown on empty tiles; arrows indicate the direction of
#' the beams. If a tile contains beams moving in multiple directions, the
#' number of distinct directions is shown instead. Here is the same diagram
#' but instead only showing whether a tile is *energized* (`#`) or not
#' (`.`):
#'
#'     ######....
#'     .#...#....
#'     .#...#####
#'     .#...##...
#'     .#...##...
#'     .#...##...
#'     .#..####..
#'     ########..
#'     .#######..
#'     .#...#.#..
#'
#' Ultimately, in this example, *`46`* tiles become *energized*.
#'
#' The light isn\'t energizing enough tiles to produce lava; to debug the
#' contraption, you need to start by analyzing the current situation. With
#' the beam starting in the top-left heading right, *how many tiles end up
#' being energized?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f16a(x)` returns .... For Part Two,
#'   `f16b(x)` returns ....
#' @export
#' @examples
#' f16a(example_data_16())
#' f16b()
f16a <- function(x) {
  x <- example_data_16()
  # parse contraption
  parsed_contraption <- lapply_df(x, \(x) strsplit(x, "")[[1]])
  # entry point (directions: up = 1, right = 2, down = 3, left = 4)
  # (x, y, z) where z is the direction of travel
  pos <- data.frame(i = 1, j = 1, k = 2)
  trace_beam(parsed_contraption, pos)
}

trace_beam <- function(x, pos) {
  browser()
  # x <- parsed_contraption
  # create new environment with a matrix of energised locations and tiles
  env <- new.env()
  assign("energised", matrix(FALSE, nrow = nrow(x), ncol = ncol(x)), env)
  assign("x", x, env)
  assign("k", pos$k, env)

  # energised_tiles <- list()
  # curr_point <- pos
  # prev_point <- pos
  # # next_points <- get_next_beam_tiles()
  # while (TRUE) {
  #   chr <- x[curr_point$i, curr_point$j]
  #   get_next_beam_tiles(env, chr, curr_point)
  # }

  # retrieve positions connected to the starting point (up, left, down, right)
  new_indices <- get_next_beam_tiles(env, x[pos$i, pos$j], pos)
  solution_found <- FALSE
  sub_env <- env
  indices <- list(pos) # list of visited points
  # loop through the list of points connected to the starting point
  for (i in seq_len(nrow(new_indices))) {
    steps <- 1 # step counter
    indices <- list(pos, new_indices[i, ]) # record indices visited
    # get next tiles from the current position
    new_tiles <-
      get_next_beam_tiles(sub_env,
                          chr = x[new_indices$i[i], new_indices$j[i]],
                          pos = new_indices[i, ]
                          )
    if (nrow(new_tiles) < 1) # are there any new tiles?
      next
    chr <- x[new_tiles$i[1], new_tiles$j[1]]
    pos <- new_tiles[1, ]
    # keep navigating the field until no more connections are found
    while(TRUE) {
      steps <- steps + 1
      indices <- c(indices, list(new_tiles[1, ]))
      # if (chr == "S") { # check if the current position is the starting point
      if (pos$i == new_tiles$i[1] &
          pos$j == new_tiles$j[1]) {
        solution_found <- TRUE
        break
      }
      aux <- get_next_beam_tiles(sub_env, chr = chr, pos = new_tiles[1, ])
      new_tiles <- new_tiles[-1, ]
      new_tiles <- do.call(rbind, list(aux, new_tiles))
      if (nrow(new_tiles) > 0) {
        chr <- x[new_tiles$i[1], new_tiles$j[1]]
        pos <- new_tiles[1, ]
      } else {
        break
      }
    }
    if (solution_found)
      break
    # check if the starting position can be reach by moving one position
    aux <- get_next_beam_tiles(env, chr = chr, pos = pos, all = TRUE)
    if (any(pos$i == aux$i & pos$j == aux$j)) {
      steps <- steps + 1
      break
    }
  }
}

#' Get next beam tiles, see
#' [day 16 - 2023](https://adventofcode.com/2023/day/16)
#'
#' @param env Environment with the current tile, `x`.
#' @param chr Character with current position's element.
#' @param pos Data frame with current position (`i`, `j`).
#' @param all Boolean flag to indicate if all tiles (including energised)
#'     should be returned.
#'
#' @return Data frame with next tile positions.
#' @export
get_next_beam_tiles <- function(env, chr, pos, all = FALSE) {
  # set null symbol
  NULL_symb <- "#"
  # get tiles for near the current position, `pos`
  aux <- validate_reflection(env, chr, pos, FALSE)
  # find which adjacent elements are NOT  ground
  idx <- sapply(matrix(aux, ncol = 1), \(x) x != NULL_symb)
  # find new indices in reference to original map, x
  idx_x <- get_indices(pos$i, nrow(env$x))
  idx_y <- get_indices(pos$j, ncol(env$x))
  aux_2 <- expand.grid(i = idx_x, j = idx_y)
  aux_2 <- aux_2[order(aux_2$j, aux_2$i), ]
  new_indices <- aux_2[idx, ]

  # record energised position
  env$energised[pos$i, pos$j] <- TRUE

  if (all)
    return(new_indices)

  # check energised elements
  idx <- sapply(seq_len(nrow(new_indices)), function (i) {
    env$energised[new_indices$i[i], new_indices$j[i]]
  })
  new_indices[!idx, ]
}

#' Validate beam reflection, see
#' [day 16 - 2023](https://adventofcode.com/2023/day/16)
#'
#' @param env Environment with the current space, `x`.
#' @param chr Character with current position's element.
#' @param pos Data frame with current position (`i`, `j`).
#' @param remove_original Boolean flag to indicate if the current tile should be
#'     removed from the original space (i.e., changed to `NULL_symb`).
#' @param k Integer with the number of `k` nearest neighbours to return.
#'
#' @return Matrix with validated reflections for the current tile.
#' @export
validate_reflection <- function(env, chr, pos, remove_original = TRUE, k = 1) {
  # set null symbol
  NULL_symb <- "#"
  # extract adjacent elements
  idx_x <- get_indices(pos$i, nrow(env$x), k = k)
  idx_y <- get_indices(pos$j, ncol(env$x), k = k)
  if (remove_original)
    env$x[pos$i, pos$j] <- NULL_symb
  aux <- env$x[idx_x, idx_y]
  # replace diagonals (if any)
  aux[!(idx_x %in% pos$i), !(idx_y %in% pos$j)] <- NULL_symb
  # add column and row names (relative positions)
  colnames(aux) <- idx_y
  rownames(aux) <- idx_x

  # if (chr == "S")
  #   return(aux)
  if (chr == "|") {
    # replace connections to the left and right
    aux[, idx_y != pos$j] <- NULL_symb
    if (1 %in% env$k) {
      # replace connections from the bottom
      aux[idx_x > pos$i, ] <- NULL_symb
    } else if (3 %in% env$k) {
      # replace connections from the top
      aux[idx_x < pos$i, ] <- NULL_symb
    } else {
      env$k <- c(2, 4)
    }
  } else if (chr == "-") {
    # replace connections from the top and bottom
    aux[idx_x != pos$i, ] <- NULL_symb
    if (2 %in% env$k) {
      # replace connections from the left
      aux[, idx_y < pos$j] <- NULL_symb
    } else if (4 %in% env$k) {
      # replace connections from the right
      aux[, idx_y > pos$j] <- NULL_symb
    } else {
      env$k <- c(1, 3)
    }
  } else if (chr == "F") {
    # # replace connections from the top and left
    # aux[idx_x < pos$i, ] <- NULL_symb
    # aux[, idx_y < pos$j] <- NULL_symb
  } else if (chr == "7") {
    # # replace connections from the top and right
    # aux[idx_x < pos$i, ] <- NULL_symb
    # aux[, idx_y > pos$j] <- NULL_symb
  } else if (chr == "/") {
    # replace connections from the bottom and right
    aux[idx_x > pos$i, ] <- NULL_symb
    aux[, idx_y > pos$j] <- NULL_symb

    if (2 %in% env$k) {
      # replace connections from the bottom
      aux[idx_x > pos$i, ] <- NULL_symb
      # replace connections from the right and left
      aux[, idx_y != pos$j] <- NULL_symb
      # change direction
      env$k <- 1
    } else if (3 %in% env$k) {
      # replace connections from the bottom and top
      aux[idx_x != pos$i, ] <- NULL_symb
      # replace connections from the right
      aux[, idx_y > pos$j] <- NULL_symb
      # change direction
      env$k <- 4
    } else {
      return(data.frame())
    }
  } else if (chr == "\\") {
    # replace connections from the bottom and left
    aux[idx_x > pos$i, ] <- NULL_symb
    aux[, idx_y < pos$j] <- NULL_symb

    if (4 %in% env$k) {
      # replace connections from the bottom and top
      aux[idx_x != pos$i, ] <- NULL_symb
      # replace connections from the right
      aux[, idx_y > pos$j] <- NULL_symb
      # change direction
      env$k <- 1
    } else if (3 %in% env$k) {
      # replace connections from the bottom and top
      aux[idx_x != pos$i, ] <- NULL_symb
      # replace connections from the left
      aux[, idx_y < pos$j] <- NULL_symb
      # change direction
      env$k <- 2
    } else {
      return(data.frame())
    }
  } else {
    if (1 %in% env$k) {
      # replace connections from the bottom
      aux[idx_x > pos$i, ] <- NULL_symb
      # replace connections from the right and left
      aux[, idx_y != pos$j] <- NULL_symb
    } else if (2 %in% env$k) {
      # replace connections from the bottom and top
      aux[idx_x != pos$i, ] <- NULL_symb
      # replace connections from the left
      aux[, idx_y < pos$j] <- NULL_symb
    } else if (3 %in% env$k) {
      # replace connections from the top
      aux[idx_x < pos$i, ] <- NULL_symb
      # replace connections from the right and left
      aux[, idx_y != pos$j] <- NULL_symb
    } else if (4 %in% env$k) {
      # replace connections from the bottom and top
      aux[idx_x != pos$i, ] <- NULL_symb
      # replace connections from the right
      aux[, idx_y > pos$j] <- NULL_symb
    } else {
      return(data.frame())
    }
  }
  return(aux)
}

#' @rdname day16
#' @export
f16b <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day16
#' @export
example_data_16 <- function(example = 1) {
  l <- list(
    a = c(
      ".|...\\....",
      "|.-.\\.....",
      ".....|-...",
      "........|.",
      "..........",
      ".........\\",
      "..../.\\\\..",
      ".-.-/..|..",
      ".|....-|.\\",
      "..//.|...."
    )
  )
  l[[example]]
}
