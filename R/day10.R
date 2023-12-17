#' Day 10: Pipe Maze
#'
#' [Pipe Maze](https://adventofcode.com/2023/day/10)
#'
#' @name day10
#' @rdname day10
#' @details
#'
#' **Part One**
#'
#' You use the hang glider to ride the hot air from Desert Island all the
#' way up to the floating metal island. This island is surprisingly cold
#' and there definitely aren\'t any thermals to glide on, so you leave your
#' hang glider behind.
#'
#' You wander around for a while, but you don\'t find any people or
#' animals. However, you do occasionally find signposts labeled \"[Hot
#' Springs](https://en.wikipedia.org/wiki/Hot_spring){target="_blank"}\"
#' pointing in a seemingly consistent direction; maybe you can find someone
#' at the hot springs and ask them where the desert-machine parts are made.
#'
#' The landscape here is alien; even the flowers and trees are made of
#' metal. As you stop to admire some metal grass, you notice something
#' metallic scurry away in your peripheral vision and jump into a big pipe!
#' It didn\'t look like any animal you\'ve ever seen; if you want a better
#' look, you\'ll need to get ahead of it.
#'
#' Scanning the area, you discover that the entire field you\'re standing
#' on is [densely packed with
#' pipes]{title="Manufactured by Hamilton and Hilbert Pipe Company"}; it
#' was hard to tell at first because they\'re the same metallic silver
#' color as the \"ground\". You make a quick sketch of all of the surface
#' pipes you can see (your puzzle input).
#'
#' The pipes are arranged in a two-dimensional grid of *tiles*:
#'
#' -   `|` is a *vertical pipe* connecting north and south.
#' -   `-` is a *horizontal pipe* connecting east and west.
#' -   `L` is a *90-degree bend* connecting north and east.
#' -   `J` is a *90-degree bend* connecting north and west.
#' -   `7` is a *90-degree bend* connecting south and west.
#' -   `F` is a *90-degree bend* connecting south and east.
#' -   `.` is *ground*; there is no pipe in this tile.
#' -   `S` is the *starting position* of the animal; there is a pipe on
#'     this tile, but your sketch doesn\'t show what shape the pipe has.
#'
#' Based on the acoustics of the animal\'s scurrying, you\'re confident the
#' pipe that contains the animal is *one large, continuous loop*.
#'
#' For example, here is a square loop of pipe:
#'
#'     .....
#'     .F-7.
#'     .|.|.
#'     .L-J.
#'     .....
#'
#' If the animal had entered this loop in the northwest corner, the sketch
#' would instead look like this:
#'
#'     .....
#'     .S-7.
#'     .|.|.
#'     .L-J.
#'     .....
#'
#' In the above diagram, the `S` tile is still a 90-degree `F` bend: you
#' can tell because of how the adjacent pipes connect to it.
#'
#' Unfortunately, there are also many pipes that *aren\'t connected to the
#' loop*! This sketch shows the same loop as above:
#'
#'     -L|F7
#'     7S-7|
#'     L|7||
#'     -L-J|
#'     L|-JF
#'
#' In the above diagram, you can still figure out which pipes form the main
#' loop: they\'re the ones connected to `S`, pipes those pipes connect to,
#' pipes *those* pipes connect to, and so on. Every pipe in the main loop
#' connects to its two neighbors (including `S`, which will have exactly
#' two pipes connecting to it, and which is assumed to connect back to
#' those two pipes).
#'
#' Here is a sketch that contains a slightly more complex main loop:
#'
#'     ..F7.
#'     .FJ|.
#'     SJ.L7
#'     |F--J
#'     LJ...
#'
#' Here\'s the same example sketch with the extra, non-main-loop pipe tiles
#' also shown:
#'
#'     7-F7-
#'     .FJ|7
#'     SJLL7
#'     |F--J
#'     LJ.LJ
#'
#' If you want to *get out ahead of the animal*, you should find the tile
#' in the loop that is *farthest* from the starting position. Because the
#' animal is in the pipe, it doesn\'t make sense to measure this by direct
#' distance. Instead, you need to find the tile that would take the longest
#' number of steps *along the loop* to reach from the starting point -
#' regardless of which way around the loop the animal went.
#'
#' In the first example with the square loop:
#'
#'     .....
#'     .S-7.
#'     .|.|.
#'     .L-J.
#'     .....
#'
#' You can count the distance each tile in the loop is from the starting
#' point like this:
#'
#'     .....
#'     .012.
#'     .1.3.
#'     .234.
#'     .....
#'
#' In this example, the farthest point from the start is *`4`* steps away.
#'
#' Here\'s the more complex loop again:
#'
#'     ..F7.
#'     .FJ|.
#'     SJ.L7
#'     |F--J
#'     LJ...
#'
#' Here are the distances for each tile on that loop:
#'
#'     ..45.
#'     .236.
#'     01.78
#'     14567
#'     23...
#'
#' Find the single giant loop starting at `S`. *How many steps along the
#' loop does it take to get from the starting position to the point
#' farthest from the starting position?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f10a(x)` returns .... For Part Two,
#'   `f10b(x)` returns ....
#' @export
#' @examples
#' f10a(example_data_10(1))
#' f10a(example_data_10(2))
#' f10a(example_data_10(3))
#' f10b(example_data_10(4))
#' f10b(example_data_10(5))
f10a <- function(x) {
  field_tiles <- lapply_df(x, \(x) strsplit(x, split = "")[[1]])
  tiles <- navigate_pipe(field_tiles)
  length(tiles) / 2
}

#' @rdname day10
#' @export
f10b <- function(x) {
  field_tiles <- lapply_df(x, \(x) strsplit(x, split = "")[[1]])
  tiles <- navigate_pipe(field_tiles)
  tiles_df <- lapply_df(tiles, \(x) x)
  # Use the Shoelace formula to find the area of the polygon given by the
  # vertices found: https://en.wikipedia.org/wiki/Shoelace_formula
  total_area <- shoelace(tiles_df$i, tiles_df$j)
  # use Pick's theorem to calculate the internal area
  # https://en.wikipedia.org/wiki/Pick%27s_theorem
  # given that the total area can be calculated with
  abs(total_area - nrow(tiles_df) / 2 + 1)
}

shoelace <- function(x, y) {
  idx_x <- seq_along(x)
  idx_y <- c(seq_along(y)[-1], 1)
  sign <- c(1, rep(-1, length(x) - 1))
  abs(sum(x[idx_x] * y[idx_y] - x[idx_y] * y[idx_x])) / 2
}

find_enclosed_areas <- function(x, k = 5) {
  out <- list()
  for (i in seq_len(nrow(x))) {
    for (j in seq_len(ncol(x))) {
      if (x[i, j]) {
        out <- c(out, NA)
        next
      }
      # extract adjacent elements
      idx_x <- get_indices(i, nrow(x), k = k)
      idx_y <- get_indices(j, ncol(x), k = k)
      aux <- x[idx_x, idx_y]
      out <- c(out, list(aux))
    }
  }

  new_x <- matrix(".", nrow = nrow(x), ncol = ncol(x))
  for (i in seq_along(out)) {
    if (is.na(out[i]))
      next
    aux <- out[i][[1]]
    if (sum(aux != ".") < 2)
      next
    # retrieve element positions
    idx_x <- as.numeric(rownames(aux))
    idx_y <- as.numeric(colnames(aux))
    idx <- new_x[idx_x, idx_y] == "." & aux != "."
    new_x[idx_x, idx_y][idx] <- aux[idx]
  }
  return(new_x)
}
simplify_field <- function(x, k = 1) {
  out <- list()
  for (i in seq_len(nrow(x))) {
    for (j in seq_len(ncol(x))) {
      chr <- x[i, j]
      if (chr == ".") {
        out <- c(out, NA)
        next
      }
      pos <- data.frame(i = i, j = j)
      out <- c(out,
               list(validate_connections(x, list(x = x), chr, pos, FALSE, k)))
    }
  }

  new_x <- matrix(".", nrow = nrow(x), ncol = ncol(x))
  for (i in seq_along(out)) {
    if (is.na(out[i]))
      next
    aux <- out[i][[1]]
    if (sum(aux != ".") < 2)
      next
    # retrieve element positions
    idx_x <- as.numeric(rownames(aux))
    idx_y <- as.numeric(colnames(aux))
    idx <- new_x[idx_x, idx_y] == "." & aux != "."
    new_x[idx_x, idx_y][idx] <- aux[idx]
  }
  return(new_x)
}

get_next_tiles <- function(env, chr, pos, all = FALSE) {
  # get tiles for starting position
  aux <- validate_connections(env$x, env, chr, pos, FALSE)
  # find which adjacent elements are a valid connection
  idx <- sapply(matrix(aux, ncol = 1), \(x) x != ".")
  # find new indices in reference to original map, x
  idx_x <- get_indices(pos$i, nrow(env$x))
  idx_y <- get_indices(pos$j, ncol(env$x))
  aux_2 <- expand.grid(i = idx_x, j = idx_y)
  aux_2 <- aux_2[order(aux_2$j, aux_2$i), ]
  new_indices <- aux_2[idx, ]

  # record visited position
  env$visited[pos$i, pos$j] <- TRUE

  if (all)
    return(new_indices)

  # check visited elements
  idx <- sapply(seq_len(nrow(new_indices)), function (i) {
    env$visited[new_indices$i[i], new_indices$j[i]]
  })
  new_indices[!idx, ]
}

navigate_pipe <- function(x) {
  start_pos <- ind2sub(x, which(x == "S"))
  env <- new.env()
  assign("visited", matrix(FALSE, nrow = nrow(x), ncol = ncol(x)), env)
  assign("x", x, env)

  new_indices <- get_next_tiles(env, "S", start_pos)
  solution_found <- FALSE
  sub_env <- env
  indices <- list(start_pos)
  for (i in seq_len(nrow(new_indices))) {
    steps <- 1
    indices <- list(start_pos, new_indices[i, ])
    new_tiles <- get_next_tiles(sub_env,
                                chr = x[new_indices$i[i], new_indices$j[i]],
                                pos = new_indices[i, ])
    if (nrow(new_tiles) < 1)
      next
    chr <- x[new_tiles$i[1], new_tiles$j[1]]
    pos <- new_tiles[1, ]
    while(TRUE) {
      steps <- steps + 1
      indices <- c(indices, list(new_tiles[1, ]))
      if (chr == "S") {
        solution_found <- TRUE
        break
      }
      aux <- get_next_tiles(sub_env,
                            chr = chr,
                            pos = new_tiles[1, ])
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
    aux <- get_next_tiles(env,
                          chr = chr,
                          pos = pos,
                          all = TRUE)
    if (any(start_pos$i == aux$i & start_pos$j == aux$j)) {
      steps <- steps + 1
      break
    }
  }

  return(indices)
}

validate_connections <- function(x, env, chr, pos, remove_original = TRUE, k = 1) {
  # extract adjacent elements
  idx_x <- get_indices(pos$i, nrow(env$x), k = k)
  idx_y <- get_indices(pos$j, ncol(env$x), k = k)
  if (remove_original)
    env$x[pos$i, pos$j] <- "."
  aux <- env$x[idx_x, idx_y]
  # replace diagonals (if any)
  aux[!(idx_x %in% pos$i), !(idx_y %in% pos$j)] <- "."
  # add column and row names (relative positions)
  colnames(aux) <- idx_y
  rownames(aux) <- idx_x

  if (chr == "S")
    return(aux)
  if (chr == "|") {
    # replace connections to the left and right
    aux[, idx_y != pos$j] <- "."
    # validate top (|, 7, F)
    idx <- aux[idx_x < pos$i, ] %in% c("|", "7", "F", "S")
    aux[idx_x < pos$i, !idx] <- "."
    # validate bottom (|, L, J)
    idx <- aux[idx_x > pos$i, ] %in% c("|", "L", "J", "S")
    aux[idx_x > pos$i, !idx] <- "."
  } else if (chr == "-") {
    # replace connections from the top and bottom
    aux[idx_x != pos$i, ] <- "."
    # validate left (-, L, F)
    idx <- aux[, idx_y < pos$j] %in% c("-", "L", "F", "S")
    aux[!idx, idx_y < pos$j] <- "."
    # validate right (-, J, 7)
    idx <- aux[, idx_y > pos$j] %in% c("-", "J", "7", "S")
    aux[!idx, idx_y > pos$j] <- "."
  } else if (chr == "F") {
    # replace connections from the top and left
    aux[idx_x < pos$i, ] <- "."
    aux[, idx_y < pos$j] <- "."
    # validate right (-, J, 7)
    idx <- aux[, idx_y > pos$j] %in% c("-", "J", "7", "S")
    aux[!idx, idx_y > pos$j] <- "."
    # validate bottom (|, L, J)
    idx <- aux[idx_x > pos$i, ] %in% c("|", "L", "J", "S")
    aux[idx_x > pos$i, !idx] <- "."
  } else if (chr == "7") {
    # replace connections from the top and right
    aux[idx_x < pos$i, ] <- "."
    aux[, idx_y > pos$j] <- "."
    # validate left (-, L, F)
    idx <- aux[, idx_y < pos$j] %in% c("-", "L", "F", "S")
    aux[!idx, idx_y < pos$j] <- "."
    # validate bottom (|, L, J)
    idx <- aux[idx_x > pos$i, ] %in% c("|", "L", "J", "S")
    aux[idx_x > pos$i, !idx] <- "."
  } else if (chr == "J") {
    # replace connections from the bottom and right
    aux[idx_x > pos$i, ] <- "."
    aux[, idx_y > pos$j] <- "."
    # validate left (-, L, F)
    idx <- aux[, idx_y < pos$j] %in% c("-", "L", "F", "S")
    aux[!idx, idx_y < pos$j] <- "."
    # validate top (|, 7, F)
    idx <- aux[idx_x < pos$i, ] %in% c("|", "7", "F", "S")
    aux[idx_x < pos$i, !idx] <- "."
  } else if (chr == "L") {
    # replace connections from the bottom and left
    aux[idx_x > pos$i, ] <- "."
    aux[, idx_y < pos$j] <- "."
    # validate right (-, J, 7)
    idx <- aux[, idx_y > pos$j] %in% c("-", "J", "7", "S")
    aux[!idx, idx_y > pos$j] <- "."
    # validate top (|, 7, F)
    idx <- aux[idx_x < pos$i, ] %in% c("|", "7", "F", "S")
    aux[idx_x < pos$i, !idx] <- "."
  } else {
    aux <- data.frame()
  }
  return(aux)
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day10
#' @export
example_data_10 <- function(example = 1) {
  l <- list(
    a1 = c(".....", ".S-7.", ".|.|.", ".L-J.", "....."),
    a2 = c("-L|F7", "7S-7|", "L|7||", "-L-J|", "L|-JF"),
    a3 = c("..F7.", ".FJ|.", "SJ.L7", "|F--J", "LJ..."),
    b1 = c(
      "...........",
      ".S-------7.",
      ".|F-----7|.",
      ".||.....||.",
      ".||.....||.",
      ".|L-7.F-J|.",
      ".|..|.|..|.",
      ".L--J.L--J.",
      "..........."
    ),
    b2 = c(
      ".F----7F7F7F7F-7....",
      ".|F--7||||||||FJ....",
      ".||.FJ||||||||L7....",
      "FJL7L7LJLJ||LJ.L-7..",
      "L--J.L7...LJS7F-7L7.",
      "....F-J..F7FJ|L7L7L7",
      "....L7.F7||L7|.L7L7|",
      ".....|FJLJ|FJ|F7|.LJ",
      "....FJL-7.||.||||...",
      "....L---J.LJ.LJLJ..."
    )
  )
  l[[example]]
}
