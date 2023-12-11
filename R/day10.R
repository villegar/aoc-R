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
#' f10a(example_data_10())
#' f10b()
f10a <- function(x) {
  field_tiles <- lapply_df(x, \(x) strsplit(x, split = "")[[1]])
  position_mat <- matrix(FALSE, nrow = nrow(field_tiles), ncol = ncol(field_tiles))
  ref_connections <- valid_connections()
  start_pos <- ind2sub(field_tiles, )
  navigate_pipe(x, field_tiles, chr = "S", which(field_tiles == "S"), ref_connections)
}

navigate_pipe <- function(x, visited, chr, pos, ref) {
  aux <- get_adjacent_elements(x, pos$i, pos$j)
  validate <- ref[, colnames(ref) == chr]

  sapply(matrix(aux, nrow = 1), \(x) validate[which(names(validate) == x)])
  names(validate) %in% matrix(aux, nrow = 1)
  apply(, 1, \(x) x == names(validate))
}

valid_connections <- function() {
  mat <- upper.tri(matrix(FALSE, nrow = 8, ncol = 8))
  mat[1, 1] <- mat[2, 2] <- mat[7, 7] <- mat[8, 8] <- TRUE
  mat[1, 2] <- mat[, 7] <- mat[7, 8] <- FALSE
  colnames(mat) <- c("|", "-", "L", "J", "7", "F", ".", "S")
  rownames(mat) <- c("|", "-", "L", "J", "7", "F", ".", "S")
  return(mat)
}

#' @rdname day10
#' @export
f10b <- function(x) {

}


f10_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day10
#' @export
example_data_10 <- function(example = 1) {
  l <- list(
    a = c(".....", ".S-7.", ".|.|.", ".L-J.", "....."),
    b = c("..F7.", ".FJ|.", "SJ.L7", "|F--J", "LJ...")
  )
  l[[example]]
}
