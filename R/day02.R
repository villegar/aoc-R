#' Day 02: Cube Conundrum
#'
#' [Cube Conundrum](https://adventofcode.com/2023/day/2)
#'
#' @name day02
#' @rdname day02
#' @details
#'
#' **Part One**
#'
#' You\'re launched high into the atmosphere! The apex of your trajectory
#' just barely reaches the surface of a large island floating in the sky.
#' You gently land in a fluffy pile of leaves. It\'s quite cold, but you
#' don\'t see much snow. An Elf runs over to greet you.
#'
#' The Elf explains that you\'ve arrived at *Snow Island* and apologizes
#' for the lack of snow. He\'ll be happy to explain the situation, but
#' it\'s a bit of a walk, so you have some time. They don\'t get many
#' visitors up here; [would you like to play a
#' game]{title="No, the Elf's name is not 'WOPR'. It's Joshua."} in the
#' meantime?
#'
#' As you walk, the Elf shows you a small bag and some cubes which are
#' either red, green, or blue. Each time you play this game, he will hide a
#' secret number of cubes of each color in the bag, and your goal is to
#' figure out information about the number of cubes.
#'
#' To get information, once a bag has been loaded with cubes, the Elf will
#' reach into the bag, grab a handful of random cubes, show them to you,
#' and then put them back in the bag. He\'ll do this a few times per game.
#'
#' You play several games and record the information from each game (your
#' puzzle input). Each game is listed with its ID number (like the `11` in
#' `Game 11: ...`) followed by a semicolon-separated list of subsets of
#' cubes that were revealed from the bag (like `3 red, 5 green, 4 blue`).
#'
#' For example, the record of a few games might look like this:
#'
#'     Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
#'     Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
#'     Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
#'     Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
#'     Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
#'
#' In game 1, three sets of cubes are revealed from the bag (and then put
#' back again). The first set is 3 blue cubes and 4 red cubes; the second
#' set is 1 red cube, 2 green cubes, and 6 blue cubes; the third set is
#' only 2 green cubes.
#'
#' The Elf would first like to know which games would have been possible if
#' the bag contained *only 12 red cubes, 13 green cubes, and 14 blue
#' cubes*?
#'
#' In the example above, games 1, 2, and 5 would have been *possible* if
#' the bag had been loaded with that configuration. However, game 3 would
#' have been *impossible* because at one point the Elf showed you 20 red
#' cubes at once; similarly, game 4 would also have been *impossible*
#' because the Elf showed you 15 blue cubes at once. If you add up the IDs
#' of the games that would have been possible, you get *`8`*.
#'
#' Determine which games would have been possible if the bag had been
#' loaded with only 12 red cubes, 13 green cubes, and 14 blue cubes. *What
#' is the sum of the IDs of those games?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @param n_red Integer with the maximum number of red cubes.
#' @param n_green Integer with the maximum number of green cubes.
#' @param n_blue Integer with the maximum number of blue cubes.
#' @return For Part One, `f02a(x)` returns .... For Part Two,
#'   `f02b(x)` returns ....
#' @export
#' @examples
#' f02a(example_data_02())
#' f02b(example_data_02())
f02a <- function(x, n_red = 12, n_green = 13, n_blue = 14) {
  limits_df <- data.frame(
    colour = c("red", "green", "blue"),
    max_count = c(n_red, n_green, n_blue)
  )

  # parse each game into data frames
  parsed_games <- lapply(x, parse_game)
  # validate the hands in each game
  validated_hands <- lapply(parsed_games, validate_hands, limits_df = limits_df)
  # find which games are valid with the given configuration
  valid_games <- sapply(validated_hands, all)
  # find sum of game IDs
  sum(which(valid_games), na.rm = TRUE)
}


#' @rdname day02
#' @export
f02b <- function(x) {
  # parse each game into data frames
  parsed_games <- lapply(x, parse_game)
  # find minimum sets
  minimum_sets <- lapply(parsed_games, find_minimum_set)
  # find the product of the values
  power_of_sets <- sapply(minimum_sets, function(x) {
    prod(as.numeric(x$count), na.rm = TRUE)
  })
  # find sum of game IDs
  sum(power_of_sets, na.rm = TRUE)
}

#' Parse string with game details, see
#' [day 2 - 2023](https://adventofcode.com/2023/day/2)
#'
#' @param x String with game description.
#'
#' @return Data frame with game details.
#' @export
#'
#' @examples
#' parse_game("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
parse_game <- function(x) {
  # separate game ID and draw records
  aux <- strsplit(x, ":", 2)[[1]]
  game_id <- gsub("\\D", "", aux[1])
  # separate each draw record
  hands <- strsplit(aux[2], ";")[[1]]
  # convert the parse cubes into a data frame
  hands_list <- lapply(hands, parse_cubes)
  # add hand_id
  hands_list_2 <- lapply(seq_along(hands_list), function(i) {
    hands_list[[i]]$hand_id <- i
    return(hands_list[[i]])
  })

  # combine all the hands & add game_id
  out <- as.data.frame(do.call(rbind, hands_list_2))
  out$game_id <- game_id

  class(out) <- c("data.frame", "game")
  return(out)
}

#' Parse string with game details, see
#' [day 2 - 2023](https://adventofcode.com/2023/day/2)
#'
#' @param x String with game description.
#'
#' @return Data frame with game details (cubes)
#' @export
#'
#' @examples
#' parse_cubes("3 blue, 4 red")
parse_cubes <- function(x) {
  # split the hand record
  cubes <- strsplit(x, ",")[[1]]
  # create data frame with colour and number of cubes
  cubes_list <- lapply(cubes, function(c) {
    aux <- strsplit(trimws(c), " ", fixed = 2)[[1]]
    list(
      colour = aux[2],
      count = as.numeric(aux[1])
    )
  })
  # combine records
  out <- as.data.frame(do.call(rbind, cubes_list))
  class(out) <- c("data.frame", "hand")
  return(out)
}

#' Validate hand based on game configuration, see
#' [day 2 - 2023](https://adventofcode.com/2023/day/2)
#'
#' @param x Data frame with game details.
#' @param limits_df Data frame with game configuration.
#'
#' @return Vector of `TRUE` and `FALSE` values linked to each hand
#' @export
#'
#' @examples
#' demo <- parse_game("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
#' limits_df <- data.frame(
#'   colour = c("red", "green", "blue"),
#'   max_count = c(12, 13, 14)
#' )
#' validate_hands(demo, limits_df)
validate_hands <- function(x, limits_df) {
  sapply(unique(x$hand_id), function(id) {
    aux <- x[x$hand_id == id, ]
    aux_2 <- merge(aux, limits_df, by = "colour", sort = FALSE)
    all(aux_2$count <= aux_2$max_count)
  })
}

#' Find the minimum set of cubes for a game, see
#' [day 2 - 2023](https://adventofcode.com/2023/day/2/#part2)
#'
#' @param x Data frame with game details.
#'
#' @return Data frame with minim set of cubes required.
#' @export
#'
#' @examples
#' demo <- parse_game("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
#' find_minimum_set(demo)
find_minimum_set <- function(x) {
  # find the maximum cube counts across all hands for each colour
  aux <- by(x, as.character(x$colour), function(x) x[which.max(x$count), ])
  # combine the previous result into a data frame
  do.call(rbind, aux)
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day02
#' @export
example_data_02 <- function(example = 1) {
  l <- list(
    a = c(
      "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
      "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
      "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
      "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
      "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    )
  )
  l[[example]]
}
