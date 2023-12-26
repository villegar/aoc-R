#' Day 15: Lens Library
#'
#' [Lens Library](https://adventofcode.com/2023/day/15)
#'
#' @name day15
#' @rdname day15
#' @details
#'
#' **Part One**
#'
#' The newly-focused parabolic reflector dish is sending all of the
#' collected light to a point on the side of yet another mountain - the
#' largest mountain on Lava Island. As you approach the mountain, you find
#' that the light is being collected by the wall of a large facility
#' embedded in the mountainside.
#'
#' You find a door under a large sign that says \"Lava Production
#' Facility\" and next to a smaller sign that says \"Danger - Personal
#' Protective Equipment required beyond this point\".
#'
#' As you step inside, you are immediately greeted by a somewhat panicked
#' [reindeer]{title="do you like my hard hat"} wearing goggles and a
#' loose-fitting [hard
#' hat](https://en.wikipedia.org/wiki/Hard_hat){target="_blank"}. The
#' reindeer leads you to a shelf of goggles and hard hats (you quickly find
#' some that fit) and then further into the facility. At one point, you
#' pass a button with a faint snout mark and the label \"PUSH FOR HELP\".
#' No wonder you were loaded into that [trebuchet](1) so quickly!
#'
#' You pass through a final set of doors surrounded with even more warning
#' signs and into what must be the room that collects all of the light from
#' outside. As you admire the large assortment of lenses available to
#' further focus the light, the reindeer brings you a book titled
#' \"Initialization Manual\".
#'
#' \"Hello!\", the book cheerfully begins, apparently unaware of the
#' concerned reindeer reading over your shoulder. \"This procedure will let
#' you bring the Lava Production Facility online - all without burning or
#' melting anything unintended!\"
#'
#' \"Before you begin, please be prepared to use the Holiday ASCII String
#' Helper algorithm (appendix 1A).\" You turn to appendix 1A. The reindeer
#' leans closer with interest.
#'
#' The HASH algorithm is a way to turn any
#' [string](https://en.wikipedia.org/wiki/String_(computer_science)){target="_blank"}
#' of characters into a single *number* in the range 0 to 255. To run the
#' HASH algorithm on a string, start with a *current value* of `0`. Then,
#' for each character in the string starting from the beginning:
#'
#' -   Determine the [ASCII
#'     code](https://en.wikipedia.org/wiki/ASCII#Printable_characters){target="_blank"}
#'     for the current character of the string.
#' -   Increase the *current value* by the ASCII code you just determined.
#' -   Set the *current value* to itself multiplied by `17`.
#' -   Set the *current value* to the
#'     [remainder](https://en.wikipedia.org/wiki/Modulo){target="_blank"}
#'     of dividing itself by `256`.
#'
#' After following these steps for each character in the string in order,
#' the *current value* is the output of the HASH algorithm.
#'
#' So, to find the result of running the HASH algorithm on the string
#' `HASH`:
#'
#' -   The *current value* starts at `0`.
#' -   The first character is `H`; its ASCII code is `72`.
#' -   The *current value* increases to `72`.
#' -   The *current value* is multiplied by `17` to become `1224`.
#' -   The *current value* becomes *`200`* (the remainder of `1224` divided
#'     by `256`).
#' -   The next character is `A`; its ASCII code is `65`.
#' -   The *current value* increases to `265`.
#' -   The *current value* is multiplied by `17` to become `4505`.
#' -   The *current value* becomes *`153`* (the remainder of `4505` divided
#'     by `256`).
#' -   The next character is `S`; its ASCII code is `83`.
#' -   The *current value* increases to `236`.
#' -   The *current value* is multiplied by `17` to become `4012`.
#' -   The *current value* becomes *`172`* (the remainder of `4012` divided
#'     by `256`).
#' -   The next character is `H`; its ASCII code is `72`.
#' -   The *current value* increases to `244`.
#' -   The *current value* is multiplied by `17` to become `4148`.
#' -   The *current value* becomes *`52`* (the remainder of `4148` divided
#'     by `256`).
#'
#' So, the result of running the HASH algorithm on the string `HASH` is
#' *`52`*.
#'
#' The *initialization sequence* (your puzzle input) is a comma-separated
#' list of steps to start the Lava Production Facility. *Ignore newline
#' characters* when parsing the initialization sequence. To verify that
#' your HASH algorithm is working, the book offers the sum of the result of
#' running the HASH algorithm on each step in the initialization sequence.
#'
#' For example:
#'
#'     rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7
#'
#' This initialization sequence specifies 11 individual steps; the result
#' of running the HASH algorithm on each of the steps is as follows:
#'
#' -   `rn=1` becomes *`30`*.
#' -   `cm-` becomes *`253`*.
#' -   `qp=3` becomes *`97`*.
#' -   `cm=2` becomes *`47`*.
#' -   `qp-` becomes *`14`*.
#' -   `pc=4` becomes *`180`*.
#' -   `ot=9` becomes *`9`*.
#' -   `ab=5` becomes *`197`*.
#' -   `pc-` becomes *`48`*.
#' -   `pc=6` becomes *`214`*.
#' -   `ot=7` becomes *`231`*.
#'
#' In this example, the sum of these results is *`1320`*. Unfortunately,
#' the reindeer has stolen the page containing the expected verification
#' number and is currently running around the facility with it excitedly.
#'
#' Run the HASH algorithm on each step in the initialization sequence.
#' *What is the sum of the results?* (The initialization sequence is one
#' long line; be careful when copy-pasting it.)
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f15a(x)` returns .... For Part Two,
#'   `f15b(x)` returns ....
#' @export
#' @examples
#' f15a(example_data_15())
#' f15b()
f15a <- function(x) {
  parsed_sequence <- sapply(strsplit(x, ",")[[1]],
                            \(x) strsplit(x, "")[[1]])
  sum(sapply(parsed_sequence, get_mapping), na.rm = TRUE)
}

get_hash <- function(x, offset = 0, max = 256, mult = 17) {
  ((utf8ToInt(x) + offset) * mult) %% max
}

get_mapping <- function(x, offset = 0, max = 256, mult = 17) {
  for (i in seq_along(x)) {
    offset <- get_hash(x[i], offset, max, mult)
  }
  return(offset)
}

#' @rdname day15
#' @export
f15b <- function(x) {
  parsed_sequence <- sapply(strsplit(x, ",")[[1]],
                            \(x) strsplit(x, "")[[1]])
  mapped_boxes <- map_boxes(parsed_sequence)
  boxes_power <- focus_power(mapped_boxes)
  sum(boxes_power$power, na.rm = TRUE)
}

map_boxes <- function(x) {
  hsh_tb <- hashtab(type = c("identical", "address"), 256)
  for (i in seq_along(x)) {
    if (any(x[i][[1]] == "=")) { # add/replace [label lens]
      label <- x[i][[1]][1:2]
      lens <- x[i][[1]][4]
      key <- get_mapping(label)
      contents <- gethash(hsh_tb, key, nomatch = NULL)
      if (is.null(contents)) { # empty box
        sethash(hsh_tb, key, new_box(label, lens))
      } else { # box has contents
        # check if the current label is already in the box
        if (find_label_lens(contents, label)) {
          sethash(hsh_tb,
                  key,
                  replace_label_lens(contents, label, lens)
                  )
        } else { # not in the box
          sethash(hsh_tb,
                  key,
                  paste0(contents,
                         " ",
                         new_box(label, lens)
                         )
                  )
        }
      }
    } else { # remove (if exists) and shift (remaining) lenses
      label <- x[i][[1]][1:2]
      key <- get_mapping(label)
      contents <- gethash(hsh_tb, key, nomatch = NULL)
      if (!is.null(contents)) { # box has contents
        sethash(hsh_tb,
                key,
                remove_lense(contents, label)
        )
      }
    }
  }
  return(hsh_tb)
}

new_box <- function(label, lens) {
  sprintf("[%s %s]", paste0(label, collapse = ""), lens)
}

find_label_lens <- function(contents, label) {
  grepl(sprintf("\\[%s", paste0(label, collapse = "")), contents)
}

find_lens <- function(contents, lens) {
  grepl(sprintf("%s\\]", lens), contents)
}

remove_lense <- function(contents, label, lens) {
  gsub(sprintf("\\[%s\\s[0-9]{1,1}\\]", paste0(label, collapse = "")),
       "",
       contents)
}

replace_label_lens <- function(contents, label, lens) {
  gsub(sprintf("\\[%s\\s[0-9]{1,1}\\]", paste0(label, collapse = "")),
       new_box(label, lens),
       contents)
}

replace_lens <- function(contents, label, lens) {
  gsub(sprintf("\\[[a-z]{2,2}\\s%s\\]", lens),
       new_box(label, lens),
       contents)
}

focus_power <- function(x) {
  keys <- sort(hashkeys(x))
  # get contents of hash table
  contents <- list()
  for (i in seq_along(keys)) {
    contents <- c(contents,
                  gethash(x, keys[i], list()))
  }
  # parse contents
  ## create new hashmap
  contents_hm <- hashtab()
  lapply(seq_along(contents), function(i) {
    ht <- contents[i][[1]]
    box_id <- as.numeric(keys[i])
    aux <- strsplit(gsub("^\\[|\\]$", "",trimws(ht)),
                    "\\] \\[")[[1]]
    sapply(seq_along(aux), function(j) {
      value <- gethash(contents_hm, aux[j], 0) + ((box_id + 1) * j *
        as.numeric(gsub("\\D", "", aux[j])))
      sethash(contents_hm, aux[j], value)
    })
  })

  new_keys <- sort(hashkeys(contents_hm))
  data.frame(
    keys = new_keys,
    power = sapply(new_keys, \(x) gethash(contents_hm, x, 0))
  )
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day15
#' @export
example_data_15 <- function(example = 1) {
  l <- list(
    a = c("rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")
  )
  l[[example]]
}
