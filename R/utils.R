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
