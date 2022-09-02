#' Read a csv file
#'
#' This is a function for reading a csv type document. External packages are loaded
#' including readr (for reading a file) and dplyr (for transforming into a tibble data
#' framework). The file name need to include a directory of the file. Otherwise, such a
#' function cannot read it successfully.
#'
#' @param filename The name of a csv file.
#' @return A tibble data framework.
#' @examples
#' fars_read("Desktop/accident_2013.csv")
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Generate a file name in a form of accident_year.csv.bz2.
#'
#' This function is simply used for generating a file name without any external packages required.
#'
#' @param year A number of a year.
#' @return a name of the file including the year.
#' @examples
#' make_filename(1980.1)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read months and years from documents.
#'
#' This function is used for read the months and years information from a document.
#' External package dplyr is loaded to extract month and year information.
#'
#' @param years a vector of years.
#' @return a list of tibbles including months, years and errors if exist.
#'
#' @examples
#' fars_read_years(c(2013,2014))
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Count the total number of observations for each month and year combination.
#'
#' This function is used to summarize documents in a count table.
#' External packages are loaded including dplyr (for counting) and tidyr (for generating a table).
#'
#' @param years a vector of years.
#' @return a table for counting based on each combination of months and years.
#'
#' @examples
#' fars_summarize_years(c(2013,2014))
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Draw a map for a given state number and a given year.
#'
#' This function is used to draw a map for a specific state number given a specific year.
#' External packages are loaded including dplyr (for filtering a subset of the data)
#' and maps, graphics (for drawing a plot).
#'
#' @param state.num a numeric value to indicate the state number.
#' @param years a numeric value of a specific year.
#' @return a map satisfied the condition.
#'
#' @examples
#' fars_map_state(1,2013)
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
