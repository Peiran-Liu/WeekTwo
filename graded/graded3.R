#' Read file and load it as tbl_df object
#' If the package don't exixt it will print "' does not exist"
#'
#' @param filename the character string of the file name
#'
#' @return dataframe in tbl_df class
#' if the file does not exist it will return a error
#' @import dplyr maps tidyr readr
#' @export
#'
#' @examples
#' fars_read("abc.csv")
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Make a file that having the input year
#'
#' @param year int or list, the years of the accident file you want
#'
#' @return return a list of file name that have the input years in it
#' @import tidyr
#' @export
#'
#' @examples
#' make_filename("2015")
#' make_filename(c("2015","2016","2017"))
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

fars_read_years <- function(years) {
#' summary of all fatalities by year, for the year of input
#'
#' @param year list or int, the years over the summary will be create
#'
#' @return Returns a dataframe with the summaries of each year if the year is not found will return a error
#' @import dplyr
#' @export
#'
#' @examples
#' fars_summarize_years(c("2015","2017"))
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

#' take years and group by the years and return the sum of years
#'
#' @param years int or list, the years of the accident file you want
#'
#' @return return the summerized data of the given year or list of years
#' @import tidyr dplyr
#' @export
#'
#' @examples
#' fars_summarize_years("2015")
#' fars_summarize_years(c("2015","2016","2017"))
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Take number of state and year to crate a map with point on it.
#'
#' @param state.num int, number of state
#' @param year int, year
#'
#' @return the map with the point on it
#' @import dplyr maps graphics
#' @export
#'
#' @examples
#' fars_map_state(1,2021)
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
