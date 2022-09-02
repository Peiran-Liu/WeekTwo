#' Read the data
#'
#' @description
#' `fars_read` reads the csv file of the function's argument.
#'
#' @details
#' This function read the csv data. If it already exist, it will stop and
#' return a message. If it does not exist it will read the data and transform
#' it into a tbl format.
#'
#' @param filename name and path of the csv file to be read.
#' @return a tibble data of the data read.
#' @export
#'
#' @importFrom readr read_csv
#'
#' @examples
#' fars_read(filename="C:/Users/accident_2013.csv.bz2")
#' fars_read(filename="C:/Users/accident_2014.csv.bz2")
#' fars_read(filename="C:/Users/accident_2015.csv.bz2")
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}





#' Create the name of the data
#'
#' @description
#' `make_filename` assigns the name of the read data.
#'
#' @details
#' This function adds the name of the data into the pre-defined data name.
#'
#' @param year year of the data to read.
#' @return a character string of the name of the read data.
#' @export
#'
#' @importFrom base sprintf
#'
#' @examples
#' make_filename(year=2013)
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}









#' Extract the month and year from a list of data files
#'
#' @description
#' `fars_read_years` extract the month and year from a list of data files.
#'
#' @details
#' This function returns a list of each year with a single row by month and year. If the year parameter
#' is not a valid value, then it gives a warning message.
#'
#' @param years a list of years to read data.
#' @return a list of the number of years. Within each list we have the Month and the year of the data.
#' @export
#' @import make_filename
#' @import fars_read
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' 
#' @examples
#' fars_read_years(years=c(2013,2014,2015))
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







#' Summarize the number of rows by month and year
#'
#' @description
#' `fars_summarize_years` summarizes into a unique data the number of rows by month and year.
#'
#' @details
#' This function summarizes the list of read data into a single data tibble. 
#' It contains 12 rows and the number of columns is the number of year of the data.
#'
#' @param years a list of years to read data.
#' @return a data tibble summarizing the number of rows by month and year.
#' @export
#' @import fars_read_years
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' 
#' @examples
#' fars_summarize_years(years=c(2013,2014,2015))
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}









#' Graph a map of the latitude and longitude of a state and year
#'
#' @description
#' `fars_map_state` graphs of the latitude and longitude of a state and year.
#'
#' @details
#' This function reads the data of the year selected and for the number of selected state
#' first it validates that the corresponding state exist into the data. Then it subsets the data
#' for the corresponding state. If is does not exist it gives an error message. Then, if there is not data of
#' latitude and longitude it assigns an specific value higher than certain thresholds.  
#'
#' @param state.num the number of the state to obtain the graph.
#' @param year the year of the data.
#' @return a graph of the latitude and longitude of a state and year.
#' @export
#' @import make_filename
#' @import fars_read
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' 
#' @examples
#' fars_map_state(state.num=1,year=2013)
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