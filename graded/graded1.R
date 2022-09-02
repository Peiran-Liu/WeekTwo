#' function: fars_read 
#'
#' This function uses readr::read_csv to read a .csv file, and then create a
#'   dataframe from the data in the file. Function will check if the file exists. If
#'   it doesn't it will print message. 
#'
#'
#' @param filename A string that contains the path and file name
#' 
#' @return The function returns either a dataframe made from the data in the file
#'         or no dataframe with a message " 'file' does not exist" if there is no
#'         file with the name provided
#'
#' @examples
#' fars_read("c:\R\example data.csv")
# 

fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}
################################################################################
#' function: make_filename 
#'
#' The purpose of this function is to create a filename for the accidents occuring in
#' a year given by the user. #' This function will create a string with 
#' format "accident_%d.csv.bz2" , where %d is a year number provided by the user. 
#' There is no error handling.
#'     
#' @param year A string or number
#' 
#' @return The function returns a string with format: "accident_%d.csv.bz2"
#' 
#' @examples make_filename(2000)
#'

make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}
################################################################################

#' function: fars_read_years 
#'
#' Purpose of this function is to create a dataframe that shows the accidents occuring
#' in each month for each year provided by the user. This function takes creates a dataframe for each year in a list of years provided
#' by the user. It takes a list of years, then uses functions make_filename, and fars_read
#' to first generate a filename for a specific year, then create a dataframe for the file
#' with that filename. Lastly, the dataframe has a "year" column added to it, and the 
#' columns "Month" and "Year" are selected for return. 
#'     
#' 
#' @param years A vector or list of years. These could be either strings or numbers
#' 
#' @return Dataframes with columns "Month" and "Year" for each year in the list "years". The
#'         function will retrn a warning if a year in the list is invalid.
#'
#' @examples fars_read_years(c("2000","2001","2002")) 
#'           fars_read_years(seq(2000,2020, by=2))
#' 
#'

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
################################################################################

#' function: fars_summarize_years 
#'  
#' This function will summarize the accidents in year for a set of years provided 
#' by the user. It takes a list of years, makes dataframes for each year using the
#' "fars_read_years" function. Then using functions from dplyr, the dataframes are joined 
#' grouped by year, and Month. The number of each month / year is counted. Finally, the
#' dataframe is spread by year and the counts for that year. 
#'     
#' 
#' @param years A vector or list of years. These could be either strings or numbers
#' 
#' @return a dataframe that summarizes the number of accidents in a range of years.
#'         Dataframe has two columns: "year" and "n" where "n" represents the number
#'         of accidents in that year. 
#'
#' @examples fars_summarize_years(c("2000","2001","2002")) 
#'           fars_summarize_years(seq(2000,2020, by=2))
#'

fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}
################################################################################

#' function: fars_map_state 
#'  
#' The purpose of this function is to provide a state-wide map of accidents for a year
#' provided by the user. It plots the position of each accident using long and lat
#' data provided by the data set "STATE". The function handles invalid state numbers 
#' and cases where there are no accidents to report. 
#'     
#' 
#' @param state.num the number or string representing the state ID in dataset STATE
#' @param year a number or string of the year         
#'  
#' @return If there are no accidents to plot, then a message is returned. Otherwise, a map
#'         of the state with accidents is generated
#'
#' @examples fars_map_state("1","2001")
#'           fars_map_state(2, 2002)  
#'

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
################################################################################