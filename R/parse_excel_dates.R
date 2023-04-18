#' Cleans excel dates
#'
#' @param dates The vector of dates (possibly in month/day/Year or excel number format)
#'
#' @return The formatted dates as Date objects
#' @export
#'
#' @examples
#' event.table <- data.frame(
#'     events = c('birthday', 'party', 'meeting'),
#'     dates = c('7/27/2022', '43528', '7/26/2022')
#' );
#'
#' event.table$dates <- clean.dates(event.table$dates);
#'
clean.dates <- function(dates) {
    new.dates <- as.Date(dates, format = '%m/%d/%Y');
    missing.format <- is.na(new.dates);
    new.dates[missing.format] <- as.Date(as.numeric(dates[missing.format]), origin = '1899-12-30');
    new.dates <- format(new.dates, '%Y-%m-%d');
    return(new.dates);
    }
