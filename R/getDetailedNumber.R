#' Getting R2019-nCoV object and checking real-time monitoring numbers of the 2019-nCoV virus
#'
#' @return a R2019-nCoV object for further use
#' @export
#' @importFrom jsonlite fromJSON
#' @examples
#'
#' x <- get_2019nCoV()
get_2019nCoV <- function() {
    x <- jsonlite::fromJSON(inside())
    message("Current confirmed cases of China:", x$chinaTotal$confirm)
    message("last update:", x$lastUpdateTime, "\n")
    class(x) <- "R2019-nCoV"
    return(x)
}

#' Getting data.frame of detailed real-time statistical numbers of every provinces of China of 2019-nCoV virus cases
#' @return a data.frame object which contains detailed real-time statistics of 2019-nCoV virus cases
#' @param x  a R2019-nCoV object, i.e. the result of function get_2019nCoV
#' @export
#' @examples
#'
#' x <- get_2019nCoV()
#' df <- getDataFrame(x)
getDataFrame <- function(x) {
    df <- x$areaTree[1, "children"][[1]]
    return(df)
}

inside <- function() {
    url <- "https://view.inews.qq.com/g2/getOnsInfo?name=disease_h5&callback=1580373566110"
    x <- suppressWarnings(readLines(url, encoding = "UTF-8"))
    x <- sub("^\\d+\\(", "", x)
    x <- sub("\\)$", "", x)
    x2 <- jsonlite::fromJSON(x)
    return(x2$data)
}
