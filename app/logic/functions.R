box::use(
  stringr[...],
)

#' @export
FixColNames <- function(Data) {
  colnames(Data) <- gsub("_", " ", colnames(Data))
  colnames(Data) <- stringr::str_to_title(colnames(Data))

  return(Data)
}

#' @export
ParseUserInput <- function(string) {
  string <- stringr::str_to_title(string)
  string <- trimws(string)

  return(string)
}

#' @export
as.MT.Date <- function(date_time) {
  as.Date(date_time, tz = Sys.getenv("TZ"))
}
