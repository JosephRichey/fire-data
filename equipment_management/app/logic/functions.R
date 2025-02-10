box::use(
  dplyr[...],
  lubridate[...],
)

box::use(
  
)


#' @export
GenerateThreshold <- function(date, leadTime, leadTimeUnit) {
  case_when(
    leadTimeUnit == "day" ~ date - days(leadTime),
    leadTimeUnit == 'month' ~ date %m-% months(leadTime),
    leadTimeUnit == 'year' ~ date %m-% years(leadTime)
  )
}
