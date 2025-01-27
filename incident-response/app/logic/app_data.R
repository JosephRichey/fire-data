box::use(
  dplyr[filter, ...],
  odbc[...],
  shiny[...],
)

#' @export
CON <- dbConnect(RMySQL::MySQL(),
                 dbname = "test",
                 host = Sys.getenv("DB_HOST"),
                 port = 3306,
                 user = "admin",
                 password = Sys.getenv("DB_PASSWORD"))

#' @export
Firefighter <- dbGetQuery(CON,"SELECT * FROM firefighter")

#' @export
Apparatus <- dbGetQuery(CON, "SELECT * FROM apparatus")

#' @export
Incident <- dbGetQuery(CON, "SELECT * FROM incident")

#' @export
Firefighter_Incident <- dbGetQuery(CON, "SELECT * FROM firefighter_incident")

#' @export
Setting <- dbGetQuery(CON, "SELECT * FROM setting")

#' @export
Dispatch_Codes <- lapply(
  split(Setting$setting_value, Setting$minor_setting_key),
  function(values) {
    as.list(values)
  }
)
  


# Creating mapping vectors to get Ids
#' @export
apparatus_mapping <- stats::setNames(Apparatus$apparatus_id, Apparatus$apparatus_name)

#' @export
firefighter_mapping <- stats::setNames(Firefighter$firefighter_id, Firefighter$firefighter_full_name)