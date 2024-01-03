library(shiny)
library(shinyWidgets)
library(bslib)
library(pins)
require(googledrive)
library(data.table)
library(dplyr)
library(odbc)

CON = dbConnect(RMySQL::MySQL(),
                dbname = "cfddb",
                host = Sys.getenv("CFDDB_HOST"),
                port = 3306,
                user = "admin",
                password = Sys.getenv("CFDDB_PASSWORD"))

Training <- dbGetQuery(CON,
                       "SELECT * 
                       FROM cfddb.training
                       WHERE training_delete IS NULL")

Roster <- dbGetQuery(CON,
                     "SELECT * FROM cfddb.firefighter")

Attendance <- dbGetQuery(CON,
                         "SELECT * FROM cfddb.attendance") |> 
  mutate(check_in = as.POSIXct(check_in),
         check_out = as.POSIXct(check_out))

FixColNames <- function(Data) {
    colnames(Data) <- gsub("_", " ", colnames(Data))
    colnames(Data) <- stringr::str_to_title(colnames(Data))
    
    return(Data)
}

ui <- page_fillable(
    title = "Corinne Fire Department",
    h1("Corinne Fire Department"),
    theme = bs_theme(version = 5,
                     success = "#375A7F",
                     bootswatch = "darkly"),
    card(
        selectInput('name', "Name", Roster$firefighter_full_name, selected = NULL),
        actionButton('check_in', "Check In")
    )
    
)


server <- function(input, output, session) {
    
    observeEvent(input$check_in, {
      
        if(!any(Training$training_date == Sys.Date(), na.rm = TRUE)) {
          showModal(modalDialog(paste0("There is currently no scheduled training for ",
                                       Sys.Date(),
                                       ". It may not have been added to the system yet. ",
                                       "For further questions please contact Joseph Richey.", 
                                       "801-644-6893"),
                                title = "No Valid Training Found"))
        } else {
          showModal(modalDialog("Please wait...", title = "Processing Changes"))
          
          target_ff_id <- Roster |> 
            filter(firefighter_full_name == input$name) |> 
            dplyr::select(firefighter_id) |> 
            unlist() |> 
            unname()
          
          target_training_id <- Training |> 
            filter(training_date == Sys.Date()) |> 
            dplyr::select(training_id) |> 
            unlist() |> 
            unname()
          
          Temp <- data.frame(attendance_id = nrow(Attendance) + 1,
                             firefighter_id = target_ff_id,
                             training_id = target_training_id,
                             check_in = Sys.time())
                      
          
          Attendance <- dplyr::bind_rows(Attendance, Temp)
          
          Attendance <- Attendance |> mutate(check_in = as.character(check_in),
                                             check_out = as.character(check_out))
          
          Attendance <- Attendance |> mutate(check_in = as.POSIXct(check_in),
                                             check_out = as.POSIXct(check_out))
          
          dbWriteTable(CON,
                       name = "cfddb.attendance",
                       value = Attendance,
                       field.type = c(attendance_id = "int",
                                      firefighter_id = "int",
                                      training_id = "int"),
                       overwrite = TRUE)
          
          dbWriteTable(CON,
                       "test_table",
                       value = cars,
                       row.names = FALSE,
                       overwrite = TRUE)
          
          removeModal()
          showModal(modalDialog("You have been successfully checked in!", title = "Success!"))
        }
        
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
