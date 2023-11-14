library(shiny)
library(shinyWidgets)
library(bslib)
library(pins)
require(googledrive)
library(data.table)
library(dplyr)

# Set options to auth google drive
options(
    # whenever there is one account token found, use the cached token
    gargle_oauth_email = "corinne.fire.data@gmail.com",
    # specify auth tokens should be stored in a hidden directory ".secrets"
    gargle_oauth_cache = "cfd_training_app/.secrets"
)

if (getwd() == "C:/Users/jwric/OneDrive/Documents/cfd-training-attendance") {
    googledrive::drive_auth(token = readRDS(here::here(".secrets/c13dc354db9600c8cd9b2bd868d1bf25_corinne.fire.data@gmail.com")))
} else {
    googledrive::drive_auth(token = readRDS(here::here(".secrets/c13dc354db9600c8cd9b2bd868d1bf25_corinne.fire.data@gmail.com")))
}

board <- board_gdrive("CFD Training App")

Trainings <- board |> pin_read("trainings")  |>  
    dplyr::mutate(date = as.Date(date)) |> 
    dplyr::filter(date <= Sys.Date()) |> 
    arrange(desc(date)) |> 
    dplyr::mutate(full_desc = paste0(date, ": ", description))

Roster <- board |> pin_read("roster") |> 
    dplyr::mutate(start_date = as.Date(start_date)) |> 
    dplyr::filter(active_status == TRUE) |> 
    dplyr::mutate(full_name = paste(first_name, last_name))

Attendance <- board |> 
  pin_read("attendance")

FixColNames <- function(Data) {
    colnames(Data) <- gsub("_", " ", colnames(Data))
    colnames(Data) <- stringr::str_to_title(colnames(Data))
    
    return(Data)
}

ui <- page_fillable(
    title = "Corinne Fire Department",
    theme = bs_theme(version = 5,
                     success = "#375A7F",
                     bootswatch = "darkly"),
    card(
        selectInput('name', "Name", Roster$full_name, selected = NULL),
        selectInput('training', "Training", Trainings$full_desc, selected = NULL),
        actionButton('submit', "Submit")
    )
    
)


server <- function(input, output, session) {
    
    observeEvent(input$submit, {
        showModal(modalDialog("Please wait...", title = "Processing Changes"))
        
        target_ff_id <- Roster |> 
            filter(full_name == input$name) |> 
            dplyr::select(firefighter_id) |> 
            unlist() |> 
            unname()
        
        target_training_id <- Trainings |> 
            filter(full_desc == input$training) |> 
            dplyr::select(training_id) |> 
            unlist() |> 
            unname()
        
        Temp <- data.frame(firefighter_id = target_ff_id,
          training_id = target_training_id)
        
        Attendance <- dplyr::bind_rows(Attendance, Temp)
        
        board |> pin_write(Attendance, "attendance", "rds")
        
        removeModal()
        showModal(modalDialog("Your attendace has been successfully added.",
                              title = "Success!",
                              easyClose = TRUE))
        
        
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
