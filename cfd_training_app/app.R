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

if (getwd() == "C:/Users/jwric/OneDrive/Documents/cfd_training_app/cfd_training_app") {
    googledrive::drive_auth(token = readRDS(here::here("cfd_training_app/.secrets/c13dc354db9600c8cd9b2bd868d1bf25_corinne.fire.data@gmail.com")))
} else {
    googledrive::drive_auth(token = readRDS(here::here(".secrets/c13dc354db9600c8cd9b2bd868d1bf25_corinne.fire.data@gmail.com")))
}

board <- board_gdrive("CFD Training App")

FixColNames <- function(Data) {
    colnames(Data) <- gsub("_", " ", colnames(Data))
    colnames(Data) <- stringr::str_to_title(colnames(Data))
    
    return(Data)
}



ui <- page_navbar(
    title = "Corinne Fire Department",
    theme = bs_theme(version = 5,
                     success = "#375A7F",
                     bootswatch = "darkly"),
    nav_panel(title = "Add Training", 
        layout_sidebar(
            sidebar = sidebar(
                actionButton('add_training', "Add Training"),
                dateRangeInput('training_filter_range',
                               "Show trainings between:",
                               start = Sys.Date() - 365,
                               end = Sys.Date() + 30)
            ),
            card(dataTableOutput('view_trainings'))
        )
    ),
    
    nav_panel(title = "Manage Training", 
        layout_sidebar()
    ),
    
    nav_panel(title = "Manage Roster", 
        layout_sidebar(
            sidebar = sidebar(actionButton('add_firefighter', 'Add Firefighter'),
                              actionButton('remove_firefighter', 'Remove Firefighter')
                              ),
            card(dataTableOutput('roster')
            )
        ),
            
    ),
    nav_panel(title = "Training Summary", 
              layout_sidebar()),
    nav_spacer(),
    nav_menu(
        title = "Settings",
        align = "right",
        nav_item(actionButton("sign_out", "Sign Out"), align = "center"),
        "Version 0.0.1"
    )
)


server <- function(input, output, session) {
    
    # Use reactiveValues to maintain a local copy of the roster that is available at all times.
    # Update the local copy whenever the stored copy is updated.
    MyReactives <- reactiveValues()
    MyReactives$roster <- board %>% pin_read("roster") %>% 
        dplyr::mutate(start_date = as.Date(start_date))
    MyReactives$trainings <- board %>% pin_read("trainings") %>% 
        dplyr::mutate(date = as.Date(date))
    
    
    # Sing in/out capabilites
    observeEvent(input$sign_out, {
        showModal(modalDialog(
            textInput("username", "Username"),
            passwordInput("password", "Password"),
            title = "Sign in",
            footer = tagList(
                actionButton("sign_in", "Sign In")
            )
        ))
    }, ignoreNULL = FALSE)
    
    # Check password and username
    observeEvent(input$sign_in, {
        if(input$username == "Test" && input$password == "1234") {
            removeModal()
        } else {
            stopApp()
        }
    })
    
    ###### Add Trainings #####
    # Display current trainings
    output$view_trainings <- renderDataTable({
        # browser()
        Table_Data <- MyReactives$trainings |>
            filter(date > input$training_filter_range[1] &
                   date < input$training_filter_range[2]) |>
            select(-training_id)
        
        Table_Data <- FixColNames(Table_Data)
        
        data.table(Table_Data)
        
    })
    
    # Update the training topic based on the training type.
    observe({
        # browser()
        x <- input$add_training_type
        
        if(is.null(x)) {
            # Do nothing
        }
        
        # Can also set the label and select items
        else if(x == "EMS") {
        updateSelectInput(session,
                          "add_training_topic",
                          choices = c(
                              "Airway/Respiraroty/Ventilation",
                              "Cardiovascular",
                              "Trauma",
                              "Medical",
                              "Operations"
                          ))
        }

        else if(x == "Fire") {
            updateSelectInput(session,
                              "add_training_topic",
                              choices = c(
                                  "Fire 1",
                                  "Fire 2",
                                  "Hazmat",
                                  "Ops"
                              ))
        }
    })
    
    # Enter information to create the training.
    observeEvent(input$add_training, {
        showModal(modalDialog(
            selectInput('add_training_type', 'Training Type', choices = c("EMS", "Fire", "Wildland"), selected = "EMS"),
            selectInput('add_training_topic', 'Training Topic', choices = c()), # Update with above observe statement
            numericInput('add_training_length', "Training Length (Hours)", value = 2),
            textInput('add_description', 'Training Description'),
            dateInput('add_training_date', 'Training Date', value = Sys.Date()),
            title = "Add Training",
            footer = tagList(
                actionButton("action_add_training", "Add Training")
            ),
            easyClose = TRUE
        ))
    })
    
    # Create the training
    observeEvent(input$action_add_training, {
        # browser()
        removeModal()
        
        trainings <- MyReactives$trainings
        showModal(modalDialog("Please wait...", title = "Processing Changes"))
        new_index <- nrow(MyReactives$trainings) + 1
        MyReactives$trainings <- dplyr::bind_rows(MyReactives$trainings, 
                                               data.frame(
                                                   training_id = new_index,
                                                   training_type = input$add_training_type,
                                                   topic = input$add_training_topic,
                                                   training_length = input$add_training_length,
                                                   description = input$add_description,
                                                   date = input$add_training_date
                                                   )
        )
        board %>% pin_write(MyReactives$trainings, "trainings", "rds")
        removeModal()
        showModal(modalDialog("Your training has been successfully added.",
                              title = "Success!",
                              easyClose = TRUE))
        
        
    })
    
    ###### Manage Roster ######
    output$roster <- renderDataTable({
        # browser()
        Table_Data <- MyReactives$roster |>
            filter(active_status == TRUE) |>
            select(first_name, last_name, start_date)
        
        Table_Data <- FixColNames(Table_Data)
        
        data.table(Table_Data)
    })
    
    observeEvent(input$add_firefighter, {
        showModal(modalDialog(
            textInput('add_first_name', 'First Name'),
            textInput('add_last_name', 'Last Name'),
            dateInput('ff_start_date', 'Start Date', value = Sys.Date()),
            title = "Add Firefighter",
            footer = tagList(
                actionButton("action_add_firefigher", "Add Firefighter")
            )
        ))
    })

    
    # Test if duplicate names can be added
    # Test if white space if successuflly removed
    observeEvent(input$action_add_firefigher, {
        # browser()
        removeModal()
        proposed_full_name <- paste(trimws(input$add_first_name), trimws(input$add_last_name))
        
        roster <- MyReactives$roster
        if (proposed_full_name %in% paste(roster$first_name, roster$last_name)) {
            showModal(modalDialog("The name you tried to add already exists. Please add a unique name.",
                                  title = "Add Firefighter Failed"))
        } else {
            showModal(modalDialog("Please wait...", title = "Processing Changes"))
            new_index <- nrow(MyReactives$roster) + 1
            MyReactives$roster <- dplyr::bind_rows(MyReactives$roster, 
                data.frame(
                  firefighter_id = new_index,
                  first_name = trimws(input$add_first_name), 
                  last_name = trimws(input$add_last_name),
                  start_date = input$ff_start_date,
                  active_status = TRUE)
                )
            board %>% pin_write(MyReactives$roster, "roster")
            removeModal()
            showModal(modalDialog(paste(proposed_full_name, "has been successfully added."),
                                        title = "Success!",
                                  easyClose = TRUE))
        }
        
    })
    
    observeEvent(input$remove_firefighter, {
        roster <- MyReactives$roster
        active_roster <- roster |>
            filter(active_status == TRUE)
        full_names <- paste(active_roster$first_name, active_roster$last_name)
        showModal(modalDialog(selectInput('remove_full_name', 'Please select firefighter to remove.', full_names),
                              title = "Remove Firefighter",
                              footer = tagList(
                                  actionButton("action_remove_firefigher", "Remove Firefighter")
                              )
                              ))
    })
    
    observeEvent(input$action_remove_firefigher, {
        removeModal()
        
        roster <- MyReactives$roster
        
        if (input$remove_full_name %in% paste(roster$first_name, roster$last_name)) {
            showModal(modalDialog("Please wait...", title = "Processing Changes"))
            local_first_name <- strsplit(input$remove_full_name, " ")[[1]][1]
            local_last_name <- strsplit(input$remove_full_name, " ")[[1]][2]
            # roster <- roster[!(roster$first_name == local_first_name & roster$last_name == local_last_name),]
            roster[roster$first_name == local_first_name & roster$last_name == local_last_name,]$active_status <- FALSE
            board %>% pin_write(roster, "roster", 'rds')
            MyReactives$roster <- roster
            removeModal()
            showModal(modalDialog(paste(input$remove_full_name, "has been successfully removed."),
                                  title = "Success!",
                                  easyClose = TRUE))
            
        } else {
            showModal(modalDialog("Please contact Joseph Richey.",
                                  title = "Error Code 1",
                                  easyClose = TRUE))
        }
        
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
