library(shiny)
library(shinyWidgets)
library(bslib)
library(pins)
require(googledrive)
library(data.table)
library(DT)
library(dplyr)
library(plotly)

# Set options to auth google drive
options(
    # whenever there is one account token found, use the cached token
    gargle_oauth_email = "corinne.fire.data@gmail.com",
    # specify auth tokens should be stored in a hidden directory ".secrets"
    gargle_oauth_cache = "cfd_training_app/.secrets"
)

if (getwd() == "C:/Users/jwric/OneDrive/Documents/cfd_training_app") {
    googledrive::drive_auth(token = readRDS(here::here("cfd_training_app/.secrets/c13dc354db9600c8cd9b2bd868d1bf25_corinne.fire.data@gmail.com")))
} else {
    googledrive::drive_auth(token = readRDS(here::here(".secrets/c13dc354db9600c8cd9b2bd868d1bf25_corinne.fire.data@gmail.com")))
}

board <- board_gdrive("CFD Training App")
Trainings <- board |> pin_read("trainings")
Roster <- board |> pin_read("roster") |> 
  filter(active_status == TRUE) |> 
  mutate(full_name = paste(first_name, last_name))
Attendance <- board |> pin_read("attendance") |> 
  unique()

FixColNames <- function(Data) {
    colnames(Data) <- gsub("_", " ", colnames(Data))
    colnames(Data) <- stringr::str_to_title(colnames(Data))
    
    return(Data)
}

load_data <- function() {
  Sys.sleep(2)
  hide("loading_page")
  show("main_content")
}

ui <- page_navbar(
    title = "Corinne Fire Department",
    theme = bs_theme(version = 5,
                     success = "#375A7F",
                     bootswatch = "darkly"),
    nav_panel(title = "Training", 
        layout_sidebar(
            sidebar = sidebar(
                width = 400,
                open = "desktop",
                actionButton('add_training', "Add Training"),
                actionButton('modify_training', "Modify Training"),
                actionButton("delete_training", "Delete Training"),
                accordion(
                  open = FALSE,
                  accordion_panel(
                    title = "Filter Trainings",
                    open = FALSE,
                    card(
                      helpText("Filter by training date"),
                      dateRangeInput('training_filter_range',
                                     "Show trainings between:",
                                     start = Sys.Date() - 365,
                                     end = Sys.Date() + 30),
                      helpText("Filter by training type"),
                      pickerInput('filter_training_type',
                                  'Training Type',
                                  choices = unique(Trainings$training_type),
                                  selected = unique(Trainings$training_type),
                                  options = list(`actions-box` = TRUE),
                                  multiple = TRUE
                      )
                    )
                  )
                )
            ),
            card(DTOutput('view_trainings'))
        )
    ),
    
    nav_panel(title = "Manage Roster", 
        layout_sidebar(
            sidebar = sidebar(actionButton('add_firefighter', 'Add Firefighter'),
                              actionButton('remove_firefighter', 'Remove Firefighter')
                              ),
            card(DTOutput('roster')
            )
        )
            
    ),
    nav_panel(title = "Training Summary", 
      navset_pill(
        nav_panel(title = "Individual", 
          layout_sidebar(
            sidebar = sidebar(
              title = "Set Filters",
              selectInput("summary_firefighter", "Firefighter", Roster$full_name),
              dateRangeInput('ind_training_filter_range',
                             "Show trainings between:",
                             start = as.Date(paste0(year(Sys.Date()), "-01-01")),
                             end = as.Date(paste0(year(Sys.Date()), "-12-31"))),
              downloadButton("download_ind", "Download Firefighter Training Summary")
            ),
            layout_columns(
              value_box("EMS Hours", textOutput("ff_ems_hours")),
              value_box("Fire Hours", textOutput("ff_fire_hours")),
              value_box("Wildland Hours", textOutput("ff_wildland_hours")),
            ),
            card(
              plotlyOutput("ff_hours_plot")
            )
          )
        ),
        
        nav_panel(title = "Department", 
          layout_sidebar(
            sidebar = sidebar(
              title = "Set Filters",
              dateRangeInput('dep_training_filter_range',
                             "Show trainings between:",
                             start = as.Date(paste0(year(Sys.Date()), "-01-01")),
                             end = as.Date(paste0(year(Sys.Date()), "-12-31"))),
              downloadButton("download_dep", "Download Department Training Summary")
            ),
            value_box("Total Training Hours", textOutput("dep_total_hours")),
            layout_columns(
              value_box("EMS Hours", textOutput("dep_ems_hours")),
              value_box("Fire Hours", textOutput("dep_fire_hours")),
              value_box("Wildland Hours", textOutput("dep_wildland_hours")),
            ),
            card(
              plotlyOutput("dep_hours_plot")
            )
          )          
        )
      )
    ),
    nav_spacer(),
    nav_menu(
        title = "Settings",
        align = "right",
        nav_item(actionButton("sign_out", "Sign Out"), align = "center"),
        "Version 0.0.1"
    )
)


server <- function(input, output, session) {
    
    ##### Global Stuff #####
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
        if(input$username == "CFD" && input$password == "1975") {
            removeModal()
        } else {
            stopApp()
        }
    })
    
    ###### Trainings #####
    # Display current trainings
    output$view_trainings <- renderDT({
        # browser()
        Table_Data <- MyReactives$trainings |>
            filter(date > input$training_filter_range[1] &
                   date < input$training_filter_range[2] &
                   training_type %in% input$filter_training_type &
                   delete == FALSE) |>
            select(-training_id, -delete)
        
        Table_Data <- FixColNames(Table_Data)
        
        DT::datatable(Table_Data,
                      selection = 'single')
        
    })
    
    # Update the add training topic based on the training type.
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
    
    # Update the modify training topic based on the type input
    observe({
      # browser()
      x <- input$modify_training_type
      
      if(is.null(x)) {
        # Do nothing
      }
      
      # Can also set the label and select items
      else if(x == "EMS") {
        updateSelectInput(session,
                          "modify_training_topic",
                          choices = c(
                            "Airway/Respiraroty/Ventilation",
                            "Cardiovascular",
                            "Trauma",
                            "Medical",
                            "Operations"
                          ),
                          selected = MyReactives$trainings[input$view_trainings_cell_clicked$row,]$topic
                          )
      }
      
      else if(x == "Fire") {
        updateSelectInput(session,
                          "modify_training_topic",
                          choices = c(
                            "Fire 1",
                            "Fire 2",
                            "Hazmat",
                            "Ops"
                          ),
                          selected = MyReactives$trainings[input$view_trainings_cell_clicked$row,]$topic
                          )
      }
    })
    
    # Enter information to create the training.
    observeEvent(input$add_training, {
        showModal(modalDialog(
            selectInput('add_training_type', 'Training Type', choices = c("EMS", "Fire", "Wildland"), selected = "EMS"),
            selectInput('add_training_topic', 'Training Topic', choices = c()), # Update with above observe statement
            numericInput('add_training_length', "Training Length (Hours)", value = 2),
            textAreaInput('add_description', 'Training Description'),
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
    
    # Modify the training
    observeEvent(input$modify_training, {
      # browser()
      cell_click <- input$view_trainings_cell_clicked
      # Make sure a row was clicked
      if (length(cell_click) != 0) {
        showModal(modalDialog(
          selectInput('modify_training_type', 'Training Type', choices = c("EMS", "Fire", "Wildland"), selected = MyReactives$trainings[cell_click$row,]$training_type),
          selectInput('modify_training_topic', 'Training Topic', choices = c()),
          numericInput('modify_training_length', "Training Length (Hours)", value = MyReactives$trainings[cell_click$row,]$training_length),
          textAreaInput('modify_description', 'Training Description', value = MyReactives$trainings[cell_click$row,]$description),
          dateInput('modify_training_date', 'Training Date', value = MyReactives$trainings[cell_click$row,]$date),
          title = "Modify Training",
          footer = tagList(
            actionButton("action_modify_training", "Modify Training")
          ),
          easyClose = TRUE
        ))
      }
    })
    
    
    observeEvent(input$action_modify_training, {
      # browser()
      removeModal()
      
      Trainings <- MyReactives$trainings
      showModal(modalDialog("Please wait...", title = "Processing Changes"))
      
      cell_click <- input$view_trainings_cell_clicked
      MyReactives$trainings <- rows_update(x = Trainings, 
                                           y = data.frame(training_id = MyReactives$trainings[cell_click$row,]$training_id,
                                                          training_type = input$modify_training_type,
                                                          topic = input$modify_training_topic,
                                                          training_length = input$modify_training_length,
                                                          description = input$modify_description,
                                                          date = input$modify_training_date,
                                                          delete = FALSE
                                           ),
                                           by = 'training_id'
      )
      
      board %>% pin_write(MyReactives$trainings, "trainings", "rds")
      removeModal()
      showModal(modalDialog("Your training has been successfully modified.",
                            title = "Success!",
                            easyClose = TRUE))
    })
    
    observeEvent(input$delete_training, {
      # browser()
      cell_click <- input$view_trainings_cell_clicked
      
      if (length(cell_click) != 0) {
        showModal(modalDialog(
          title = "Confirm Deletion",
          "Are you sure you want to delete this training? If you're sure, please type \"Delete\"",
          textInput("confirm_deletion", ""),
          footer = tagList(
            actionButton("action_delete_training", "Delete Training")
          ),
          easyClose = TRUE
        ))
      }
    })
    
    observeEvent(input$action_delete_training, {
      # browser()
      removeModal()
      
      if(input$confirm_deletion == "Delete") {
        showModal(modalDialog("Please wait...", title = "Processing Changes"))
        cell_click <- input$view_trainings_cell_clicked
        
        MyReactives$trainings <- rows_update(x = MyReactives$trainings, 
                                             y = data.frame(training_id = MyReactives$trainings[cell_click$row,]$training_id,
                                                            training_type = MyReactives$trainings[cell_click$row,]$training_type,
                                                            topic = MyReactives$trainings[cell_click$row,]$topic,
                                                            training_length = MyReactives$trainings[cell_click$row,]$training_length,
                                                            description = MyReactives$trainings[cell_click$row,]$description,
                                                            date = as.Date(MyReactives$trainings[cell_click$row,]$date),
                                                            delete = TRUE
                                             ),
                                             by = 'training_id'
        )
        
        board %>% pin_write(MyReactives$trainings, "trainings", "rds")
        removeModal()
        showModal(modalDialog("Your training has been successfully deleted.",
                              title = "Success!",
                              easyClose = TRUE))
        
      } else {
        showModal(modalDialog("Records will not be deleted.", title = "Delete Failed"))
      }
      Trainings <- MyReactives$trainings
      
      
      
      
    })
    
    
    ###### Manage Roster ######
    output$roster <- renderDT({
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
    
    ##### Ind Training Summary ####
    R_Training_Data <- reactive({
      Filtered_Trainings <- MyReactives$trainings |> 
        filter(delete == FALSE &
               date > input$ind_training_filter_range[1] &
               date < input$ind_training_filter_range[2])
      
      Filtered_Roster <- MyReactives$roster |> 
        filter(active_status == TRUE) |> 
        mutate(full_name = paste(first_name, last_name))
      
      Attendance |> 
        left_join(Filtered_Roster) |> 
        left_join(Filtered_Trainings) |> 
        filter(!is.na(delete) & !is.na(active_status))
      
    })
    
    output$ff_ems_hours <- renderText({
      Data <- R_Training_Data() |> 
        filter(full_name == input$summary_firefighter) |> 
        filter(training_type == "EMS")
      
      paste(sum(Data$training_length))
    })
    
    output$ff_fire_hours <- renderText({
      Data <- R_Training_Data() |> 
        filter(full_name == input$summary_firefighter) |> 
        filter(training_type == "Fire")
      
      paste(sum(Data$training_length))
    })
    
    output$ff_wildland_hours <- renderText({
      Data <- R_Training_Data() |> 
        filter(full_name == input$summary_firefighter) |> 
        filter(training_type == "Wildland")
      
      paste(sum(Data$training_length))
    })
    
    output$ff_hours_plot <- renderPlotly({
      
      # Assuming your dataframe has a "date" column
      R_Training_Data <- R_Training_Data() %>%
        mutate(Month = format(as.Date(date), "%Y-%m")) |> 
        filter(full_name == input$summary_firefighter)
      
      # Generate a complete set of months
      all_months <- expand.grid(training_type = unique(R_Training_Data$training_type),
                                Month = unique(R_Training_Data$Month),
                                stringsAsFactors = FALSE)
      
      # Merge with the training data to fill in missing months with zeros
      plot_data <- merge(all_months, R_Training_Data, by = c("training_type", "Month"), all.x = TRUE) %>%
        mutate(training_length = ifelse(is.na(training_length), 0, training_length)) |> 
        group_by(training_type, Month) %>%
        summarise(Total_Length = sum(training_length))
      
      # Create the plot with specified colors, legend, and hover text
      plot <- plot_ly(plot_data, x = ~Month, y = ~Total_Length, color = ~training_type, 
                      type = 'scatter', mode = 'lines', colors = c("blue", "red", "green"),
                      text = ~paste("Total Hours: ", Total_Length, " hours")) %>%
        layout(title = "Training Summary",
               xaxis = list(title = "Month"),
               yaxis = list(title = "Training Length (hours)", zeroline = FALSE),
               showlegend = TRUE)
      
      plot
    })
    
    # Individual Data Download
    R_Ind_Data_Download <- reactive({
      R_Training_Data() |> 
        filter(full_name == input$summary_firefighter) |> 
        select(full_name, training_type, topic, training_length, description, date)
    })
    
    # Download Handler
    output$download_ind <- downloadHandler(
      filename = function() {
        paste0(input$summary_firefighter, "-training-data-", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(R_Ind_Data_Download(), file)
      }
    )
    
    ##### Dep Training Summary ####
    R_Dep_Training_Data <- reactive({
      Filtered_Trainings <- MyReactives$trainings |> 
        filter(delete == FALSE &
                 date > input$dep_training_filter_range[1] &
                 date < input$dep_training_filter_range[2])
      
      Filtered_Roster <- MyReactives$roster |> 
        filter(active_status == TRUE) |> 
        mutate(full_name = paste(first_name, last_name))
      
      Attendance |> 
        left_join(Filtered_Roster) |> 
        left_join(Filtered_Trainings) |> 
        filter(!is.na(delete) & !is.na(active_status))
      
    })
    
    
    output$dep_ems_hours <- renderText({
      Data <- R_Dep_Training_Data() |> 
        filter(training_type == "EMS")
      
      paste(sum(Data$training_length))
    })
    
    output$dep_fire_hours <- renderText({
      Data <- R_Dep_Training_Data() |> 
        filter(training_type == "Fire")
      
      paste(sum(Data$training_length))
    })
    
    output$dep_wildland_hours <- renderText({
      Data <- R_Dep_Training_Data() |> 
        filter(training_type == "Wildland")
      
      paste(sum(Data$training_length))
    })
    
    output$dep_total_hours <- renderText({
      Data <- R_Dep_Training_Data()
      
      paste(sum(Data$training_length))
    })
    
    output$dep_hours_plot <- renderPlotly({
      
      # Assuming your dataframe has a "date" column
      R_Training_Data <- R_Dep_Training_Data() %>%
        mutate(Month = format(as.Date(date), "%Y-%m"))
      
      # Generate a complete set of months
      all_months <- expand.grid(training_type = unique(R_Training_Data$training_type),
                                Month = unique(R_Training_Data$Month),
                                stringsAsFactors = FALSE)
      
      # Merge with the training data to fill in missing months with zeros
      plot_data <- merge(all_months, R_Training_Data, by = c("training_type", "Month"), all.x = TRUE) %>%
        mutate(training_length = ifelse(is.na(training_length), 0, training_length)) |> 
        group_by(training_type, Month) %>%
        summarise(Total_Length = sum(training_length))
      
      # Create the plot with specified colors, legend, and hover text
      plot <- plot_ly(plot_data, x = ~Month, y = ~Total_Length, color = ~training_type, 
                      type = 'scatter', mode = 'lines', colors = c("blue", "red", "green"),
                      text = ~paste("Total Hours: ", Total_Length, " hours")) %>%
        layout(title = "Training Summary",
               xaxis = list(title = "Month"),
               yaxis = list(title = "Training Length (hours)", zeroline = FALSE),
               showlegend = TRUE)
      
      plot
    })
    
    # Individual Data Download
    R_Dep_Data_Download <- reactive({
      R_Dep_Training_Data() |> 
        select(full_name, training_type, topic, training_length, description, date)
    })
    
    # Download Handler
    output$download_dep <- downloadHandler(
      filename = function() {
        paste0("cfd-training-data-", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(R_Dep_Data_Download(), file)
      }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
