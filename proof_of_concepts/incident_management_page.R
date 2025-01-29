library(shiny)
library(tidyverse)
library(shinyjs)
library(DT)




# Load the data
incident_data <- read.csv(here::here("../proof_of_concepts/data/incident.csv")) |> 
  select(-c(incident_ems_units, incident_ems_units, incident_wildland_units))
firefighter_data <- read.csv(here::here("../proof_of_concepts/data/firefighter.csv"))


jsCode <- "shinyjs.editRow = function(params) {
  Shiny.setInputValue('edit_row', params, {priority: 'event'});
}"

ui <- fluidPage(
  useShinyjs(),
  extendShinyjs(text = jsCode, functions = c("editRow")),
  titlePanel("Incident Management"),
  mainPanel(
    DT::dataTableOutput("table")
  )
)


server <- function(input, output, session) {
  current_state <- incident_data
  
  app_data <- reactive({
    table <- current_state
  
  table$edit <- sprintf('<button id="edit" 
                        onclick = "shinyjs.editRow(%s)" 
                        class="btn btn-primary" 
                        data-toggle="modal" 
                        data-target="#editModal">Edit</button>',
                        rownames(table))
  
  table$delete <- sprintf('<button id="delete"
                          onclick = "shinyjs.deleteRow(%s)"
                          class="btn btn-danger">Delete</button>',
                          rownames(table))
  table

  })

  
  output$table <- DT::renderDataTable({
    DT::datatable(app_data(), escape = FALSE, selection = 'none')
  })
  
  observeEvent(input$edit_row, {
    showModal(
      modalDialog(
        title = "Edit Incident",
        DTOutput("edit_table"),
        
        actionButton("save_to_db", "Save")
      )
    )
  })
  
  observeEvent(input$delete_row, {
    
    # browser()
    # Capture the row to delete
    row_id <- as.numeric(input$delete_row)
    
    # Update the reactive current_state object
    current_state <- current_state[-row_id, ]
    
    # Save to a file or database here (see step 3)
    write.csv(current_state, here::here("../proof_of_concepts/data/incident_new.csv"), row.names = FALSE)
    
    # Update the DataTable
    replaceData(proxy, current_state, resetPaging = FALSE)
  })
  
  row_id <- reactive({
    as.numeric(input$edit_row)
  })
  
  proxy <- dataTableProxy("edit_table")
  
  output$edit_table <- renderDT({
    
    edit_data <- app_data()[row_id(),, drop = FALSE]
    
    DT::datatable(
      edit_data,
      escape = FALSE,
      selection = 'none',
      editable = TRUE
    )
  })
  
  observeEvent(input$save_to_db, {
    # Capture the edited data from the DataTable
    # browser()
    
    edited_data <- input$edit_table_cell_edit
    if (!is.null(edited_data)) {
      # Update the reactive current_state object with the new data
      current_state[row_id(), input$edit_table_cell_edit$col] <- input$edit_table_cell_edit$value
      
      # Save to a file or database here (see step 3)
      write.csv(current_state, here::here("../proof_of_concepts/data/incident_new.csv"), row.names = FALSE)
      
      # Update the DataTable
      replaceData(proxy, current_state, resetPaging = FALSE)
    }
    
    # Close the modal
    removeModal()
  })
  
  
  
  
}

shinyApp(ui = ui, server = server)




