library(shiny)
library(sortable)

reactlog::reactlog_enable()

ui <- fluidPage(
  tags$head(
    tags$style(HTML(".bucket-list-container {min-height: 350px;}"))
  ),
  fluidRow(
    column(
      width = 12,
      selectInput(
        inputId = "apparatus",
        label = "Select Apparatus:",
        choices = c("truck 1", "truck 2", "truck 3", 
                    "truck 4", "truck 5", "truck 6",
                    "truck 7", "truck 8", "truck 9",
                    "truck 10", "truck 11", "truck 12",
                    "truck 13", "truck 14", "truck 15"),
        selected = c("truck 1"),
        multiple = TRUE
      ),
      selectInput(
        inputId = "names",
        label = "Select Names:",
        choices = c("Alice", "Bob", "Charlie", "David", "Eve", "Frank",
                    "Zelda", "Yvonne", "Xander", "Wendy", "Victor", "Ursula",
                    "Thomas", "Samantha", "Robert", "Quincy", "Patricia", "Oscar",
                    "Nancy", "Molly", "Lenny", "Kevin", "Jenny", "Isaac", "Hannah",
                    "George", "Fred", "Edgar", "Doris", "Cathy"),
        selected = c("Alice", "Bob"),
        multiple = TRUE
      ),
      actionButton("show_modal", "Open Bucket List")
    )
  ),
  fluidRow(
    column(
      width = 12,
      tags$b("Result"),
      column(
        width = 12,
        tags$p("input$rank_list_1"),
        verbatimTextOutput("results_1"),
        tags$p("input$rank_list_2"),
        verbatimTextOutput("results_2"),
        tags$p("input$bucket_list_group"),
        verbatimTextOutput("results_3")
      )
    )
  )
)

server <- function(input, output, session) {
  
  cache <- reactiveVal(NULL) 
  
  # Function to generate dynamic bucket list UI
  generate_bucket_list <- reactive({
    req(input$apparatus, input$names)
    
    # if(is.null(cache())) {
      print('No cached value. Initializing.')
      do.call(
        bucket_list,
        c(
          list(
            header = "Drag the items in any desired bucket",
            group_name = "bucket_list_group",
            orientation = "horizontal",
            add_rank_list(
              text = "Drag from here",
              labels = input$names,
              input_id = "rank_list_1"
            )
          ),
          lapply(input$apparatus, function(apparatus_name) {
            add_rank_list(
              text = apparatus_name,
              labels = NULL,
              input_id = paste0("rank_list_", apparatus_name)
            )
          })
        )
      )
    # } else {
    #   print('Cached value found. Using cached value.')
    #   a <- cache()
    #   # browser()
    #   do.call(
    #     bucket_list,
    #     c(
    #       list(
    #         header = "Drag the items in any desired bucket",
    #         group_name = "bucket_list_group",
    #         orientation = "horizontal",
    #         add_rank_list(
    #           text = "Drag from here",
    #           labels = a$rank_list_1,
    #           input_id = "rank_list_1"
    #         )
    #       ),
    #       lapply(input$apparatus, function(apparatus_name) {
    #         add_rank_list(
    #           text = apparatus_name,
    #           labels = NULL,
    #           input_id = paste0("rank_list_", apparatus_name)
    #         )
    #       })
    #     )
    #   )
    # }
    
  }) 
  
  # observe({
   # cache(input$bucket_list_group)
  #  # browser()
  # }
  # ) |> 
  #   bindEvent(input$bucket_list_group)
  
  # Show modal when button is clicked
  observeEvent(input$show_modal, {
    showModal(modalDialog(
      title = "Bucket List",
      uiOutput("dynamic_bucket_list"),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close"),
        actionButton("save_bucket_list", "Save")
      ), size = 'l'
    ))
  })
  
  observeEvent(input$save_bucket_list, {
    removeModal()
  })
  
  # Render dynamic bucket list inside modal
  output$dynamic_bucket_list <- renderUI({
    generate_bucket_list()
  })
  
  # Render results
  output$results_1 <- renderPrint(input$rank_list_1)
  
  output$results_2 <- renderPrint({
    sapply(input$apparatus, function(apparatus_name) {
      input[[paste0("rank_list_", apparatus_name)]]
    }, simplify = FALSE)
  })
  
  output$results_3 <- renderPrint(input$bucket_list_group)
}

shinyApp(ui, server)

# shiny::reactlogShow()
