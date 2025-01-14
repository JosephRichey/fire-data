library(shiny)
library(sortable)

ui <- fluidPage(
  tags$head(
    tags$style(HTML(".bucket-list-container {min-height: 350px;}"))
  ),
  fluidRow(
    column(
      width = 12,
      tags$b("Select Apparatus and Names"),
      selectInput(
        inputId = "apparatus",
        label = "Select Apparatus:",
        choices = c("truck 1", "truck 2", "truck 3"),
        selected = c("truck 1"),
        multiple = TRUE
      ),
      selectInput(
        inputId = "names",
        label = "Select Names:",
        choices = c("Alice", "Bob", "Charlie", "David", "Eve", "Frank"),
        selected = c("Alice", "Bob"),
        multiple = TRUE
      )
    )
  ),
  fluidRow(
    column(
      tags$b("Exercise"),
      width = 12,
      uiOutput("dynamic_bucket_list")
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
  
  # Generate dynamic bucket list UI
  output$dynamic_bucket_list <- renderUI({
    req(input$apparatus, input$names)
    
    # Combine static and dynamic rank lists
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
