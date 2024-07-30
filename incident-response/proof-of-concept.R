library(shiny)
library(shinyWidgets)
library(bslib)

# Define UI
ui <- page_fixed(
  title = "Corinne Fire Department",
  h1("Corinne Fire Department"),
  h3("Call Attendance"),
  theme = bs_theme(version = 5,
                   secondary = "#87292b",
                   success = "#87292b",
                   bootswatch = "darkly",
                   "card-title-color" = "#fff",
                   "card-cap-bg" = "#87292b"),
  
  layout_columns(
    card(
      card_body(
        min_height = "300px",
        actionButton("add_call_btn", "Add Call")
      )
    ),
    
    card(
      card_body(
        min_height = "800px",
        bslib::card_title("Recent Calls"),
        uiOutput("call_cards")
      )
    ),
    
    col_widths = c(12,12)
  )
)

# Define server logic
server <- function(input, output, session) {
  # Track the number of calls
  call_count <- reactiveValues(count = 0)
  
  # Store call details
  call_details <- reactiveValues()
  
  # Function to render a new call card
  observeEvent(input$add_call_btn, {
    call_count$count <- call_count$count + 1
    
    modal <- modalDialog(
      textInput("dispatch_reason", "Dispatch Reason:", ""),
      dateInput("dispatch_date", "Dispatch Date:"),
      selectInput('responders', 'Responders', c('Clint Norman', 'Von Sorenson', 'Mickey Mouse'), selected = c(), multiple = T),
      checkboxGroupInput('services', 'Services', c('EMS', 'Fire', 'Brush')),
      textInput("call_notes", "Notes:", ""),
      footer = tagList(
        actionButton("submit_call", "Next")
      )
    )
    
    # Show modal
    showModal(modal)
  })
  
  # Function to render call cards
  observe({
    call_cards <- lapply(1:call_count$count, function(i) {
      # browser()
      
      card(
        bslib::card_header(paste(
          call_details[[paste0("call_", i)]]$dispatch_date,
          call_details[[paste0("call_", i)]]$dispatch_reason,
          collapse = " "), class = "h3"),
        bslib::card_body(
          "Responders: ",
          paste(call_details[[paste0("call_", i)]]$responders, collapse = ", "),
          br(),
          "Services: ",
          paste(call_details[[paste0("call_", i)]]$services, collapse = ", "),
          br(),
          "Notes: ",
          paste(call_details[[paste0("call_", i)]]$call_notes),
          actionButton(paste0("call_", i), "Modify"),
          class = 'card-bg'
        ),
      )
    })
    
    output$call_cards <- renderUI(call_cards)
  })
  
  # Function to handle submitted call
  observeEvent(input$submit_call, {
    removeModal()
    
    # browser()
    
    dispatch_reason <- input$dispatch_reason
    dispatch_date <- input$dispatch_date
    responders <- input$responders
    services <- input$services
    call_notes <- input$call_notes
    
    modal <- modalDialog(
      responders[1],
      radioButtons('button 1', "", c('Active', 'Standby')),
      br(),
      responders[2],
      radioButtons('button 2', "", c('Active', 'Standby')),
      
      footer = tagList(
        actionButton("submit_roles", "Submit")
      )
    )
    
    # Show modal
    showModal(modal)
    
    
    # Store call details
    call_details[[paste0("call_", call_count$count)]] <- list(dispatch_reason = dispatch_reason,
                                                              dispatch_date = dispatch_date,
                                                              responders = responders,
                                                              services = services,
                                                              call_notes = call_notes)
  })
  
  observeEvent(input$submit_roles, {
    removeModal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
