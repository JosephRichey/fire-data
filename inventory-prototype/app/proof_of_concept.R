library(shiny)
library(shinyWidgets)
library(bslib)
library(odbc)
library(RMySQL)
library(dplyr)

CON <- dbConnect(RMySQL::MySQL(),
                 dbname = "cfddb",
                 host = "db1.c5umikw8m5v5.us-east-2.rds.amazonaws.com",
                 port = 3306,
                 user = "admin",
                 password = "7zKEDCKHwtfvhmvLeiPGbRN8XEHGr")

Inventory_Item <- dbGetQuery(CON, "SELECT * FROM cfddb.inventory_item")


# Define UI
ui <- page_fixed(
  title = "Corinne Fire Department",
  h1("Corinne Fire Department"),
  h3("Inventory"),
  theme = bs_theme(version = 5,
                   secondary = "#87292b",
                   success = "#87292b",
                   bootswatch = "darkly",
                   "card-title-color" = "#fff",
                   "card-cap-bg" = "#87292b"),

    card(
      card_body(
        min_height = "300px",
        textInput("barcode", 
                  "",
                  placeholder = 'Scan barcode here'),
        layout_column_wrap(
          width = 1/2,
          actionButton("less_1", "-1"),
          actionButton("less_5", "-5"),
          actionButton("plus_1", "+1"),
          actionButton("plus_5", "+5"),
        ),
        
        htmlOutput('item_details')
        
        
      )
    )
)

# Define server logic
server <- function(input, output, session) {
  
  r_barcode <- reactiveVal("")
  
  r_Item <- reactive({
    Inventory_Item <- dbGetQuery(CON, 
                                 paste0("SELECT * FROM cfddb.inventory_item
                                        WHERE item_id = ", r_barcode()))
    
    Inventory_Level <- dbGetQuery(CON,
                                  paste0("SELECT * FROM cfddb.inventory_level
                                         WHERE item_id = ", r_barcode()))
    
    Inventory <- Inventory_Item |> 
      left_join(Inventory_Level)
  })
  
  observe({
    if(nchar(input$barcode) >= 8) {
      r_barcode(input$barcode)
      updateTextInput(session = session,
                      inputId = "barcode",
                      value = "")
    }
  })
  
  output$item_details <- renderUI({
    # browser()
    
    if(!nchar(r_barcode()) >= 8) {
      req(FALSE) 
    }
    
    
    if(!(r_barcode() %in% Inventory_Item$item_id)) {
      showModal(
        modalDialog(
          title = "Invalid Barcode",
          "This barcode is not valid. Please try again and confirm this item has been added to the system."
        )
      )
    }
    
    Item <- r_Item()
    
    HTML(paste0(
      "Item ID: ", r_barcode(), br(),
      "Item Name: ", Item$item_description, br(),
      "Item Count: ", Item$item_count
      
    ))
    
    
  })
  
  observeEvent(input$less_1, {
    if(!nchar(r_barcode()) >= 8) {
      req(FALSE) 
    }
    
    Item <- r_Item()
    
    update_statement <- paste0('UPDATE cfddb.inventory_level SET item_count = ', Item$item_count - 1,
                               ' WHERE item_id = ', Item$item_id)
    dbExecute(CON, update_statement)
    
  })
  
  observeEvent(input$less_5, {
    if(!nchar(r_barcode()) >= 8) {
      req(FALSE) 
    }
    
    Item <- r_Item()
    
    update_statement <- paste0('UPDATE cfddb.inventory_level SET item_count = ', Item$item_count - 5,
                               ' WHERE item_id = ', Item$item_id)
    
    print(update_statement)
    dbExecute(CON, update_statement)
    
  })
  
  observeEvent(input$plus_1, {
    if(!nchar(r_barcode()) >= 8) {
      req(FALSE) 
    }
    
    Item <- r_Item()
    
    update_statement <- paste0('UPDATE cfddb.inventory_level SET item_count = ', Item$item_count + 1,
                               ' WHERE item_id = ', Item$item_id)
    dbExecute(CON, update_statement)
    
  })
  
  observeEvent(input$plus_5, {
    if(!nchar(r_barcode()) >= 8) {
      req(FALSE) 
    }
    
    Item <- r_Item()
    
    update_statement <- paste0('UPDATE cfddb.inventory_level SET item_count = ', Item$item_count + 5,
                               ' WHERE item_id = ', Item$item_id)
    dbExecute(CON, update_statement)
    
  })
  
  session$onSessionEnded(function() {
    dbDisconnect(CON)
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
