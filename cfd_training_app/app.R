library(shiny)
library(shinyWidgets)
library(bslib)
library(pins)
require(googledrive)

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
board %>% pin_write(1:10, "test_pin", "rds")
board %>% pin_read("test_pin")

ui <- page_navbar(
    title = "Corinne Fire Department",
    theme = bs_theme(version = 5,
                     success = "#375A7F",
                     bootswatch = "darkly"),
    nav_panel(title = "Add Training", 
              layout_sidebar()),
    nav_panel(title = "Manage Training", 
              layout_sidebar()),
    nav_panel(title = "Manage Roster", 
              layout_sidebar()),
    nav_panel(title = "Training Summary", 
              layout_sidebar()),
    nav_spacer(),
    nav_menu(
        title = "Settings",
        align = "right",
        nav_item(actionButton("sign_out", "Sign Out"), align = "center")
    )
)


server <- function(input, output) {
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
 
        
}

# Run the application 
shinyApp(ui = ui, server = server)
