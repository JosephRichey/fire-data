library(shiny)
library(shinyWidgets)
library(bslib)

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
