library(shiny)
library(shiny.router)
library(bslib)
library(fontawesome)

# Define routes
menu <- tags$ul(
  class = "nav-icons",
  tags$li(a(href = route_link("/"), fa("home", title = "Main"))),
  tags$li(a(href = route_link("another"), fa("list", title = "Another page")))
)

# Add CSS for positioning and layout
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body {
      margin: 0;
    }
    .nav-icons {
      list-style: none;
      padding: 0;
      margin: 0;
      position: fixed;
      top: 100px;
      left: 30px;
      text-align: center;
      width: 60px; /* Adjust width to match your pseudo-sidebar */
    }
    .nav-icons li {
      margin: 15px 0;
    }
    .nav-icons a {
      color: inherit;
      text-decoration: none;
      font-size: 24px;
    }
    .nav-icons a:hover {
      color: #007bff;
    }
    .main-content {
      margin-left: 100px; /* Offset to the right of the pseudo-sidebar */
      padding: 20px;
    }
  "))),
  # Sidebar with icons
  div(class = "nav-icons", menu),
  # Main content
  div(class = "main-content",
      router_ui(
        route("/", 
              page_navbar(
                title = "Main Page",
                sidebar = sidebar(
                  selectInput("input1", "Input 1", choices = c(1, 2, 3))
                )
              )),
        route("another", 
              page_navbar(
                title = "Another Page",
                sidebar = sidebar(
                  numericInput("input2", "Input 2", value = 0)
                )
              ))
      )
  )
)

server <- function(input, output, session) {
  router_server()
}

shinyApp(ui, server)
