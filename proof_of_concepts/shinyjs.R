# https://deanattali.com/shinyjs/example

library(shiny)
shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(list(.big = "font-size: 2em")),
    div(id = "myapp",
        h2("shinyjs demo"),
        checkboxInput("big", "Bigger text", FALSE),
        textInput("name", "Name", ""),
        a(id = "toggleAdvanced", "Show/hide advanced info", href = "#"),
        shinyjs::hidden(
          div(id = "advanced",
              numericInput("age", "Age", 30),
              textInput("company", "Company", "")
          )
        ),
        p("Timestamp: ",
          span(id = "time", date()),
          a(id = "update", "Update", href = "#")
        ),
        actionButton("submit", "Submit"),
        actionButton("reset", "Reset form")
    )
  ),
  
  server = function(input, output) {
    observe({
      shinyjs::toggleState("submit", !is.null(input$name) && input$name != "")
    })
    
    shinyjs::onclick("toggleAdvanced",
                     shinyjs::toggle(id = "advanced", anim = TRUE))    
    
    shinyjs::onclick("update", shinyjs::html("time", date()))
    
    observe({
      shinyjs::toggleClass("myapp", "big", input$big)
    })
    
    observeEvent(input$submit, {
      shinyjs::alert("Thank you!")
    })
    
    observeEvent(input$reset, {
      shinyjs::reset("myapp")
    })    
  }
)