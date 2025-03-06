box::use(
  dplyr[...],
  tidyr[...],
  gt[...],
  bslib[...],
  shiny[...],
)

box::use(
  ../logic/app_data,
  ../logic/openai,
  ../logic/functions,
)

Ai_UI <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      min_height = '75%',
      title = 'Answer',
      card_body(
        shinycssloaders::withSpinner(gt_output(ns('table'))),
        textOutput(ns('answer'))
      )
    ),
    card(
      title = 'Question',
      card_body(
        textInput(ns('question'),
                  'Question',
                  width = '100%',
                  placeholder = 'Ask a question'),
        actionButton(ns('submit'), 'Submit')
      )
    ),
    accordion(
      open = FALSE,
      accordion_panel(
        title = 'SQL',
        textOutput(ns('sql'))
      )
    )
  )
}

Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      openai_response <- reactiveVal()

      sql <- reactiveVal()

      observeEvent(input$submit, {
        # browser()
        updateTextInput(session, 'question', value = '')

        openai_response(openai$chat$chat(
          input$question
        ))
      })

      output$table <- render_gt({
        req(input$submit)
        # browser()

        theme <- bs_get_variables(session$getCurrentTheme(),
                                         c("bg", "fg", "primary", "secondary"))



        sql(stringr::str_extract(openai_response(), "SELECT[\\s\\S]*?;"))

        if(is.na(sql())) {
          req(FALSE)
        }

        # Check if the SQL contains any unwanted keywords
        if (grepl("\\b(DELETE|DROP|UPDATE|INSERT|ALTER|TRUNCATE)\\b", sql(), ignore.case = TRUE)) {
          logger::log_error("Unsafe SQL detected!")
          stop()
        } else {

          table <- DBI::dbGetQuery(app_data$CON, sql()) |>
            functions$FixColNames()

          gt(table) |>
            tab_options(
              table.background.color = theme['bg'],
              table.font.color = theme['fg'],
              heading.background.color = theme['primary'],
              column_labels.background.color = theme['secondary'],
              column_labels.font.weight = "bold",
              table.border.top.color = theme['primary'],
              table.border.bottom.color = theme['primary']
            )


        }


      }) |>
        bindEvent(input$submit)

      output$sql <- renderText({
        req(input$submit)
        sql()
      })

      output$answer <- renderText({
        # browser()
        req(input$submit)
        stringr::str_extract(openai_response(), "Answer:.*")
      })

    }
  )
}
