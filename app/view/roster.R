box::use(
  shiny[...],
  bslib[...],
  shinyWidgets[...],
  DT[...],
  dplyr[filter, ...],
  DBI[...],
  tibble[...],
)


box::use(
  ../logic/app_data,
  ../logic/functions,
  ../modals/modals,
)


UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns('add_firefighter'), 'Add Firefighter'),
    actionButton(ns('remove_firefighter'), 'Remove Firefighter')
  )
}


Output <- function(id) {
  ns <- NS(id)

  tagList(
    card(DTOutput(ns('roster_table')))
  )

}

Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      rv <- reactiveVal(app_data$Roster)

      ns <- session$ns

      updateReactiveValue <- function() {
        rv(DBI::dbGetQuery(app_data$CON, paste0("SELECT * FROM cfddb.firefighter", Sys.getenv("TESTING")," WHERE firefighter_deactive_date IS NULL")) |>
             remove_rownames()
           )
      }

      output$roster_table <- renderDT({

        Table_Data <- rv() |>
          select(firefighter_first_name, firefighter_last_name, firefighter_start_date)

        Table_Data <- functions$FixColNames(Table_Data)

        datatable(Table_Data,
                   rownames = FALSE)
      })

      observeEvent(input$add_firefighter, {
        showModal(modalDialog(
          textInput(ns('add_first_name'), 'First Name'),
          textInput(ns('add_last_name'), 'Last Name'),
          dateInput(ns('ff_start_date'), 'Start Date', value = Sys.Date()),
          title = "Add Firefighter",
          footer = tagList(
            actionButton(ns("action_add_firefigher"), "Add Firefighter")
          ),
          easyClose = TRUE
        ))
      })


      # Test if duplicate names can be added
      # Test if white space if successfully removed
      # Test capitalization differences
      observeEvent(input$action_add_firefigher, {
        # browser()
        removeModal()
        proposed_full_name <- paste(functions$ParseUserInput(input$add_first_name), functions$ParseUserInput(input$add_last_name))

        roster <- rv()
        if (proposed_full_name %in% roster$firefighter_full_name) {
          showModal(modals$warningModal("The name you tried to add already exists. Please add a unique name."))
        } else {
          sql_command <- paste0(
            "INSERT INTO cfddb.firefighter", Sys.getenv("TESTING")," (firefighter_first_name, firefighter_last_name, firefighter_full_name, firefighter_start_date, firefighter_trainer, firefighter_officer, firefighter_deactive_date) VALUES (",
            "'", functions$ParseUserInput(input$add_first_name), "', ",
            "'", functions$ParseUserInput(input$add_last_name), "', ",
            "'", proposed_full_name, "', ",
            "'", input$ff_start_date, "', ",
            "TRUE, FALSE, ",
            "NULL",
            ")"

          )

          write_result <- DBI::dbExecute(app_data$CON, sql_command)

          if(write_result == 1) {
            showModal(
              modals$successModal(paste0(proposed_full_name, " has been successfully added."))
            )
          } else {
            showModal(
              modals$errorModal(paste("Firefighter add failed with write result equal to ", write_result))
            )
          }
        }

        updateReactiveValue()

      })

      observeEvent(input$remove_firefighter, {
        roster <- rv()
        full_names <- rv()$firefighter_full_name
        showModal(modalDialog(selectInput(ns('remove_full_name'), 'Please select firefighter to remove.', full_names),
                              title = "Remove Firefighter",
                              footer = tagList(
                                actionButton(ns("action_remove_firefigher"), "Remove Firefighter")
                              )
        ))
      })

      observeEvent(input$action_remove_firefigher, {
        removeModal()

        sql_command <- paste0(
          'UPDATE cfddb.firefighter', Sys.getenv("TESTING"),' SET firefighter_deactive_date = NOW() WHERE firefighter_full_name = "',
          input$remove_full_name, '"', 'AND firefighter_deactive_date IS NULL'
        )

        write_result <- DBI::dbExecute(app_data$CON, sql_command)

        if(write_result == 1) {
          showModal(
            modals$successModal(paste0(input$remove_full_name, " has been successfully removed."))
          )
        } else {
          showModal(
            modals$errorModal(paste("Firefighter delete failed with write result equal to ", write_result))
          )
        }

        updateReactiveValue()
      })

    }
  )
}




