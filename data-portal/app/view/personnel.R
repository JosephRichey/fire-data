box::use(
  shiny[...],
  bslib[...],
  shinyWidgets[...],
  DT[...],
  dplyr[filter, ...],
  DBI[...],
  tibble[...],
  ggplot2[...],
  ggraph[...],
  tidygraph[...],
  logger[...],
  shinyalert[...],
)


box::use(
  ../logic/app_data,
  ../logic/functions,
  ../modals/modals,
)


Roster_UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(
      ns('add_firefighter'),
      'Add Firefighter',
      class = 'btn-primary'
      ),
    actionButton(
      ns('edit_firefighter'),
      'Edit Firefighter',
      class = 'btn-secondary'
      ),
    checkboxInput(
      ns('active_only'),
      'Active Only',
      value = TRUE
    )
  )
}


Roster_Output <- function(id) {
  ns <- NS(id)

  tagList(
    card(
    fill = FALSE,
    card_body(
        fillable = FALSE,
        DTOutput(ns('roster_table'))
      )
    )
  )

}

Certification_UI <- function(id) {
  ns <- NS(id)
  tagList(
    pickerInput(
      ns('certification_type'),
      'Certification Type',
      choices = c(app_data$Certification_Type$certification_name |> unique()),
      selected = c(app_data$Certification_Type$certification_name |> unique()),
      options = list(
        `actions-box` = TRUE,
        #FIXME set with .csv settings
        # `live-search` = TRUE,
        size = 10
      ),
      multiple = TRUE
    ),
    pickerInput(
      ns('cert_firefighter'),
      'Firefighter',
      choices = c(app_data$Firefighter |> filter(active_status == 1) |> pull(full_name)),
      selected = c(app_data$Firefighter |> filter(active_status == 1) |> pull(full_name)),
      options = list(
        `actions-box` = TRUE,
        #FIXME set with .csv settings
        # `live-search` = TRUE,
        size = 10
      ),
      multiple = TRUE
    ),

    checkboxGroupInput(
      ns('expire_due_filter'),
      label = '',
      choices = c('Due', 'Approaching', 'Normal'),
      #FIXME set with .csv settings
      selected = c('Due', 'Approaching'),
      inline = TRUE
    ),
    hr(),
    br(),
    actionButton(
      ns('add_cert'),
      'Add Certification',
      icon = icon('plus'),
      class = 'btn-primary',
    ),
    actionButton(
      ns('renew'),
      'Renew',
      icon = icon('check'),
      class = 'btn-secondary',
      width = '100%'
    ),
    actionButton(
      ns('delete'),
      'Delete',
      icon = icon('trash'),
      class = 'btn-warning',
      width = '100%'
    ),

    actionButton(
      ns('cert_refresh'),
      'Refresh',
      icon = icon('rotate-right'),
      class = 'btn-light',
      width = '100%'
    )
  )
}

Certification_Output <- function(id) {
  ns <- NS(id)

  tagList(
    card(
      fill = FALSE,
      card_body(
        fillable = FALSE,
        DTOutput(ns('certification'))
      )
    )
  )
}

Org_Chart_Output <- function(id) {
  ns <- NS(id)

  tagList(
    card(
      downloadButton(
        ns('download_org_chart'),
        'Download Org Chart',
        class = 'btn-primary'
      ),
      fill = FALSE,
      card_body(
        fillable = FALSE,
        plotOutput(ns('org_chart'))
      )
    )
  )

}

Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      r_Firefighter <- reactiveVal(app_data$Firefighter)

      r_Chain_Of_Command <- reactiveVal(app_data$Chain_Of_Command)

      r_Roster <- reactive({
        # browser()
        roster <- r_Firefighter() |>
          # Always show active firefighters, and show deactive if active_only is unchecked
          dplyr::filter(active_status == 1 |
                          (active_status == 1) == input$active_only) |>
          mutate(tenure = sample(1:10, n(), replace = TRUE)) |> #FIXME
          left_join(r_Chain_Of_Command(),
                    by = c('firefighter_id')) |>
          left_join(r_Firefighter() |>
                      select(firefighter_id, full_name) |>
                      rename(reports_to = full_name),
                    by = c('supervisor_id' = 'firefighter_id')) |>
          left_join(app_data$Company, by = c('company_id')) |>
          select(firefighter_id,
                 full_name,
                 company_name,
                 firefighter_role,
                 reports_to,
                 tenure,
                 active_status)

      })

      output$roster_table <- renderDT({
        # browser()

        Table_Data <- r_Roster() |>
          mutate('active_status' = sprintf(
            '<a href="#" onclick="App.toggle_firefighter(\'%s\', \'%d\')" style="font-size: 25px; text-decoration: none; font-weight: bold; color: %s">%s</a>',
            ns(''),
            firefighter_id,
            ifelse(active_status == 1,
                   bs_get_variables(session$getCurrentTheme(), 'success') |> unname(),
                   bs_get_variables(session$getCurrentTheme(), 'primary') |> unname()),
            "⏻")
            ) |>
          functions$FixColNames()


        datatable(Table_Data,
                   rownames = FALSE,
                  escape = FALSE,
                  options = list(
                    columnDefs = list(
                      list(visible = FALSE, targets = c(0)),
                      list(className = 'dt-center', targets = '_all')
                    ),
                    order = list(1, 'asc')
                  )
                  )
      })

      observe({
        print('Toggling')
        New <- r_Firefighter() |>
          mutate(active_status = if_else(firefighter_id == input$toggle_firefighter,
                                         ifelse(active_status == 1, 0, 1),
                                         active_status))
        r_Firefighter(New)

        #FIXME Need to consider rebuilding the chain of command when a firefighter is removed.


      }) |>
        bindEvent(input$toggle_firefighter)

      observeEvent(input$add_firefighter, {
        # browser()
        showModal(modals$addFirefighterModal(ns))
      })

      observeEvent(input$edit_firefighter, {
        #TODO Build the modal so previous values can be passed to it and coalesced.
        # Not implementing this now- wait until after focus group.
        shinyalert::shinyalert(
          title = "Edit Firefighter",
          text = "This feature is not yet implemented.",
          type = "info"
        )
      })


      observeEvent(input$action_add_firefigher, {

        removeModal()

        roster <- r_Firefighter()

        New <- roster |>
          add_row(
            firefighter_id = max(roster$firefighter_id) + 1,
            full_name = input$add_full_name,
            start_date = input$add_start_date,
            trainer = ifelse(input$add_trainer, 1, 0),
            officer = ifelse(input$add_officer, 1, 0),
            active_status = 1,
            company_id = app_data$Company |> filter(input$add_company == company_name) |> pull(company_id),
            firefighter_role = input$add_role)

        r_Firefighter(New)

        Chain_Of_Command <- r_Chain_Of_Command()

        New <- Chain_Of_Command |>
          add_row(
            chain_of_command_id = max(Chain_Of_Command$chain_of_command_id) + 1,
            supervisor_id = r_Firefighter() |> filter(full_name == input$add_reports_to) |> pull(firefighter_id),
            firefighter_id = max(roster$firefighter_id) + 1)

        r_Chain_Of_Command(New)


      })

      ###### Certifications #####

      r_Certification <- reactiveVal({
        app_data$Certification
      })

      r_Visible_Certs <- reactive({
        # browser()
        certification <- r_Certification() |>
          left_join(app_data$Certification_Type, by = c('type_id' ='certification_type_id')) |>
          #FIXME Certs to inactive firefighters shouldn't be shown.
          left_join(r_Firefighter(), by = c('firefighter_id')) |>
          mutate(warning_threshold = functions$GenerateThreshold(expiration_date, lead_time, lead_time_unit)) |>
          mutate(
            flag_type = case_when(
              expiration_date <= app_data$Local_Date ~ 'Due',
              warning_threshold <= app_data$Local_Date ~ 'Approaching',
              TRUE ~ 'Normal'
            ),
          status = case_when(
            flag_type == 'Due' ~ bsicons::bs_icon("exclamation-triangle", fill = 'red'),
            flag_type == 'Approaching' ~ bsicons::bs_icon("exclamation-triangle", fill = 'yellow'),
            TRUE ~ bsicons::bs_icon("check-circle-fill", fill = 'green')
            )
          ) |>
          filter(full_name %in% input$cert_firefighter) |>
          filter(certification_name %in% input$certification_type) |>
          filter(flag_type %in% input$expire_due_filter) |>
          select(certification_id, type_id,
                 status, full_name, certification_name, expiration_date)
      })

      output$certification <- renderDT({
        datatable(r_Visible_Certs() |> functions$FixColNames(),
                  rownames = FALSE,
                  escape = FALSE,
                  options = list(
                    columnDefs = list(
                      list(visible = FALSE, targets = c(0, 1)),
                      list(className = 'dt-center', targets = '_all')
                    ),
                    order = list(5, 'asc')
                  )
        ) |>
          DT::formatDate(columns = 6, method = 'toLocaleDateString')
      })

      observe({
        # browser()
        log_trace('Checking to see if certs can be renewed.')

        if(is.null(input$certification_rows_selected)) {
          shinyalert(
            title = 'Error',
            type = 'error',
            text = 'Please select certifications to renew.'
          )
          return()
        }

        #TODO Does this need to be in a function? Used multiple places.

        renew_ids <- r_Visible_Certs()[input$certification_rows_selected,1]
        renew_types <- r_Visible_Certs()[input$certification_rows_selected,2]

        dates <- app_data$Certification_Type |>
          filter(certification_type_id %in% renew_types) |>
          arrange(desc(renew_time)) |>
          slice_head(n = 1)

        recommended_expiration <- functions$GenerateThreshold(app_data$Current_Local_Date, dates$renew_time, dates$renew_time_unit, TRUE) |>
          as.Date()


        showModal(
          modalDialog(
            title = 'Select New Expiration Date',

            'Please select the new expiration date for the selected certifications.',
            dateInput(
              ns('new_expiration_date'),
              'New Expiration Date',
              min = app_data$Current_Local_Date,
              value = recommended_expiration
            ),

            footer = tagList(
              modalButton('Cancel'),
              actionButton(ns('confirm_expiration_update'), 'Confirm',
                           class = 'btn-primary')
            )
          )
        )
      }) |>
        bindEvent(input$renew)

      observeEvent(input$confirm_expiration_update, {
        # browser()
        renew_ids <- r_Visible_Certs()[input$certification_rows_selected,1]

        New <- r_Certification() |>
          mutate(expiration_date = if_else(certification_id %in% renew_ids, input$new_expiration_date, expiration_date))

        r_Certification(New)

        removeModal()
      })

      observeEvent(input$cert_refresh, {
        # browser()
        r_Certification(app_data$Certification)
      })

      observe({
        # browser()
        if(is.null(input$certification_rows_selected)) {
          shinyalert(
            title = 'Error',
            type = 'error',
            text = 'Please select certifications to delete.'
          )
          return()
        }

        showModal(
          modalDialog(
            title = 'Delete Certifications',
            'Are you sure you want to delete the selected certifications?',
            footer = tagList(
              modalButton('Cancel'),
              actionButton(ns('confirm_delete'), 'Confirm',
                           class = 'btn-primary')
            )
          )
        )
      }) |>
        bindEvent(input$delete)

      observe({
        # browser()

        removeModal()
        delete_ids <- r_Visible_Certs()[input$certification_rows_selected,1]

        New <- r_Certification() |>
          filter(!certification_id %in% delete_ids)

        r_Certification(New)

        showNotification(
          'Certifications deleted.',
          duration = 5,
          type = 'message'
        )
      }) |>
        bindEvent(input$confirm_delete)

      observe({

        showModal(
          modalDialog(
            title = 'Add Certification',
            selectInput(
              ns('add_cert_type'),
              'Certification Type',
              choices = c(app_data$Certification_Type$certification_name |> unique())
            ),
            selectInput(
              ns('add_cert_firefighter'),
              'Firefighter',
              choices = c(app_data$Firefighter |> filter(active_status == 1) |> pull(full_name)),
              multiple = TRUE
            ),
            dateInput(
              #FIXME Update based on selected cert
              ns('add_cert_date'),
              'Expiration Date',
              value = app_data$Current_Local_Date + 730
            ),
            footer = tagList(
              modalButton('Cancel'),
              actionButton(ns('confirm_add_cert'), 'Confirm',
                           class = 'btn-primary')
            )
          )
        )

      }) |>
        bindEvent(input$add_cert)

      observeEvent(input$confirm_add_cert, {

        #FIXME Perform check to make sure there are't duplicate certs.

        # browser()
        New <- r_Certification() |>
          add_row(
            certification_id = max(r_Certification()$certification_id) + 1,
            firefighter_id = app_data$Firefighter |> filter(full_name %in% input$add_cert_firefighter) |> pull(firefighter_id),
            type_id = app_data$Certification_Type |> filter(certification_name == input$add_cert_type) |> pull(certification_type_id),
            expiration_date = input$add_cert_date
          )

        r_Certification(New)

        removeModal()

        showNotification(
          'Certification(s) added.',
          duration = 5,
          type = 'message'
        )
      })



      ###### Org Chart ######
      output$org_chart <- renderPlot({
        # Plot using ggraph
        # browser()

        edges <- r_Chain_Of_Command() |>
          left_join(
            r_Firefighter() |>
              select(firefighter_id, full_name) |>
              rename(from = full_name),
            by = c('supervisor_id' = 'firefighter_id')
            ) |>
          left_join(
            r_Firefighter() |>
              select(firefighter_id, full_name) |>
              rename(to = full_name),
            by = c('firefighter_id')) |>
          select(from, to) |>
          stats::na.omit()


        nodes <- r_Firefighter() |>
          dplyr::filter(active_status == 1) |>
          left_join(app_data$Company, by = c('company_id')) |>
          select(full_name, firefighter_role, company_name) |>
          rename(name = full_name, role = firefighter_role) |>
          #FIXME Add rank when creating settings for firefighter roles
          mutate(rank = case_when(
            role == 'Chief' ~ 1,
            role == 'Assistant Chief' ~ 2,
            role == 'Captain' ~ 3,
            role == 'Lieutenant' ~ 4,
            role == 'Firefighter' ~ 5,
            role == 'Probationary' ~ 6
          ))

        # Convert to a tidygraph object
        tryCatch(
          graph <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

        , error = function(e) {
          shinyalert(
            title = 'Error',
            type = 'error',
            text = 'There was an error generating the org chart. This usually happens when you remove someone from active duty without removing them from the chain of command.'
          )
          print(e)
          return(NULL)
        }

        )

        return(ggraph(graph, layout = "tree") +
                 geom_edge_link() +  # No arrows specified
                 geom_node_point(ggplot2::aes(color = company_name), size = 6) +
                 geom_node_text(ggplot2::aes(label = paste(name, "\n", role)), size = 5) +
                 theme_minimal() +
                 theme(
                   axis.line = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank(),
                   axis.title = element_blank(),
                   panel.grid = element_blank(),
                   panel.background = element_rect(fill = bs_get_variables(session$getCurrentTheme(), 'gray-500') |> unname(), color = NA),
                   plot.background = element_rect(fill = bs_get_variables(session$getCurrentTheme(), 'gray-500') |> unname(), color = NA),
                   legend.position = "left"
                 ) +
                 labs(color = "Company"))
      })

      output$download_org_chart <- downloadHandler(
        filename = function() { "org_chart.png" },
        content = function(file) {
          ggsave(file, plot = last_plot(), device = "png", width = 18, height = 12, dpi = 300)
        }
      )

    }
  )
}




