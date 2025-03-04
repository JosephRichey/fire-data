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
    actionButton(
      ns('add_certification'),
      'Add Certification',
      class = 'btn-primary'
    ),
    actionButton(
      ns('edit_certification'),
      'Edit Certification',
      class = 'btn-secondary'
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
        DTOutput(ns('certification_table'))
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
        'Download Org Chart'
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
        graph <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

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




