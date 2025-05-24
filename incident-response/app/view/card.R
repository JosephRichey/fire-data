box::use(
  shiny[...],
  bslib[...],
  dplyr[...],
  logger[...],
)

log_trace("Loading card.R", namespace = "card")

box::use(
  ../logic/global_functions[GetSetting,
                            IdToString,
                            HipaaLog],
  ../logic/app_data,
)

Output <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('incident_cards'))
  )
}

Server <- function(id, rdfs) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$incident_cards <- renderUI({
        Incidents <- rdfs$incident |> 
          filter(is_locked == 0) |> 
          arrange(desc(incident_start))
        
        HipaaLog(
          glue::glue(
            "Incidents viewed: {paste(
              Incidents$id,
              collapse = ', '
            )}"
          ), session
        )
        
        Firefighter_Response <- rdfs$firefighter_response
        
        lapply(seq_len(nrow(Incidents)), function(i) {
          incident <- Incidents[i, ]
          
          Responses <- rdfs$response[rdfs$response$incident_id == incident$id, ] |>
            arrange(desc(response_start))
          card(
            class = 'incident-card',
            card_header(
              div(
                class = "incident-header",
                div(
                  span(class = "incident-id", incident$cad_identifier),
                  span(class = "incident-meta", paste(
                    IdToString(app_data$Dispatch_Code, dispatch_code, incident$dispatch_id),
                    sep = " | "
                  ))
                ),
                div(
                  class = 'button-group',
                  tags$button(
                    HTML('Add Response'),
                    onclick = sprintf("add_response('%s', '%s')", ns(""), incident$id),
                    class = "edit-btn"
                  ),
                  tags$button(
                    HTML('Edit'),
                    onclick = sprintf("edit_incident('%s', '%s')", ns(""), incident$id),
                    class = "edit-btn"
                  )
                )
              )
            ),
          
            lapply(seq_len(nrow(Responses)), function(j) {
              response <- Responses[j, ]
              firefighters <- rdfs$firefighter_response |> 
                filter(response_id == response$id) |>
                pull(firefighter_id)
              
              responded <- sapply(firefighters, function(x) {
                IdToString(
                  df = app_data$Firefighter,
                  column = full_name,
                  id = x
                )
              })
              card(
                class = 'response-card',
                card_header(
                  div(class = "incident-header",
                      div(
                        span(class = "incident-meta",
                             paste(
                               format(response$response_start, "%m-%d-%Y", usetz = F), " | ",
                               format(response$response_start, "%H:%M"), " - ",
                               format(response$response_end, "%H:%M")
                            ))
                      ),
                      tags$button(
                        HTML('Edit'),
                        onclick = sprintf("edit_response('%s', '%s')", ns(""), response$id),
                        class = "edit-btn"
                      )
                  )
                )
                ,
                card_body(
                  div(class = "firefighter-list",
                      lapply(responded, function(name) {
                        tags$span(name)
                      })
                  )
                  
                )
              )
              
              
            })
          )
        })  
      }) |> 
        bindEvent(rdfs$incident, rdfs$response, rdfs$firefighter_response, 
                  ignoreNULL = F, ignoreInit = F)
    }
  )
}

log_trace("Loading card.R complete", namespace = "card")