box::use(
  shiny[...],
  bslib[...],
  shinyWidgets[...],
  dplyr[filter, ...],
  logger[...],
  shinyalert[...],
  clipr[write_clip],
  blastula[...],

)


box::use(
  ../logic/app_data,
)


UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("copy_email"), "Copy Email Addresses",
                 class = 'btn-primary'),
    actionButton(ns("copy_phone"), "Copy Phone Numbers",
                 class = 'btn-primary'),
  )
}

Output <- function(id) {
  ns <- NS(id)
  tagList(
    helpText('This will be a drop down list of all firefighters'),
    textInput(
      ns("receipient"),
      "Recipient",
      value = "",
      width = "100%"
      ),
    textInput(
      ns("subject"),
      "Subject",
      value = "",
      width = "100%"
      ),
    textAreaInput(
      ns("message"),
      "Message",
      value = "",
      width = "100%",
      rows = 10
      ),
    # Add option to attach a file
    fileInput(ns("file"), "Attach File"),
    actionButton(ns("send"), "Send Message",
                 class = 'btn-primary')
  )
}

Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      observe({
        app_data$Firefighter_Contact |>
          select(email_address) |>
          pull() |>
          write_clip(breaks = ", ")

        showNotification("Email addresses copied to clipboard", duration = 5)

      }) |>
        bindEvent(input$copy_email)

      observe({
        app_data$Firefighter_Contact |>
          select(phone_number) |>
          pull() |>
          write_clip(breaks = ", ")

        showNotification("Phone numbers copied to clipboard", duration = 5)

      }) |>
        bindEvent(input$copy_phone)

      observe({
        showModal(
          modalDialog(
            title = "Password Required",
            "Currently, this uses my personal email for testing. To prevent the interent at large from using my email to send emails to whoever they want, I have password protected this feature. If you would like to test this feature, please contact me for the password.",
            passwordInput(ns("password"), "Password"),
            easyClose = TRUE,
            footer = tagList(
              actionButton(ns("cancel"), "Cancel"),
              actionButton(ns("submit"), "Submit", class = "btn-primary")
            )
          )
        )

      }) |>
        bindEvent(input$send)

      observe({
        # browser()

        req(input$password)

        if (input$password != Sys.getenv("EMAIL_PASSWORD")) {
          shinyalert("error", "Incorrect Password", "Please try again.")
          removeModal()
        } else {

          date_time <- add_readable_time()

          email <- compose_email(
            body = md(input$message),
            footer = md(
              glue::glue("Email sent on {date_time} by FirePulse test app.")
              )
            )

          email |>
            smtp_send(
              to = input$receipient,
              from = c("no-reply@FirePulse" = "jwrichey.1@gmail.com"),
              subject = input$subject,
              credentials = creds_key("gmail_creds")
            )

          removeModal()
          updateTextAreaInput(session, "message", value = "")
          updateTextInput(session, "subject", value = "")
          updateTextInput(session, "receipient", value = "")
          # updateFileInput(session, "file", value = list())
          showNotification("Email sent", duration = 5)

        }

      }) |>
        bindEvent(input$submit)

    }
  )
}






