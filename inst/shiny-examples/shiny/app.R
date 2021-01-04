ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "{aRbs}",
                                  titleWidth = "100%"),
  shinydashboard::dashboardSidebar(
    width = "0px"
  ),
  shinydashboard::dashboardBody(
    shiny::fluidRow(
      shiny::column(4,
                    shiny::uiOutput("select_subdomain") %>%
                      shinycssloaders::withSpinner()
      ),
      shiny::column(4,
                    shiny::br(),
                    shiny::uiOutput("get_arbs_ui") %>%
                      shinycssloaders::withSpinner()
      ),
      shiny::column(4,
                    shiny::uiOutput("select_arb_ui") %>%
                      shinycssloaders::withSpinner()
      )),
    shiny::fluidRow(
      shiny::uiOutput("win_box"),
      shiny::br(),
      shiny::column(width = 12,
                    DT::DTOutput("best_choice")
      ),
      shiny::br()
    ),

    shiny::fluidRow(
      shiny::column(width = 12,
                    shiny::uiOutput("web_page") %>%
                      shinycssloaders::withSpinner()
      )
    )
  ), skin = "green"
)

server <- function(input, output, session) {

  # Initialise reactive values
  rv <- shiny::reactiveValues()
  rv$button_disabled <- FALSE
  rv$arbs_text <- " Find arbs"
  rv$arbs_icon <- shiny::icon("search-dollar")

  # Find level one subdomains for arb select input
  # will be fixed for later versions...
  #subdomains <- level_one_subdomains()$subdomains
  subdomains <- c("Football" = "football")

  # Create arb select input
  output$select_subdomain <- shiny::renderUI({
    shiny::selectInput(
      "select_subdomain", "Pick a subdomain",
      choices = subdomains, width = "100%"
    )
  })

  output$select_arb_ui <- shiny::renderUI({
    shiny::selectInput("select_arb", "Pick an arb",
                choices = NULL, width = "100%")
  })

  output$best_choice <- DT::renderDT({
    DT::datatable(rv$arbs[[input$select_arb]]$best_choice,
                  options = list(dom = "t"),
                  style = "bootstrap4",
                  fillContainer = FALSE,
                  rownames = FALSE)
  })

  output$win_box <- shiny::renderUI({
    if(is.null(rv$arbs[[input$select_arb]]$Win)) {
      win_box <- NULL
    } else {
      win_box <- shinydashboard::infoBox(
        "Returns: ",
        paste0(rv$arbs[[input$select_arb]]$Win, "%"),
        icon = shiny::icon("hand-holding-usd"),
        color = "green",
        fill = TRUE, width = 12
      )
    }
    win_box
  })


  # Create original source as first subdomain (likely quicker)
  rv$src <- "https://www.oddschecker.com/"

  # Create oddschecker embedded iframe with source as
  # whichever arb is selected.
  observeEvent(input$select_subdomain, {

    rv$button_disabled <- FALSE
    rv$arbs_text <- " Find arbs"
    rv$arbs_icon <- shiny::icon("search-dollar")

    rv$src <- paste0("https://www.oddschecker.com/",
                     input$select_subdomain)

    output$web_page <- shiny::renderUI({
      rv$website <- shiny::tags$iframe(
        seamless = "seamless",
        src = rv$src,
        height = 600, width = 1230
      )
    })
  })

  # "Find arbs" button pressed
  shiny::observeEvent(input$get_arbs_button_btn, {
    rv$button_disabled <- TRUE
    rv$arbs_icon <- shiny::icon("check-circle")
  })

  # Create find arbs action button that we can disable
  output$get_arbs_ui <- shiny::renderUI({
    shinyBS::bsButton(
      inputId = "get_arbs_button_btn",
      label = rv$arbs_text,
      icon = rv$arbs_icon,
      style = "success",
      size = "small",
      block = TRUE,
      disabled = rv$button_disabled
    )
  })

  shiny::observeEvent(input$get_arbs_button_btn, {

    rv$arbs <- aRbs::get_arbs(
      paste0("https://www.oddschecker.com/",
             input$select_subdomain)
    )

    rv$arbs_text <- paste0(
      " ", length(rv$arbs),
      ifelse(length(rv$arbs) == 1,
             " arb found", " arbs found.")
    )

    shiny::updateSelectInput(session, "select_arb",
                      choices = names(rv$arbs))
  })

  # Changed arb
  shiny::observeEvent(input$select_arb, {
    rv$src <- paste0("https://www.oddschecker.com/",
                     rv$arbs[[input$select_arb]]$event)
  })

}

shiny::shinyApp(ui, server)
