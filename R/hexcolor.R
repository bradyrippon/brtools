
#' Hex color picker app
#'
#' Opens a small Shiny app with a color picker.
#'
#' @param value Initial color value.
#' @param height Plot preview height.
#' @param viewer Viewer to launch the app in. Defaults to the RStudio Viewer pane.
#'
#' @return A Shiny app is launched.
#' @export
hexcolor <- function(
    value = "#FFFFFF",
    height = "200px",
    viewer = shiny::paneViewer()
) {
  rlang::check_installed(c("shiny", "colourpicker", "ggplot2"))

  ui <- shiny::fluidPage(
    shiny::titlePanel(""),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        colourpicker::colourInput(
          inputId = "color",
          label = NULL,
          value = value,
          allowTransparent = TRUE,
          showColour = "both"
        ),

        shiny::br(),

        shiny::verbatimTextOutput("hex")
      ),

      shiny::mainPanel(
        shiny::plotOutput("color_plot", height = height)
      )
    )
  )

  server <- function(input, output, session) {
    output$hex <- shiny::renderText({
      input$color
    })

    output$color_plot <- shiny::renderPlot({
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(
            fill = input$color,
            color = input$color
          ),
          panel.background = ggplot2::element_rect(
            fill = input$color,
            color = input$color
          )
        )
    })
  }

  shiny::runApp(
    shiny::shinyApp(ui, server),
    launch.browser = viewer
  )

  invisible(NULL)
}
