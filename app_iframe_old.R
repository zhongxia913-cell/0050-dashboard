# 0050 dashboard: launch this file with RStudio's Run App button.
# First-time setup in the Console: install.packages("shiny")
# R serves the app; existing JavaScript performs calculations in the browser.

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop('Please install Shiny first: install.packages("shiny")', call. = FALSE)
}

ui <- shiny::tags$html(
  lang = "zh-Hant",
  shiny::tags$head(
    shiny::tags$title("0050 | Investment Research"),
    shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    shiny::tags$style(shiny::HTML(
      "html, body { margin: 0; padding: 0; width: 100%; height: 100%; overflow: hidden; }
       #dashboard-frame { display: block; width: 100%; height: 100vh; border: 0; }"
    ))
  ),
  shiny::tags$body(
    shiny::tags$iframe(
      id = "dashboard-frame",
      src = "dashboard/index.html",
      title = "0050 investment research dashboard"
    )
  )
)

server <- function(input, output, session) {
  # The dashboard is a self-contained client-side application.
  # CSV imports and calculations occur in the browser, not R's Environment.
}

shiny::shinyApp(ui = ui, server = server)
