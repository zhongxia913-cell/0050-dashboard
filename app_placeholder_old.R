library(shiny)

ui <- fluidPage(
  
  h1("0050 Investment Research Dashboard"),
  
  p("新的 Dashboard 專案正在建立中。")
  
)

server <- function(input, output, session) {
  
}

shinyApp(
  ui = ui,
  server = server
)