library(shiny)
library(bs4Dash)

shinyApp(
  ui = dashboardPage(
    header = dashboardHeader(
      title = dashboardBrand(
        title = "EVEREST-HTE",
        color = "olive",
        href = "https://dr-you-group.github.io/",
        image = "img/icon.png"
      )
    ),
    sidebar = dashboardSidebar(),
    body = dashboardBody(
      lapply(getAdminLTEColors(), function(color) {
        box(status = color)
      })
    ),
    controlbar = dashboardControlbar(),
    title = "DashboardPage"
  ),
  server = function(input, output) { }
)
