library(shiny)
library(bs4Dash)
library(shinyWidgets)

ui <- dashboardPage(
  header = dashboardHeader(
    title = dashboardBrand(
      title = "EVEREST-HTE",
      color = "olive",
      href = "https://github.com/dr-you-group/EVEREST-HTE",
      image = "https://github.com/zarathucorp/EVEREST_HTE/raw/main/img/icon.png"
    ),
    skin = "dark",
    status = "gray-dark",
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css"))
  ),
  sidebar = dashboardSidebar(disable = TRUE),
  body = dashboardBody(
    fluidRow(
      column(
        width = 4,
        bs4Card(
          width = 12,
          title = NULL,
          collapsible = FALSE,
          headerBorder = FALSE,
          status = "info",
          solidHeader = FALSE,
          fluidRow(
            class = "center",
            # Factor
            column(
              width = 6,
              awesomeRadio(
                inputId = "sex",
                label = "Sex",
                choices = c("M", "F"),
                selected = NULL,
                inline = TRUE,
                status = "success"
              )
            ),
            column(
              width = 6,
              materialSwitch(
                inputId = "Id055",
                label = "Smoking", value = TRUE,
                status = "primary"
              )
            )
          ),
          hr(class = "gray-hr"),
          fluidRow(
            class = "center",
            column(
              width = 6,
              radioGroupButtons(
                inputId = "dia",
                label = "Diabetes",
                choices = c("A", "B"),
                status = "warning",
                justified = TRUE
              )
            ),
            column(
              width = 6,
              awesomeRadio(
                inputId = "dia.i",
                label = "Diabetes Insulin",
                choices = c("A", "B", "C"),
                selected = "A",
                inline = TRUE,
                status = "success"
              )
            )
          ),
          hr(class = "gray-hr"),
          fluidRow(
            class = "center",
            column(
              width = 6,
              awesomeRadio(
                inputId = "dys",
                label = "Dyslipidemia",
                choices = c("A", "B"),
                selected = "A",
                inline = TRUE,
                status = "success"
              )
            ),
            column(
              width = 6,
              awesomeRadio(
                inputId = "htn",
                label = "HTN",
                choices = c("A", "B"),
                selected = "A",
                inline = TRUE,
                status = "success"
              )
            )
          ),
          hr(class = "gray-hr"),
          fluidRow(
            class = "center",
            column(
              width = 4,
              awesomeRadio(
                inputId = "mi",
                label = "Prior.MI",
                choices = c("A", "B"),
                selected = "A",
                inline = TRUE,
                status = "success"
              )
            ),
            column(
              width = 4,
              awesomeRadio(
                inputId = "pci",
                label = "Prior.PCI",
                choices = c("A", "B"),
                selected = "A",
                inline = TRUE,
                status = "success"
              )
            ),
            column(
              width = 4,
              awesomeRadio(
                inputId = "stroke",
                label = "Prior.Stroke",
                choices = c("A", "B"),
                selected = "A",
                inline = TRUE,
                status = "success"
              )
            )
          ),
          hr(class = "gray-hr"),
          fluidRow(
            class = "center",
            column(
              width = 6,
              awesomeRadio(
                inputId = "stable",
                label = "Stable.cad",
                choices = c("A", "B"),
                selected = "A",
                inline = TRUE,
                status = "success"
              )
            ),
            column(
              width = 6,
              awesomeRadio(
                inputId = "unstable",
                label = "Unstable.cad",
                choices = c("A", "B"),
                selected = "A",
                inline = TRUE,
                status = "success"
              )
            )
          ),
          hr(class = "gray-hr"),
          fluidRow(
            class = "center",
            column(
              width = 6,
              awesomeRadio(
                inputId = "nstemi",
                label = "nstemi",
                choices = c("A", "B"),
                selected = "A",
                inline = TRUE,
                status = "success"
              )
            ),
            column(
              width = 6,
              awesomeRadio(
                inputId = "stemi",
                label = "stemi",
                choices = c("A", "B"),
                selected = "A",
                inline = TRUE,
                status = "success"
              )
            )
          ),
          fluidRow(
            class = "center",
            column(
              width = 4,
              awesomeRadio(
                inputId = "mvd",
                label = "MVD.YN",
                choices = c("A", "B"),
                selected = "A",
                inline = TRUE,
                status = "success"
              )
            ),
            column(
              width = 4,
              awesomeRadio(
                inputId = "pci",
                label = "PCI.LM.YN",
                choices = c("A", "B"),
                selected = "A",
                inline = TRUE,
                status = "success"
              )
            ),
            column(
              width = 4,
              awesomeRadio(
                inputId = "mvd.pci",
                label = "MVD.PCI.YN",
                choices = c("A", "B"),
                selected = "A",
                inline = TRUE,
                status = "success"
              )
            )
          ),
          hr(),
          # Numeric
          numericInputIcon(
            inputId = "age",
            label = NULL,
            value = NULL,
            min = 1,
            max = 100,
            step = 1,
            icon = list("Age")
          ),
          numericInputIcon(
            inputId = "stent.no",
            label = NULL,
            value = NULL,
            min = 1,
            max = 100,
            step = 1,
            icon = list("Stent.No")
          ),
          numericInputIcon(
            inputId = "stent.length",
            label = NULL,
            value = NULL,
            min = 1,
            max = 100,
            step = 1,
            icon = list("Stent.Length")
          ),
          numericInputIcon(
            inputId = "hb",
            label = NULL,
            value = NULL,
            min = 1,
            max = 100,
            step = 1,
            icon = list("Hemoglobin")
          ),
          numericInputIcon(
            inputId = "wbc",
            label = NULL,
            value = NULL,
            min = 1,
            max = 100,
            step = 1,
            icon = list("Wbc.Count")
          ),
          numericInputIcon(
            inputId = "bmi",
            label = NULL,
            value = NULL,
            min = 1,
            max = 100,
            step = 1,
            icon = list("BMI")
          ),
          numericInputIcon(
            inputId = "stent.lesion",
            label = NULL,
            value = NULL,
            min = 1,
            max = 100,
            step = 1,
            icon = list("Stent.Lesion.No")
          ),
          fluidRow(
            class = "center",
            column(
              width = 6,
              actionBttn(
                inputId = "reset",
                label = "Reset",
                style = "material-flat",
                color = "danger"
              )
            ),
            column(
              width = 6,
              actionBttn(
                inputId = "calculate",
                label = "Calculate",
                style = "material-flat",
                color = "warning"
              )
            )
          )
        )
      ),
      column(
        width = 8,
        bs4Card(
          width = 12,
          collapsible = FALSE,
          headerBorder = FALSE,
          background = "gray-dark",
          plotOutput(outputId = "plot")
        ),
        bs4Callout(
          title = 'Model Score',
          status = 'success',
          width = 12,
          descriptionBlock(
            number = 1234,
            numberColor = 'white',
            rightBorder = FALSE,
            marginBottom = FALSE
          )
        )
      )
    )
  ),
  controlbar = NULL,
  footer = bs4DashFooter(
    left = HTML('Shinyapp built by <a href = "https://www.zarathu.com/"> Zarathu </a>')
  ),
  title = "EVEREST-HTE",
  dark = TRUE,
  fullscreen = FALSE,
  help = FALSE,
  scrollToTop = TRUE
)

server <- function(input, output, session) {
  output$plot <- renderPlot(plot(1:3, 2:4))
}
shinyApp(ui = ui, server = server)
