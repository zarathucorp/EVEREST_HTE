# library for UI
library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(reactable)
# library for Server
library(ranger)

train_x_learner_fit <- list(
  xf0 = readRDS("xf0.rds"),
  xf1 = readRDS("xf1.rds"),
  mapping0 = readRDS("mapping0.rds"),
  mapping1 = readRDS("mapping1.rds"),
  yhat0 = readRDS("yhat0.rds"),
  yhat1 = readRDS("yhat1.rds")
)

covariates_factor <- c(
  "SEX", "DIABETES", "DIABETES.INSULIN", "DYSLIPIDEMIA", "HTN",
  "PRIOR.MI", "PRIOR.PCI", "PRIOR.STROKE", "stable.cad", "unstable.cad",
  "nstemi", "stemi", "MVD.YN", "PCI.LM.YN", "SMOKING", "MVD.PCI.YN"
)
covariates_numeric <- c(
  "AGE", "IMPLANTED.STENT.NO", "TOTAL.STENT.LENGTH", "BASELINE.HEMOGLOBIN",
  "WBC.COUNT", "BMI", "STENT.LESION.NO"
)

get_oob_predictions <- function(X, forest, mapping) {
  raw_preds <- predict(forest, X, predict.all = TRUE)$predictions
  final_preds <- vector("numeric", length = dim(X)[1])
  inbag_counts <- forest$inbag.counts

  for (i in 1:dim(X)[1]) {
    if (mapping[i] == 0 || i > length(mapping)) {
      final_preds[i] <- mean(raw_preds[i, ])
    } else {
      temp_preds <- vector("list", length = forest$num.trees)
      for (j in 1:forest$num.trees) {
        if (inbag_counts[j][[1]][mapping[i]] == 0) {
          temp_preds[[j]] <- raw_preds[i, j]
        }
      }
      final_preds[i] <- mean(unlist(Filter(is.numeric, temp_preds)))
    }
  }
  return(final_preds)
}

predict_x_learner <- function(X, W, estimate_propensities, predict_oob) {
  if (predict_oob) {
    preds_1 <- get_oob_predictions(X, train_x_learner_fit$xf1, train_x_learner_fit$mapping1)
    preds_0 <- get_oob_predictions(X, train_x_learner_fit$xf0, train_x_learner_fit$mapping0)
  } else {
    preds_1 <- predict(train_x_learner_fit$xf1, X)$predictions
    preds_0 <- predict(train_x_learner_fit$xf0, X)$predictions
  }

  if (estimate_propensities) {
    propf <- ranger(W ~ .,
      data = data.frame(X, W = W),
      min.node.size = 1
    )
    ehat <- propf$predictions
    preds <- (1 - ehat) * preds_1 + ehat * preds_0
  } else {
    preds <- 0.5 * preds_1 + 0.5 * preds_0
  }
  return(preds)
}

headerUI <- function() {
  dashboardHeader(
    title = dashboardBrand(
      title = "iDAPT tool",
      color = "olive",
      href = "https://github.com/dr-you-group/EVEREST-HTE" # ,
      # image = "https://github.com/zarathucorp/EVEREST_HTE/raw/main/img/icon.png"
    ),
    skin = "dark",
    status = "gray-dark",
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    tags$head(
      tags$link(rel = "shortcut icon", href = "https://github.com/zarathucorp/EVEREST_HTE/raw/main/img/favicon.ico", type = "image/x-icon")
    )
  )
}

myUI <- function(title, UI) {
  fluidRow(
    class = "left",
    column(
      width = 6,
      tags$b(title)
    ),
    column(
      width = 6,
      UI
    )
  )
}

inputUI <- function() {
  column(
    width = 4,
    bs4Card(
      width = 12,
      title = NULL,
      collapsible = FALSE,
      headerBorder = FALSE,
      status = "success",
      solidHeader = FALSE,
      fluidRow(
        class = "center",
        column(
          width = 6,
          actionBttn(
            inputId = "reset",
            label = "Reset",
            style = "material-flat",
            color = "default",
            block = TRUE
          )
        ),
        column(
          width = 6,
          actionBttn(
            inputId = "calculate",
            label = "Calculate",
            style = "material-flat",
            color = "success",
            block = TRUE
          )
        )
      ),
      hr(class = "gray-hr"),
      myUI(
        title = "Age",
        UI = numericInputIcon(
          inputId = "age",
          label = NULL,
          value = 60,
          min = 1,
          max = 100,
          step = 1,
          size = "sm"
        )
      ),
      myUI(
        title = "Sex",
        UI = radioGroupButtons(
          inputId = "sex",
          label = NULL,
          choices = c("Male" = 0, "Female" = 1),
          selected = 0,
          status = "success",
          justified = TRUE,
          size = "sm"
        )
      ),
      myUI(
        title = "Body mass index",
        UI = numericInputIcon(
          inputId = "bmi",
          size = "sm",
          label = NULL,
          value = 25,
          min = 0,
          max = 50,
          step = .1
        )
      ),
      myUI(
        title = "Hemoglobin",
        UI = numericInputIcon(
          inputId = "hb",
          size = "sm",
          label = NULL,
          value = 13.5,
          min = 1,
          max = 100,
          step = .1,
          icon = list(NULL, "g/dL")
        )
      ),
      myUI(
        "White Blood Cells",
        numericInputIcon(
          inputId = "wbc",
          label = NULL,
          value = 9,
          min = 1,
          max = 100,
          step = .1,
          size = "sm",
          icon = list(NULL, "10³/μL")
        )
      ),
      myUI(
        "Dyslipidemia",
        radioGroupButtons(
          inputId = "dys",
          label = NULL,
          choices = c("Y" = 1, "N" = 0),
          status = "success",
          selected = 1,
          justified = TRUE,
          size = "sm"
        )
      ),
      myUI(
        "Hypertension",
        radioGroupButtons(
          inputId = "htn",
          label = NULL,
          choices = c("Y" = 1, "N" = 0),
          status = "success",
          selected = 1,
          justified = TRUE,
          size = "sm"
        )
      ),
      myUI(
        "Current Smoker",
        radioGroupButtons(
          inputId = "smoke",
          label = NULL,
          status = "success",
          choices = c("Y" = 1, "N" = 0),
          selected = 0,
          justified = TRUE,
          size = "sm"
        )
      ),
      myUI(
        "Diabetes",
        radioGroupButtons(
          inputId = "dia",
          label = NULL,
          choices = c("Y" = 1, "N" = 0),
          selected = 0,
          status = "success",
          justified = TRUE,
          size = "sm"
        )
      ),
      myUI(
        "Insulin",
        radioGroupButtons(
          inputId = "dia.i",
          label = NULL,
          choices = c("Y" = 1, "N" = 0),
          selected = 0,
          status = "success",
          justified = TRUE,
          size = "sm"
        )
      ),
      myUI(
        "Prior myocardial infarction",
        radioGroupButtons(
          inputId = "mi",
          label = NULL,
          choices = c("O" = 1, "X" = 0),
          status = "success",
          selected = 0,
          justified = TRUE,
          size = "sm"
        )
      ),
      myUI(
        "Prior percutaneous coronary intervention",
        radioGroupButtons(
          inputId = "pci",
          label = NULL,
          choices = c("Y" = 1, "N" = 0),
          selected = 0,
          status = "success",
          justified = TRUE,
          size = "sm"
        )
      ),
      myUI(
        "Prior stroke",
        radioGroupButtons(
          inputId = "stroke",
          label = NULL,
          choices = c("Y" = 1, "N" = 0),
          status = "success",
          selected = 0,
          justified = TRUE,
          size = "sm"
        )
      ),
      hr(class = "gray-hr"),
      fluidRow(
        class = "center",
        column(
          width = 12,
          radioGroupButtons(
            inputId = "st",
            label = NULL,
            choices = c(
              "Stable angina" = 1,
              "Unstable angina" = 2,
              "Non–ST-elevation myocardial infarction" = 3,
              "ST-elevation myocardial infarction" = 4
            ),
            selected = 4,
            status = "success",
            justified = TRUE,
            size = "sm"
          )
        )
      ),
      myUI(
        "Multi-vessel disease",
        radioGroupButtons(
          inputId = "mvd",
          label = NULL,
          choices = c("Y" = 1, "N" = 0),
          selected = 0,
          status = "success",
          justified = TRUE,
          size = "sm"
        )
      ),
      myUI(
        "Multi-vessel intervention",
        radioGroupButtons(
          inputId = "mvd.pci",
          label = NULL,
          choices = c("Y" = 1, "N" = 0),
          status = "success",
          selected = 0,
          justified = TRUE,
          size = "sm"
        )
      ),
      myUI(
        "Left-main artery intervention",
        radioGroupButtons(
          inputId = "pci",
          label = NULL,
          choices = c("Y" = 1, "N" = 0),
          selected = 0,
          status = "success",
          justified = TRUE,
          size = "sm"
        )
      ),
      myUI(
        "Total No. of stents per patient",
        sliderInput(
          inputId = "stent.no",
          label = NULL,
          min = 0,
          max = 7,
          value = 1,
          step = 1,
          ticks = FALSE,
        )
      ),
      myUI(
        "Treated lesions per patient",
        sliderInput(
          inputId = "stent.lesion",
          label = NULL,
          value = 1,
          min = 0,
          max = 10,
          step = 1,
          ticks = FALSE
        )
      ),
      myUI(
        "Total stent length per patient",
        numericInputIcon(
          inputId = "stent.length",
          label = NULL,
          value = 30,
          min = 1,
          max = 100,
          step = 1,
          icon = list(NULL, "mm")
        )
      ),
      hr(class = "gray-hr"),
      fluidRow(
        class = "center",
        column(
          width = 6,
          actionBttn(
            inputId = "reset",
            label = "Reset",
            style = "material-flat",
            color = "default",
            block = TRUE
          )
        ),
        column(
          width = 6,
          actionBttn(
            inputId = "calculate",
            label = "Calculate",
            style = "material-flat",
            color = "success",
            block = TRUE
          )
        )
      )
    )
  )
}

infoUI <- function() {
  bs4Card(
    width = 12,
    title = "Model Score",
    status = "success",
    collapsible = FALSE,
    headerBorder = FALSE,
    reactableOutput(outputId = "box")
  )
}


figureUI <- function() {
  column(
    width = 4,
    bs4Card(
      width = 12,
      collapsible = FALSE,
      headerBorder = FALSE,
      background = "gray-dark",
      plotOutput(outputId = "plot")
    ),
    # bs4Card(
    #   width = 12,
    #   collapsible = FALSE,
    #   headerBorder = FALSE,
    #   background = "gray-dark",
    #   plotOutput(outputId = "plot2")
    # )
    infoUI()
  )
}


ui <- dashboardPage(
  header = headerUI(),
  sidebar = dashboardSidebar(disable = TRUE),
  body = dashboardBody(
    chooseSliderSkin(skin = "Round"),
    fluidRow(
      inputUI(),
      figureUI() # ,
      # infoUI()
    )
  ),
  controlbar = NULL,
  footer = bs4DashFooter(
    left = HTML('Shinyapp built by <a href = "https://www.zarathu.com/"> Zarathu </a>')
  ),
  title = "iDAPT",
  dark = TRUE,
  fullscreen = FALSE,
  help = FALSE,
  scrollToTop = TRUE
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    plot(1:3, 2:4)
  })

  output$plot2 <- renderPlot({
    plot(iris$Sepal.Length, iris$Sepal.Width)
  })
  # make Data frame
  observeEvent(input$calculate, {
    userInfo <- data.frame(
      AGE = input$age,
      SEX = input$sex,
      BMI = input$bmi,
      BASELINE.HEMOGLOBIN = input$hb,
      WBC.COUNT = input$wbc,
      DYSLIPIDEMIA = input$dys,
      HTN = input$htn,
      SMOKING = input$smoke,
      DIABETES = input$dia,
      DIABETES.INSULIN = input$dia.i,
      PRIOR.MI = input$mi,
      PRIOR.PCI = input$pci,
      PRIOR.STROKE = input$stroke,
      stable.cad = ifelse(input$st == 1, 1, 0),
      unstable.cad = ifelse(input$st == 2, 1, 0),
      nstemi = ifelse(input$st == 3, 1, 0),
      stemi = ifelse(input$st == 4, 1, 0),
      MVD.YN = input$mvd,
      PCI.LM.YN = input$pci,
      MVD.PCI.YN = input$mvd.pci,
      IMPLANTED.STENT.NO = input$stent.no,
      STENT.LESION.NO = input$stent.lesion,
      TOTAL.STENT.LENGTH = input$stent.length
    )

    predict_x_learner_fit <- predict_x_learner(
      userInfo[, c(covariates_factor, covariates_numeric)],
      W = 1,
      estimate_propensities = FALSE,
      predict_oob = TRUE
    ) * -1 # Inverse

    output$box <- renderReactable({
      v <- data.frame(
        x = NA,
        y = predict_x_learner_fit
      )
      colnames(v) <- c("Cluster of benefit", "Predicted Risk Reductio")

      reactable(
        data = v,
        defaultColDef = colDef(
          align = "right",
          headerStyle = list(
            background = "#1e7e34",
            color = "#fff"
          )
        ),
        bordered = TRUE,
        theme = reactableTheme(
          borderColor = "#222222",
        ),
        columns = list(
          `Predicted Risk Reductio` = colDef(
            cell = function(value) {
              if (value < 0) paste0("\u274c ", value) else paste0("\u2714\ufe0f ", value)
            },
            style = function(value) {
              if (value > 0) {
                color <- "#badc58"
              } else if (value < 0) {
                color <- "#eb4d4b"
              } else {
                color <- "#777"
              }
              list(color = color, fontWeight = "bold")
            }
          ) # colDef
        ) # columns
      ) # reactable
    })
  })
}

shinyApp(ui = ui, server = server)
