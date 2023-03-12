# library for UI
library(shiny)
library(bs4Dash)
library(shinyWidgets)

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
      title = "EVEREST-HTE",
      color = "olive",
      href = "https://github.com/dr-you-group/EVEREST-HTE",
      image = "https://github.com/zarathucorp/EVEREST_HTE/raw/main/img/icon.png"
    ),
    skin = "dark",
    status = "gray-dark",
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css"))
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
      status = "info",
      solidHeader = FALSE,
      fluidRow(
        class = "center",
        # Factor
        column(
          width = 6,
          numericInputIcon(
            inputId = "age",
            label = "Age",
            value = NULL,
            min = 1,
            max = 100,
            step = 1,
            size = "sm"
          )
        ),
        column(
          width = 6,
          radioGroupButtons(
            inputId = "sex",
            label = "Sex",
            choices = c("Male" = 0, "Female" = 1),
            status = "info",
            justified = TRUE,
            size = "sm"
          )
        )
      ),
      fluidRow(
        class = "center",
        column(
          width = 4,
          numericInputIcon(
            inputId = "bmi",
            size = "sm",
            label = "BMI",
            value = NULL,
            min = 0,
            max = 50,
            step = .1
          )
        ),
        column(
          width = 4,
          numericInputIcon(
            inputId = "hb",
            size = "sm",
            label = "Hemoglobin",
            value = NULL,
            min = 1,
            max = 100,
            step = .01,
            icon = list(NULL, "g/dL")
          )
        ),
        column(
          width = 4,
          numericInputIcon(
            inputId = "wbc",
            label = "WBC.Count",
            value = NULL,
            min = 1,
            max = 100,
            step = .01,
            size = "sm"
          ),
        )
      ),
      fluidRow(
        class = "center",
        column(
          width = 4,
          radioGroupButtons(
            inputId = "dys",
            label = "Dyslipidemia",
            choices = c("Y" = 1, "N" = 0),
            status = "success",
            justified = TRUE,
            size = "sm"
          )
        ),
        column(
          width = 4,
          radioGroupButtons(
            inputId = "htn",
            label = "HTN",
            choices = c("Y" = 1, "N" = 0),
            status = "danger",
            justified = TRUE,
            size = "sm"
          )
        ),
        column(
          width = 4,
          radioGroupButtons(
            inputId = "smoke",
            label = "Smoker",
            status = "success",
            choices = c("Y" = 1, "N" = 0),
            justified = TRUE,
            size = "sm"
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
            choices = c("Y" = 1, "N" = 0),
            status = "warning",
            justified = TRUE
          )
        ),
        column(
          width = 6,
          radioGroupButtons(
            inputId = "dia.i",
            label = "Diabetes (insulin)",
            choices = c("Y" = 1, "N" = 0),
            status = "primary",
            justified = TRUE
          )
        )
      ),
      hr(class = "gray-hr"),
      fluidRow(
        class = "center",
        column(
          width = 4,
          radioGroupButtons(
            inputId = "mi",
            label = "Prior.MI",
            choices = c("O" = 1, "X" = 0),
            status = "success",
            justified = TRUE
          )
        ),
        column(
          width = 4,
          radioGroupButtons(
            inputId = "pci",
            label = "Prior.PCI",
            choices = c("Y" = 1, "N" = 0),
            status = "success",
            justified = TRUE
          )
        ),
        column(
          width = 4,
          radioGroupButtons(
            inputId = "stroke",
            label = "Prior.Stroke",
            choices = c("Y" = 1, "N" = 0),
            status = "success",
            justified = TRUE
          )
        )
      ),
      hr(class = "gray-hr"),
      fluidRow(
        class = "center",
        column(
          width = 12,
          radioGroupButtons(
            inputId = "st",
            label = "LABEL",
            choices = c("stable.cad" = 1, "unstable.cad" = 2, "nstemi" = 3, "stemi" = 4),
            status = "success",
            justified = TRUE
          )
        )
      ),
      hr(class = "gray-hr"),
      fluidRow(
        class = "center",
        column(
          width = 4,
          radioGroupButtons(
            inputId = "mvd",
            label = "MVD.YN",
            choices = c("Y" = 1, "N" = 0),
            status = "success",
            justified = TRUE
          )
        ),
        column(
          width = 4,
          radioGroupButtons(
            inputId = "pci",
            label = "PCI.LM.YN",
            choices = c("Y" = 1, "N" = 0),
            status = "success",
            justified = TRUE
          )
        ),
        column(
          width = 4,
          radioGroupButtons(
            inputId = "mvd.pci",
            label = "MVD.PCI.YN",
            choices = c("Y" = 1, "N" = 0),
            status = "success",
            justified = TRUE
          )
        )
      ),
      hr(class = "gray-hr"),
      fluidRow(
        column(
          width = 4,
          sliderInput(
            inputId = "stent.no",
            label = "No. Stent",
            min = 0,
            max = 7,
            value = 0,
            step = 1,
            ticks = FALSE
          )
        ),
        column(
          width = 4,
          sliderInput(
            inputId = "stent.lesion",
            label = "No. Stent.Lesion",
            value = 0,
            min = 0,
            max = 10,
            step = 1,
            ticks = FALSE
          )
        ),
        column(
          width = 4,
          numericInputIcon(
            inputId = "stent.length",
            label = "Stent.Length",
            value = NULL,
            min = 1,
            max = 100,
            step = 1,
            icon = list("mm")
          )
        )
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
    bs4Card(
      width = 12,
      collapsible = FALSE,
      headerBorder = FALSE,
      background = "gray-dark",
      plotOutput(outputId = "plot2")
    )
  )
}

infoUI <- function() {
  column(
    width = 4,
    bs4Callout(
      title = "Model Score",
      status = "success",
      width = 12,
      uiOutput(outputId = "box")
    )
  )
}

ui <- dashboardPage(
  header = headerUI(),
  sidebar = dashboardSidebar(disable = TRUE),
  body = dashboardBody(
    chooseSliderSkin(skin = "Round"),
    fluidRow(
      inputUI(),
      figureUI(),
      infoUI()
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
    )

    output$box <- renderUI({
      descriptionBlock(
        number = predict_x_learner_fit,
        numberColor = "white",
        rightBorder = FALSE,
        marginBottom = FALSE
      )
    })
  })
}

shinyApp(ui = ui, server = server)
