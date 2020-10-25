rm(list=ls())

library(shiny)
library(ranger)

model <- readRDS("C:/Users/shrey/Downloads/covid_machine_learning/c50_model.rds")
dataset <- read.csv("C:/Users/shrey/Downloads/covid_machine_learning/covid.csv", sep=' ', header = T)
dataset$sex <- as.factor(dataset$sex)
dataset$age_group <- as.factor(dataset$age_group)
dataset$Race.and.ethnicity..combined. <- as.factor(dataset$Race.and.ethnicity..combined.)
dataset$risk_factor <- as.factor(dataset$risk_factor)


ui <- fluidPage(
  titlePanel("C50 model for COVID-19 Risk Factor Prediction"),
  sidebarPanel(
    selectInput('sex', 'Gender',
                unique(dataset$sex)),
    selectInput('age_group', 'Age Group',
                unique(dataset$age_group)),
    selectInput('Race.and.ethnicity..combined.','Race', 
                unique(dataset$Race.and.ethnicity..combined.)),
    selectInput('medcond_yn','Medical Condition in past 1 month',
                unique(dataset$medcond_yn)),
    selectInput('cdc_report_dt','Enter last visit outside house (yyyymmdd)', 
                unique(dataset$cdc_report_dt)),
    actionButton("enter", label = "Submit for Prediction"),
    width = 20
  ),
  mainPanel(
    textOutput('res'),
    textOutput('risk_factor'),
    textOutput('scales_0'),
    textOutput('scales_1'),
    textOutput('scales_2'),
    textOutput('scales_3')
    
  )
)
server <- function(input, output, session) {
  val <- eventReactive(
    input$enter, {
      data.frame(
        sex = input$sex,
        age_group = input$age_group,
        Race.and.ethnicity..combined. = input$Race.and.ethnicity..combined.,
        medcond_yn = as.integer(input$medcond_yn),
        cdc_report_dt = as.numeric(input$cdc_report_dt),
        stringsAsFactors = T
      )}
  )
  output$res <- renderText("Predicted Risk Factor")
  output$risk_factor <- renderText({
    predict(model, val())[[1]]
  })
  output$scales_0 <- renderText("0 - No risk (shouldn't visit a doctor)")
  output$scales_1 <- renderText("1 - Minimal risk (Not required to go to doctor)")
  output$scales_2 <- renderText("2 - Moderate risk (should plan on visiting doctor within a week if illness pertains)")
  output$scales_3 <- renderText("3 - High Risk (Should visit a doctor immediately)")
}

shinyApp(ui, server)

