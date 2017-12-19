#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("Loan Data"),
  
  #select input file 
  fileInput('datafile', 'Please choose a file',
            accept=c('text/csv', 'text/comma-separated-values,text/plain')),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(selectInput("attribute", h3("Please choose an attribute for variables"), 
                   choices = ColName <- c("Gender","Marital Status", "No of Dependent","Education Level",
                                          "Self Employed","Applicants Income","Coapplicants Income",
                                          "Loan Amount","Term of Loan","Credit History","Property Area",
                                          "Loan Status")),
                 sliderInput("aincomerange", h6("Applicant's Income Level"),min=0,max=81000,value=c(0,810000),step=1000),
                 sliderInput("cincomerange", h6("CoApplicant's Income Level"),min=0,max=42000,value=c(0,42000),step=1000),
                 sliderInput("lincomerange", h6("Approved Loan Amount ('000)"),min=0,max=700,value=c(0,1000)),step=100),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Summary of Data",htmlOutput("summary")),
                  tabPanel("Overall", plotOutput("barplot1")),
                  tabPanel("Loan Status", plotOutput("barplot2"), textOutput("rate"))
                  )
    )
  )

))
