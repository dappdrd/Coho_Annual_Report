####################################################################################################
# CoTC Annual Report code.  Server.R must be paired with Ui.R
# Code developed by Derek Dapp (WDFW), Angelika Hagen-breaux (WDFW), and the CoTC
# App hosted by Shiny Servers
####################################################################################################

#used for sending emails
library(mailR)
#used for shiny
library(shiny)
library(shinyFiles)
library(rdrop2)
#for data frame manipulation
library(plyr)
#For data frame manipulation
library(reshape2)
library(readxl)


token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)

shinyServer(
  #Sets up our page, this one has a header
  #Side bar (inputs)
  #Main panel (graphics)
  pageWithSidebar(
    #Adds a header at the top of the page
    headerPanel("Coho PSC Annual Report Tool"),
    
    #This adds a side bar that we'll use to put inputs in
    sidebarPanel(
      #Sets up data process button
      #Changes background color to blue
      tags$head(
        tags$style(HTML('#DataProcessButton{background-color:rgba(153, 204, 255, 0.8);"}'))
      ),
      #Changes border color to black
      tags$head(
        tags$style(HTML('#DataProcessButton{border-color: rgba(0,0,0,1);"}'))
      ),
      #bolds text
      tags$head(
        tags$style(HTML('#DataProcessButton{font-weight: bold;"}'))
      ),
      
      
      
      
      #Sets up the email data button
      #Changes background color to blue
      tags$head(
        tags$style(HTML('#EmailButton{background-color:rgba(153, 204, 255, 0.8);}'))
      ),
      #Changes border color to black
      tags$head(
        tags$style(HTML('#EmailButton{border-color: rgba(0,0,0,1);"}'))
      ),
      #bolds text
      tags$head(
        tags$style(HTML('#EmailButton{font-weight: bold;"}'))
      ),

      
      #This changes the color of the side panel
      tags$style(".well {background-color:rgba(217,217,217, 0.8);}"),
      
      
      
      textInput("PasswordAdd", "Please enter password to access program functions", ""),
      textInput("YearAdd", "Please choose a year to create an annual report for", ""),
      selectInput("TAMMAdd", "Use TAMM to update coastal stocks/catch?", choice = c("Yes", "No")),
      #Adds a dropdown box
      actionButton("DataProcessButton",label = "Process data/create output"),

      selectInput("table", "Please choose a table to display",
                  choices = c("None","Table 1", "Table 2", "Table 3", "Post-Season Summary", "Pre-Season Summary")),
      
      textInput("EmailAdd", "Email to send to:", ""),
      actionButton("EmailButton",label = "Send data to my email")
    ),
    
    #Main panel is for graphics
    mainPanel(

      #outputs a plot
      plotOutput("Plot1"),
      
      dataTableOutput("Table")
    )
  )
  
)
