#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
inspections <- read_csv('https://chapple-datasets.s3.amazonaws.com/inspections2022.csv.gz')
unique(inspections$Results)

inspections <- inspections %>% 
  mutate(Results=as.factor(Results))

inspections <- inspections %>% 
  mutate(Date=mdy(`Inspection Date`)) %>% 
  select(`DBA Name`,Address,City,Date,Results) %>% 
  rename(Name= `DBA Name`)

inspections <- inspections %>% 
  filter(Results!='Business Not Located') %>% 
  filter(Results!='No Entry') %>% 
  filter(Results!='Not Ready') %>% 
  filter(Results!='Out of Business') %>% 
  mutate(Results=recode(Results, 'Pass w/ Conditions'='Pass'))

inspections <- inspections %>% 
  mutate(Results=droplevels(Results))

badchicagos <- c('312CHICAGO', 'CCHICAGO', 'CHCHICAGO', 'CHICAGO', 
                 'CHICAGOCHICAGO', 'CHICAGOHICAGO', 'CHICAGOI', 
                 'CHCICAGO', 'CHICAGOC', 'CHICAGOO', 'Chicago.')

inspections <- inspections %>% 
  mutate(City=ifelse(City %in% badchicagos, 'Chicago', City)) %>% 
  mutate(City=str_to_title(City))

keepers <- inspections %>% 
  group_by(City) %>% 
  summarize(n=n()) %>% 
  slice_max(n, n=10)

inspections <- inspections %>% 
  filter(!is.na(City)) %>% 
  filter(City %in% keepers$City)

inspections <- inspections %>% 
  mutate(City=ifelse(City=='Niles Niles', 'Niles', City))
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Chicago Restaurant Inspections"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
        checkboxGroupInput('cities', 'Choose cities to Display',
                           choices= unique(sort(inspections$City)),
                           selected= 'Chicago')
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("inspectionPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$inspectionPlot <- renderPlot({
        inspections %>%
        filter(City %in% input$cities) %>% 
        mutate(Year=as.factor(year(Date))) %>% 
        ggplot()+
        geom_bar(mapping=aes(x=Year, fill=Results))+
        theme(axis.text.x = element_text(angle=90))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
