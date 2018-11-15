library(leaflet)
library(sp)
library(magrittr)
library(maps)
library(htmltools)
library(rgdal)
library(data.table)
library(tidyverse)
library(shiny)
library(ggplot2)
library(plotly)
library(shinydashboard)
# Define UI for application that draws a histogram

dono <- read_csv("11_5_MASSCONTRIBUTIONS_csv.csv")
# total donators info group by contrib, city, state
donotor_total <- dono %>% group_by(contrib, city, state) %>%
   summarize(total = sum(amount), num = n()) %>% arrange(desc(total))
#  donators info of republican group by contrib, city, state
donotor_R<-dono %>% filter(party=="R")%>%group_by(contrib, city, state) %>%
  summarize(total = sum(amount), num = n()) %>% arrange(desc(total))
#  donators info of Independent group by contrib, city, state
donotor_I<-dono %>% filter(party=="I")%>%group_by(contrib, city, state) %>%
  summarize(total = sum(amount), num = n()) %>% arrange(desc(total))
#  donators info of democratic group by contrib, city, state
donotor_D<-dono %>% filter(party=="D")%>%group_by(contrib, city, state) %>%
  summarize(total = sum(amount), num = n()) %>% arrange(desc(total))
# total amount of donate by state
dono_state_total<-dono%>%group_by(state) %>%
  summarize(total = sum(amount), numOfDonator = n()) %>% arrange(desc(total))
# total amount of donate of republican by state
dono_state_R<-dono%>%filter(party=="R")%>% group_by(state) %>%
  summarize(total = sum(amount), numOfDonator = n()) %>% arrange(desc(total))
# total amount of donate of Independent by state
dono_state_R<-dono%>%filter(party=="I")%>% group_by(state) %>%
  summarize(total = sum(amount), numOfDonator = n()) %>% arrange(desc(total))
# total amount of donate of democratic by state
dono_state_D<-dono%>%filter(party=="D")%>% group_by(state) %>%
  summarize(total = sum(amount), numOfDonator = n()) %>% arrange(desc(total))

donor_Q08aa <- dono %>% group_by(city, state, party) %>% 
  summarize(total = sum(amount), num = n()) %>% arrange(desc(total))

sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="maps",
              menuItem("Donation Mapping Plot", tabName="plot", icon=icon("line-chart"), selected=TRUE),
              menuSubItem("Republican Donation", tabName = "R", icon = icon("angle-right")),
              menuSubItem("Democratic Donation", tabName = "D", icon = icon("angle-right")),
              menuSubItem("Total Donation", tabName = "T", icon = icon("angle-right"))
  )
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "R",
            fluidRow(width = 20,
                     
                     box(width = NULL, status = "primary", solidHeader = TRUE,
                         sliderInput("Amount","Amount of Donation in thousand",
                                     min=1, max=20000,value=100),
                     sliderInput("Num","Number of Donators",
                                 min=1, max=100000,value=1000)
                     ),
                     box(width = NULL, 
                         plotOutput("",height="500px"), 
                         side = "right", collapsible = TRUE,
                         title = "Plot", status = "primary", solidHeader = TRUE)
            )),
    
    
    tabItem(tabName = "D",
            fluidRow(width = 20, 
                     box(width = NULL,status = "primary", solidHeader = TRUE,
                         tabPanel(h5("New Patient"),
                                  sliderInput("Amount","Amount of Donation in thousand",
                                              min=1, max=20000,value=100),
                                  sliderInput("Num","Number of Donators",
                                              min=1, max=100000,value=1000)
                         ),
                         box(width = NULL, plotlyOutput("geo",height="500px"), collapsible = TRUE,
                             title = "Plot", status = "primary", solidHeader = TRUE))
            )),
    tabItem(tabName = "T",
            fluidRow(width = 20, 
                     box(width = NULL,status = "primary", solidHeader = TRUE,
                         tabPanel(h5("New Patient"),
                                  sliderInput("Amount","Amount of Donation in thousand",
                                              min=1, max=20000,value=100),
                                  sliderInput("Num","Number of Donators",
                                              min=1, max=100000,value=1000)
                         ),
                         box(width = NULL, plotlyOutput("geo",height="500px"), collapsible = TRUE,
                             title = "Plot", status = "primary", solidHeader = TRUE))
            ))
  ))




dashboardPage(
  dashboardHeader(title = "Shire EDA"),
  sidebar,
  body
)
  
shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "Donation Mapping"),
    sidebar,
    body),
  server = shinyServer(function(input, output) { 
    
  }))