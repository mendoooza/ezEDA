#############################################################
#                                                           #
# Author:     Alex Mendoza                                  #
# Date:       03/10/2024                                    #
# Subject:    Shiny App for simplified exploratory          #
#             Data Analysis; Insurance Claims Data          #
#############################################################
# Libraries
library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(htmltools)
library(scales)

##########################
# Data and Preprocessing #
##########################
#Load Data, prep long lists
data <- read.csv('insurance_claims.csv')
cont_vars <- c('Policy Premium' = 'policy_annual_premium',
               'Total Incurred Amount' = 'total_claim_amount',
               'Persons Injured' = 'bodily_injuries',
               'Injury Incurred' = 'injury_claim',
               'Property Incurred' = 'property_claim',
               'Vehicle Damage Incurred' = 'vehicle_claim',
               'Age' = 'age',
               'Capital Gains' = 'capital.gains',
               'Capital Loss' = 'capital.loss',
               'Incident Hour (24h)' = 'incident_hour_of_the_day',
               'Incident Witnesses' = 'witnesses',
               'Number of Vehicles Involved' = 'number_of_vehicles_involved',
               'Months as a Customer' = 'months_as_customer',
               'Vehicle Model Year' = 'auto_year')

cleaned_cont <- c('policy_annual_premium' = 'Policy Premium',
                  'total_claim_amount' = 'Total Incurred Amount',
                  'bodily_injuries' = 'Persons Injured',
                  'injury_claim' = 'Injury Incurred',
                  'property_claim' = 'Property Incurred',
                  'vehicle_claim' = 'Vehicle Damage Incurred',
                  'age' = 'Age',
                  'capital.gains' = 'Capital Gains',
                  'capital.loss' = 'Capital Loss',
                  'incident_hour_of_the_day' = 'Incident Hour (24h)',
                  'witnesses' = 'Incident Witnesses',
                  'number_of_vehicles_involved' = 'Number of Vehicles Involved',
                  'months_as_customer' = 'Months as a Customer',
                  'auto_year' = 'Vehicle Model Year')

cat_vars <- c('Policy State'='policy_state',
              'Policy Limit'='policy_csl',
              'Policy Deductible'='policy_deductable',
              'Gender'='insured_sex',
              'Insured Education Level'='insured_education_level',
              'Insured Occupation'='insured_occupation',
              'Insured Hobbies'='insured_hobbies',
              'Insured Relationship'='insured_relationship',
              'Collision Type'='collision_type',
              'Incident Severity Level'='incident_severity',
              'Incident Authorities Contacted'='authorities_contacted',
              'Incident State'='incident_state',
              'Number of Vehicles Involved'='number_of_vehicles_involved',
              'Property Damage Indicator'='property_damage',
              'Persons Injured'='bodily_injuries',
              'Witnesses'='witnesses',
              'Auto Make'='auto_make',
              'Auto Model'='auto_model',
              'Auto Year'='auto_year',
              'Fraud Reported'='fraud_reported')

cleaned_cat <-c('policy_state'='Policy State',
                'policy_csl'='Policy Limit',
                'policy_deductable'='Policy Deductible',
                'insured_sex'='Gender',
                'insured_education_level'='Insured Education Level',
                'insured_occupation'='Insured Occupation',
                'insured_hobbies'='Insured Hobbies',
                'insured_relationship'='Insured Relationship',
                'collision_type'='Collision Type',
                'incident_severity'='Incident Severity Level',
                'authorities_contacted'='Incident Authorities Contacted',
                'incident_state'='Incident State',
                'number_of_vehicles_involved'='Number of Vehicles Involved',
                'property_damage'='Property Damage Indicator',
                'bodily_injuries'='Persons Injured',
                'witnesses'='Witnesses',
                'auto_make'='Auto Make',
                'auto_model'='Auto Model',
                'auto_year'='Auto Year',
                'fraud_reported'='Fraud Reported')

# Theme for ggplot
theme_set(theme_gray())
##############
# Primary UI #
##############

ui <- navbarPage(
  title = h4(strong('ezEDA')),
  theme = shinytheme('superhero'),
###################
# Distribution UI #
###################
  tabPanel(title = h4('Distribution'),
           fluid = T,
           titlePanel('Variable Distribution'),
           sidebarLayout(
             sidebarPanel(
               selectInput('d1_input','Select an Attribute:',
                           cont_vars),
               sliderInput('d1_bins','Histogram - Number of Bins:',
                           min = 0,
                           max = 100,
                           value = 15),
               sliderInput('d1_alpha', 'Boxplot - Point Visibility:',
                           min = 0,
                           max = 1,
                           value = .5,
                           step = .1)
             ),
             mainPanel(
               div(style = "margin-bottom: 20px;",
                   plotOutput("d1_hist", height = '500px')),
               div(style = "margin-bottom: 20px;",
                   plotOutput('d1_box', height = '500px'))
             )
           )
           
  ),
###################
# Relationship UI #
###################
  tabPanel(title = h4('Relationships'),
           fluid = T,
           titlePanel('Variable Relationships'),
           sidebarLayout(
             sidebarPanel(
               selectInput('s1_input_1','Select Attribute (x-axis):',
                           cont_vars,
                           selected = 'total_claim_amount'),
               selectInput('s1_input_2','Select Attribute (y-axis):',
                           cont_vars,
                           selected = 'injury_claim'),
               selectInput('s1_factor','Add Color By:',
                           cat_vars),
               sliderInput('s1_alpha','Point Visibility:',
                           min = .1,
                           max = 1,
                           value = .5,
                           step = .1)
             ),
             mainPanel(
               div(style = "margin-bottom: 20px;",
                  plotOutput('s1_scatter', height = '400px')),
               div(style = "margin-bottom: 20px;",
                  plotOutput('s1_facet', height = '500px'))
             )
            )
  ),
###############
# Measures UI #
###############
  tabPanel(title = h4('Measures'),
           fluid = T,
           titlePanel('Category Measures'),
           sidebarLayout(
             sidebarPanel(
              selectInput('b1_input_1','Select an Attribute:',
                          cat_vars),
              selectInput('b1_input_2','Select and Measure',
                          cont_vars),
             ),
             mainPanel(
               div(style = "margin-bottom: 20px;",
                  plotOutput('b1_total', height = '500px')),
               div(style = "margin-bottom: 20px;",
               plotOutput('b1_avg', height = '500px'))
             )
           )
 )
)
##########
# Server #
##########

# Define server logic required to draw a histogram
server <- function(input, output) {
#######################
# Distribution Server #
#######################
  output$d1_hist <- renderPlot({
    d1_x <- data[[input$d1_input]]
    
    ggplot(data, aes(x = d1_x)) +
      geom_histogram(fill = '#4EC8F8', color = 'black', bins = input$d1_bins) +
      labs(title = sprintf('%s Distribution', cleaned_cont[input$d1_input]),
           x = sprintf('%s', cleaned_cont[input$d1_input]),
           y = 'Count')+
      scale_y_continuous(expand = expansion(mult = c(0,.1)), labels = label_comma())+
      scale_x_continuous(labels = label_comma())+
      theme(title = element_text(size = 15, face = 'bold'))
    })
  output$d1_box <- renderPlot({
    d1_x <- data[[input$d1_input]]
    
    ggplot(data, aes(x='',y=d1_x))+
      geom_boxplot(fill = '#4EC8F8', outlier.fill= '#9250A6',
                   outlier.size = 5, lwd = 1, outlier.shape = 23)+
      geom_jitter(width = .2, alpha = input$d1_alpha, color = 'coral1')+
      labs(title = sprintf('%s Box Plot',cleaned_cont[input$d1_input]),
           x = '',
           y = cleaned_cont[input$d1_input])+
      coord_flip()+
      scale_y_continuous(labels = label_comma())+
      theme(title = element_text(size = 15, face = 'bold'))
    })
#######################
# Relationship Server #
#######################
  output$s1_scatter <- renderPlot({
    s1_x <- data[[input$s1_input_1]]
    s1_y <- data[[input$s1_input_2]]
    s1_color <- data[[input$s1_factor]]
    
    ggplot(data, aes(x=s1_x, y=s1_y, color = factor(s1_color)))+
      geom_point(alpha = input$s1_alpha)+
      geom_smooth(method = 'lm', se = F, linewidth = 1.2)+
      labs(title = sprintf('Relationship between %s and %s',
                           cleaned_cont[input$s1_input_1],cleaned_cont[input$s1_input_2]),
           x=cleaned_cont[input$s1_input_1],y=cleaned_cont[input$s1_input_2])+
      scale_color_discrete(name = cleaned_cat[input$s1_colorby])+
      scale_x_continuous(labels = label_comma())+
      scale_y_continuous(labels = label_comma())+
      theme(title = element_text(size = 15, face = 'bold'))
  })
  output$s1_facet <- renderPlot({
    s1_x <- data[[input$s1_input_1]]
    s1_y <- data[[input$s1_input_2]]
    s1_color <- data[[input$s1_factor]]
    
    ggplot(data, aes(x=s1_x, y= s1_y, color = factor(s1_color)))+
      geom_point(alpha = input$s1_alpha ,show.legend = F)+
      scale_x_continuous(labels = label_comma())+
      scale_y_continuous(labels = label_comma())+
      facet_wrap(~factor(s1_color), ncol = 4)+
      labs(title = sprintf('Individual Relationships by %s',
                           cleaned_cat[input$s1_colorby]),
           x=cleaned_cont[input$s1_input_1],y=cleaned_cont[input$s1_input2])+
      theme(strip.text =element_text(size = 15, face = 'bold'))
  })
###################
# Measures Server #
###################
  output$b1_total <- renderPlot({
    b1_x <- data[[input$b1_input_1]]
    b1_y <- data[[input$b1_input_2]]
    
    ggplot(data, aes(x=factor(b1_x), y=b1_y))+
      stat_summary(fun = 'sum' ,geom = 'bar', position = 'identity', fill = '#4EC8F8',
                   width = .7)+
      #geom_bar(stat = 'identity', position = 'identity', show.legend = F, width = .7,
      #         fill = '#4EC8F8')+
      labs(title = sprintf('Total %s by %s', cleaned_cont[input$b1_input_2],
                           cleaned_cat[input$b1_input_1]),
           x = sprintf('%s',cleaned_cat[input$b1_input_1]),
           y = sprintf('Total %s',cleaned_cont[input$b1_input_2]))+
      scale_y_continuous(expand = expansion(mult =c(0,0.1)), labels = label_comma())+
      theme(title = element_text(size = 15, face = 'bold'))
    
  })
  output$b1_avg <- renderPlot({
    b1_x <- data[[input$b1_input_1]]
    b1_y <- data[[input$b1_input_2]]
  
    ggplot(data, aes(x=factor(b1_x), y=b1_y))+
      geom_bar(fun = "mean", stat = 'summary', show.legend = F, width = .7,
               fill = '#4EC8F8')+
      labs(title = sprintf('Average %s by %s',cleaned_cont[input$b1_input_2],
                           cleaned_cat[input$b1_input_1]),
           x = sprintf('%s',cleaned_cat[input$b1_input_1]),
           y = sprintf('Average %s',cleaned_cont[input$b1_input_2]))+
      scale_fill_hue()+
      scale_y_continuous(expand = expansion(mult = c(0,0.1)), labels = label_comma())+
      scale_fill_discrete()+
      theme(title = element_text(size = 15, face = 'bold'))
  })
}

#######
# App #
#######
# Run the application

shinyApp(ui = ui, server = server)