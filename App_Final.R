packages = c('shiny', 'shinydashboard', 'dashboardthemes', 'leaflet',
             'readr', 'stringr', 'dplyr', 'tidyverse', 'lubridate',
             'matrixStats', 'ggplot2', 'ggpubr', 'ggcorrplot', 'ggmap',
             'vcd', 'pROC', 'tm', 'wordcloud', 
             'rgdal', 'DT', 'htmltools'
             )
for (p in packages){
  if(!require(p,character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}


######################
###   Import Data  ###
######################
data <- read_csv("Masterdata.csv")

mapdata <- read.csv("table_f.csv")
US_states <- readOGR("USA_States.shp")

#Call predictive model
source("predictive_model_simplified.R")

#################
### Data Prep ###
#################

### Loan Grade Analysis ===============================

data$g_purpose <- data$purpose
data$g_purpose[data$g_purpose=='car' | data$g_purpose=='major_purchase' ] <- 'major_purchase'
data$g_purpose[data$g_purpose=='educational' | data$g_purpose=='vacation' | data$g_purpose=='wedding' ] <- 'occational_spending'
data$g_purpose[data$g_purpose=='home_improvement' | data$g_purpose=='house' | data$g_purpose=='moving' ] <- 'house'
data$g_purpose[data$g_purpose=='other' | data$g_purpose=='renewable_energy' ] <- 'other'

### Loan Grade Analysis ===============================
#check for missing data
#sapply(data, function(x) sum(is.na(x)))
#convert issue_d to date, plot int rate over time for different grades.
data$issue_d2 <- mdy(data$issue_d)

#data filter to grade A 
dataA<-data%>%filter(`grade`=="A")
tempdata<-data%>%filter(`loan_status` %in% c("Fully Paid","Default","Charged Off"))
tempdata2<-tempdata%>%mutate(loan_status2= if_else(loan_status=="Fully Paid","Fully Paid","Default"))
bondyear<-tempdata2%>%group_by(`grade`,`issue_d2`)%>%
  dplyr::summarise(fully_paid=count(`loan_status2`=='Fully Paid'),
                   default=count(`loan_status2`=='Default'),
                   mInt_Rate=median(`int_rate`))
bondyear['default_ratio']<-bondyear$default/(bondyear$default +bondyear$fully_paid)*100

### Two Samples Test ===============================

data$default[data$loan_status == 'Default'] <- 'yes'
data$default[data$loan_status == 'Charged Off'] <- 'yes'
data$default[data$loan_status == 'Fully Paid'] <- 'no'

### Map ===============================

bins <- c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 50)
pal <- colorBin("YlOrRd", domain = mapdata$Pct_default, bins = bins)



######################
####   Side Bar   ####
######################
sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Descriptive Analysis" ,tabName = "Overview", icon = icon("dashboard")),
      menuItem("Inferential Anaysis", tabName = "LoanGrade", icon = icon("chart-line")),
      menuItem("Predictive Analysis",tabName = "Predict", icon = icon("braille"))
    )
)


##################
####   Body   ####
##################
body <- dashboardBody(
    shinyDashboardThemes(theme = "poor_mans_flatly"),
    tabItems(
        tabItem(
            tabName = "Overview",
            fluidPage(
                #titlePanel("Descriptive Analysis of Loan Details"),
                fluidRow(
                    tabBox(width = 12,
                           tabPanel("Correlation Matrix",
                                      column(width=8,
                                             box(status = "success", width=NULL,
                                                 plotOutput(outputId = 'cor_m'))),
                                      column(width=4,
                                             fluidRow(
                                                infoBox(title = "Highly Correlated", 
                                                     color = "blue", width=NULL,
                                                     icon=icon("compress-arrows-alt"),
                                                     value=tags$p(style="font-size: 100%;"),
                                                     HTML(paste("Grade vs Interest Rate", br(),
                                                     "Loan Amount vs Installment")))),
                                             fluidRow(
                                                infoBox(title = "Moderately Correlated", 
                                                    color = "purple", width=NULL,
                                                    icon=icon("compress-alt"),
                                                    value=tags$p(style="font-size: 100%;"),
                                                    HTML(paste("Term vs Grade", br(),
                                                    "Term vs Interest Rate", br(),
                                                    "Loan Amount vs Term", br(),
                                                    "Annual Income vs Loan Amount", br(),
                                                    "Annual Income vs Installment")))))),
                          
                             
                            tabPanel("Installment",
                                         selectInput("installmt", "Select Variable:",
                                                     choices = c('By Loan Grade' = 'by_grade',
                                                                 'By Purpose' = 'by_purpose')),
                                        fluidRow(
                                          box(status = "success",
                                              plotOutput(outputId = "ci_plot2")),
                                          box(status = "success",
                                              plotOutput(outputId = "boxplot2"))),
                                        fluidRow(
                                          DTOutput(outputId = "summary2"))),
                                
                              tabPanel("Annual Income",
                                         selectInput("income", "Select Variable:",
                                                     choices = c('By Loan Grade' = 'by_grade',
                                                                 'By Purpose' = 'by_purpose')),
                                        fluidRow(
                                          box(status = "success",
                                            plotOutput(outputId = "ci_plot3")),
                                          box(status = "success",
                                              plotOutput(outputId = "boxplot3"))),
                                        fluidRow(
                                          DTOutput(outputId = "summary3"))),
                               
                              tabPanel("Credit Limit",
                                        selectInput("credit_limit", "Select Variable:",
                                                    choices = c('By Loan Grade' = 'by_grade',
                                                                'By Purpose' = 'by_purpose')),
                                        fluidRow(
                                          box(status = "success",
                                              plotOutput(outputId = "ci_plot4")),
                                          box(status = "success",
                                              plotOutput(outputId = "boxplot4"))),
                                        fluidRow(
                                          DTOutput(outputId = "summary4"))),
                               
                              tabPanel("Loan Amount",
                                        selectInput("funded", "Select Variable:",
                                                    choices = c('By Loan Grade' = 'by_grade',
                                                                'By Purpose' = 'by_purpose')),
                                        fluidRow(
                                          box(status = "success",
                                              plotOutput(outputId = "ci_plot5")),
                                          box(status = "success",
                                              plotOutput(outputId = "boxplot5"))),
                                        fluidRow(
                                          DTOutput(outputId = "summary5"))),
                               
                              tabPanel("Term",
                                        selectInput("terms1", "Select Variable:",
                                                    choices = c('By Loan Grade' = "by_grade",
                                                                'By Purpose' = "by_purpose")),
                                        box(status = "success", width=12,
                                            plotOutput("term_bar"))),
                               
                              tabPanel("Employment",
                                       fluidPage(
                                         p(" "),
                                         p(strong("Word Cloud for Employment Titles")),
                                         sidebarLayout(sidebarPanel(
                                           sliderInput("freq",
                                                       "Minimum Frequency:",
                                                       min = 1,  max = 50, value = 15),
                                           sliderInput("max",
                                                       "Maximum Number of Words:",
                                                       min = 1,  max = 300,  value = 100)),
                                           mainPanel(plotOutput("wdcloud"))))),
                              
                              tabPanel("Map",
                                       box(status = "success", width=12, solidHeader = TRUE,
                                           title = "Default Rate by States",
                                           leafletOutput("mymap")))
                               
                               
                           )))),
        
        tabItem(
          tabName = "LoanGrade",
          fluidPage(
            #titlePanel("Loan Grade Analysis"),
            fluidRow(
              tabBox(
                width = 12,
                  tabPanel("Fully Paid vs Default",
                           fluidRow(
                             box(status = "success", width=12,
                                 plotOutput("overallbarplot"),
                                 DTOutput("summary_barplot")))),
                  tabPanel("Interest Rate by Loan Grade",
                           fluidRow(
                             box(status = "success", width = 8,
                                 plotOutput(outputId = "boxplot1")),
                             box(status = "success", width = 4,
                                 title = "Outliers", solidHeader = TRUE,
                                 DTOutput(outputId = "summary1")))),
                  tabPanel("Loan Grade Prices by Year",
                           selectInput("input_grade", "Select Grade:",
                                       choices = c("ALL","A"='A',"B"='B',"C"='C',"D"='D',"E"='E',"F"='F')),
                           fluidRow(
                             box(status = "success", plotOutput("bondrate")),
                             box(status = "success", plotOutput("defaultratio")))),
                  tabPanel("Default Rate Analysis",
                           fluidRow(
                             box(status = "success", plotOutput("chisquareplot1")),
                             box(status = "success", plotOutput("chisquareplot2"))),
                           fluidRow(
                             box(status = "success", width=4, 
                                 title = "Non-defaulted Loans tend to have Higher Average Annual Income",
                                 plotOutput("ztest_inc")),
                             box(status = "success", width=4, 
                                 title = "Defaulted Loans tend to have taken a Higher Average Loan Amount",
                                 plotOutput("ztest_loanamt")),
                             box(status = "success", width=4, 
                                 title = "Defaulted Loans tend to have Higher Debt-To-Income (DTI) Ratio",
                                 plotOutput("ztest_dti")))
                           )
                  )))),
        
        tabItem(
          tabName = "Predict",
          fluidPage(
            #titlePanel("Predictive Analysis"),
            fluidRow(#Filter
              column(3, 
                     div(style = "margin-right:-60em",
                         fluidRow(box(title = "Client's Information",
                                      width = 3,
                                      numericInput(inputId = "loan_amnt",label = "Loan Amount (USD)",
                                                   value = 10000),
                                      numericInput(inputId = "annual_inc",label = "Annual Income (USD)",
                                                   value = 1000000),
                                      selectInput(inputId = "home_ownership", label = "Home Ownership",
                                                  choices = c("RENT","OWN","MORTGAGE","NONE","ANY"),
                                                  selected ='RENT'),
                                      selectInput(inputId = "emp_length", label = "Employment Length",
                                                  choices = c("< 1 year","1 year","2 years","3 years",
                                                              "4 years","5 years","6 years","7 years",
                                                              "8 years","9 years","10+ years"),
                                                  selected ='10+ years'),
                                      selectInput(inputId = "grade", label = "Grade",
                                                  choices = c("A","B","C","D","E","F","G"),
                                                  selected ='A'),
                                      numericInput(inputId = "installment",label = "Monthly Installment Amount (USD)",
                                                   value = 1000),
                                      numericInput(inputId = "int_rate",label = "Loan Interest Rate (%)",
                                                   value = 15.61),
                                      selectInput(inputId = "term", label = "Loan Term",
                                                  choices = c(" 36 months"," 60 months"),
                                                  selected =' 60 months'),
                                      numericInput(inputId = "delinq_2yrs",label = "Number of Delinquencies",
                                                   value = 0),
                                      numericInput(inputId = "dti",label = "Debt-to-Income Ratio",
                                                   value = 7.5))))),
              
              
              column(8,
                     
                     #Approval Status
                     div(style = "margin-right:-60em",
                         fluidRow(infoBoxOutput("approval_status"))),
                     
                     #tabBox
                     div(style = "margin-right:-60em",
                         fluidRow(tabBox(id = "PA_tabset",
                                         
                                         #Tab 1 (Variables Description)
                                         tabPanel("Variables Description",
                                                  tableOutput("Variables")),
                                         
                                         #Tab 2 (Model Parameters)
                                         tabPanel("Model Parameters",
                                                  plotOutput(outputId = 'roc',width = '500px'),
                                                  verbatimTextOutput('auc'),
                                                  verbatimTextOutput('parameters')))))))
            ))))
                  
                               
                               



##################
####    UI    ####
##################
ui <- dashboardPage(
    dashboardHeader(title = "ASAR G7 - Lending Club Analysis", 
                    titleWidth = 400),
    sidebar,
    body)


##################
####  Server  ####
##################
server <- function(input, output) {

  
####################
####  Overview  ####
####################    

### Installment ###
  output$summary2 <- renderDT({
    if(input$installmt == "by_grade") {
      summary_gen_instlm1 <- data %>%
        select(installment, grade) %>%
        group_by(grade) %>%
        dplyr::summarise(Min = min(installment, na.rm = TRUE), 
                  Max = max(installment, na.rm = TRUE), 
                  Mean = mean(installment, na.rm = TRUE),
                  Sd = sd(installment, na.rm = TRUE),
                  Median = median(installment, na.rm = TRUE))
      summary_gen_instlm2 <- data %>%
        dplyr::count(grade)
      summary_gen_instlm <- merge(summary_gen_instlm1, summary_gen_instlm2, by = "grade")
      summary_gen_instlm$lower_CI <- summary_gen_instlm$Mean - qnorm(0.975)* summary_gen_instlm$Sd/sqrt(summary_gen_instlm$n)
      summary_gen_instlm$upper_CI <- summary_gen_instlm$Mean + qnorm(0.975)* summary_gen_instlm$Sd/sqrt(summary_gen_instlm$n)
    }
      else if(input$installmt == "by_purpose") {
        summary_gen_instlm1 <- data %>%
          select(installment, g_purpose) %>%
          group_by(g_purpose) %>%
          dplyr::summarise(Min = min(installment, na.rm = TRUE), 
                  Max = max(installment, na.rm = TRUE), 
                  Mean = mean(installment, na.rm = TRUE),
                  Sd = sd(installment, na.rm = TRUE),
                  Median = median(installment, na.rm = TRUE))
        summary_gen_instlm2 <- data %>%
          dplyr::count(g_purpose)
        summary_gen_instlm <- merge(summary_gen_instlm1, summary_gen_instlm2, by = "g_purpose")
        summary_gen_instlm$lower_CI <- summary_gen_instlm$Mean - qnorm(0.975)* summary_gen_instlm$Sd/sqrt(summary_gen_instlm$n)
        summary_gen_instlm$upper_CI <- summary_gen_instlm$Mean + qnorm(0.975)* summary_gen_instlm$Sd/sqrt(summary_gen_instlm$n)
      }
    return(summary_gen_instlm)
  }, options=list(bLengthChange=0, # show/hide records per page dropdown
                  bFilter=0)  # global search box on/off
  )
  
  output$ci_plot2 <- renderPlot({
    if(input$installmt == "by_grade") {
      summary_gen_instlm1 <- data %>%
        select(installment, grade) %>%
        group_by(grade) %>%
        dplyr::summarise(Mean = mean(installment, na.rm = TRUE),
                  Sd = sd(installment, na.rm = TRUE))
      summary_gen_instlm2 <- data %>%
        dplyr::count(grade)
      summary_gen_instlm <- merge(summary_gen_instlm1, summary_gen_instlm2, by = "grade")
      summary_gen_instlm$lower_CI <- summary_gen_instlm$Mean - qnorm(0.975)* summary_gen_instlm$Sd/sqrt(summary_gen_instlm$n)
      summary_gen_instlm$upper_CI <- summary_gen_instlm$Mean + qnorm(0.975)* summary_gen_instlm$Sd/sqrt(summary_gen_instlm$n)
      
      ci_plot <- ggplot(summary_gen_instlm, aes(Mean, grade))
      ci_plot <- ci_plot + geom_point() +
        geom_errorbarh(aes(xmax = upper_CI, xmin = lower_CI))+
        theme_minimal() +
        labs(title = 'CI Plot of Installment by Loan Grade',
          x = "Installment", y="Loan Grade")
    } 
      else if(input$installmt == "by_purpose") {
        summary_gen_instlm1 <- data %>%
          select(installment, g_purpose) %>%
          group_by(g_purpose) %>%
          dplyr::summarise(Mean = mean(installment, na.rm = TRUE),
                    Sd = sd(installment, na.rm = TRUE))
        summary_gen_instlm2 <- data %>%
          dplyr::count(g_purpose)
        summary_gen_instlm <- merge(summary_gen_instlm1, summary_gen_instlm2, by = "g_purpose")
        summary_gen_instlm$lower_CI <- summary_gen_instlm$Mean - qnorm(0.975)* summary_gen_instlm$Sd/sqrt(summary_gen_instlm$n)
        summary_gen_instlm$upper_CI <- summary_gen_instlm$Mean + qnorm(0.975)* summary_gen_instlm$Sd/sqrt(summary_gen_instlm$n)
      
        ci_plot <- ggplot(summary_gen_instlm, aes(Mean, g_purpose))
        ci_plot <- ci_plot + geom_point() +
          geom_errorbarh(aes(xmax = upper_CI, xmin = lower_CI))+
          theme_minimal() +
          labs(title = 'CI Plot of Installment by Purpose',
            x = "Installment", y="Loan Purpose") 
    }
  return(ci_plot)
  })
  
  output$boxplot2 <- renderPlot({
    if (input$installmt == "by_grade") {
      bplot <- ggplot(data, aes(y = installment, x = grade, fill=grade))+
        geom_boxplot(show.legend = FALSE)+
        scale_fill_brewer()+
        theme_minimal() +
        labs(title = 'Boxplot of Installment by Loan Grade',
             x = 'Loan Grade', y = 'Installment')
    }
      else if(input$installmt == "by_purpose") {
        bplot <- ggplot(data, aes(y = installment, x = g_purpose, fill=g_purpose))+
          geom_boxplot(show.legend = FALSE)+
          scale_fill_brewer()+
          theme_minimal() +
          labs(title = 'Boxplot of Installment by Loan Purpose',
               x = 'Loan Purpose', y = 'Installment')+
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    return(bplot)
  })  
    

### Annual Income ###
  output$summary3 <- renderDT({
    if(input$income == "by_grade") {
      summary_gen_inc1 <- data %>%
        select(annual_inc, grade) %>%
        group_by(grade) %>%
        dplyr::summarise(Min = min(annual_inc, na.rm = TRUE), 
                  Max = max(annual_inc, na.rm = TRUE), 
                  Mean = mean(annual_inc, na.rm = TRUE),
                  Sd = sd(annual_inc, na.rm = TRUE),
                  Median = median(annual_inc, na.rm = TRUE))
      summary_gen_inc2 <- data %>%
        dplyr::count(grade)
      summary_gen_inc <- merge(summary_gen_inc1, summary_gen_inc2, by = "grade")
      summary_gen_inc$lower_CI <- summary_gen_inc$Mean - qnorm(0.975)* summary_gen_inc$Sd/sqrt(summary_gen_inc$n)
      summary_gen_inc$upper_CI <- summary_gen_inc$Mean + qnorm(0.975)* summary_gen_inc$Sd/sqrt(summary_gen_inc$n)
    }
      else if(input$income == "by_purpose") {
        summary_gen_inc1 <- data %>%
          select(annual_inc, g_purpose) %>%
          group_by(g_purpose) %>%
          dplyr::summarise(Min = min(annual_inc, na.rm = TRUE), 
                    Max = max(annual_inc, na.rm = TRUE), 
                    Mean = mean(annual_inc, na.rm = TRUE),
                    Sd = sd(annual_inc, na.rm = TRUE),
                    Median = median(annual_inc, na.rm = TRUE))
        summary_gen_inc2 <- data %>%
          dplyr::count(g_purpose)
        summary_gen_inc <- merge(summary_gen_inc1, summary_gen_inc2, by = "g_purpose")
        summary_gen_inc$lower_CI <- summary_gen_inc$Mean - qnorm(0.975)* summary_gen_inc$Sd/sqrt(summary_gen_inc$n)
        summary_gen_inc$upper_CI <- summary_gen_inc$Mean + qnorm(0.975)* summary_gen_inc$Sd/sqrt(summary_gen_inc$n)
    }
    return(summary_gen_inc)
  }, options=list(bLengthChange=0, # show/hide records per page dropdown
                  bFilter=0)  # global search box on/off
  )
  
  output$ci_plot3 <- renderPlot({
    if(input$income == "by_grade") {
      summary_gen_inc1 <- data %>%
        select(annual_inc, grade) %>%
        group_by(grade) %>%
        dplyr::summarise(Mean = mean(annual_inc, na.rm = TRUE),
                  Sd = sd(annual_inc, na.rm = TRUE))
      summary_gen_inc2 <- data %>%
        dplyr::count(grade)
      summary_gen_inc <- merge(summary_gen_inc1, summary_gen_inc2, by = "grade")
      summary_gen_inc$lower_CI <- summary_gen_inc$Mean - qnorm(0.975)* summary_gen_inc$Sd/sqrt(summary_gen_inc$n)
      summary_gen_inc$upper_CI <- summary_gen_inc$Mean + qnorm(0.975)* summary_gen_inc$Sd/sqrt(summary_gen_inc$n)
      
      ci_plot <- ggplot(summary_gen_inc, aes(Mean, grade))
      ci_plot <- ci_plot + geom_point() +
        geom_errorbarh(aes(xmax = upper_CI, xmin = lower_CI))+
        theme_minimal() +
        labs(title = 'CI Plot of Annual Income by Loan Grade',
          x = "Annual Income", y="Loan Grade")
    }
      else if(input$income == "by_purpose") {
        summary_gen_inc1 <- data %>%
          select(annual_inc, g_purpose) %>%
          group_by(g_purpose) %>%
          dplyr::summarise(Mean = mean(annual_inc, na.rm = TRUE),
                    Sd = sd(annual_inc, na.rm = TRUE))
        summary_gen_inc2 <- data %>%
          dplyr::count(g_purpose)
        summary_gen_inc <- merge(summary_gen_inc1, summary_gen_inc2, by = "g_purpose")
        summary_gen_inc$lower_CI <- summary_gen_inc$Mean - qnorm(0.975)* summary_gen_inc$Sd/sqrt(summary_gen_inc$n)
        summary_gen_inc$upper_CI <- summary_gen_inc$Mean + qnorm(0.975)* summary_gen_inc$Sd/sqrt(summary_gen_inc$n)
      
        ci_plot <- ggplot(summary_gen_inc, aes(Mean, g_purpose))
        ci_plot <- ci_plot + geom_point() +
          geom_errorbarh(aes(xmax = upper_CI, xmin = lower_CI))+
          theme_minimal() +
          labs(title = 'CI Plot of Annual Income by Purpose',
            x = "Annual Income", y="Loan Purpose") 
    }
    return(ci_plot)
  })
  
  output$boxplot3 <- renderPlot({
    if (input$income == "by_grade") {
      bplot <- ggplot(data, aes(y = annual_inc, x = grade, fill=grade))+
        geom_boxplot(show.legend = FALSE)+
        scale_fill_brewer()+
        theme_minimal() +
        scale_y_continuous(name = "Annual Income (thousands)", labels = function(y) y / 1000) +
        labs(title = 'Boxplot of Annual Income by Loan Grade',
             x = 'Loan Grade')
    }
      else if(input$income == "by_purpose") {
        bplot <- ggplot(data, aes(y = annual_inc, x = g_purpose, fill=g_purpose))+
          geom_boxplot(show.legend = FALSE)+
          scale_fill_brewer()+
          theme_minimal() +
          scale_y_continuous(name = "Annual Income (thousands)", labels = function(y) y / 1000) +
          labs(title = 'Boxplot of Annual Income by Loan Purpose',
               x = 'Loan Purpose')+
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    return(bplot)
  })      
  
    
### Total High Credit Limit ###
  output$summary4 <- renderDT({
    if(input$credit_limit == "by_grade") {
      summary_gen_credlim1 <- data %>%
        select(tot_hi_cred_lim, grade) %>%
        group_by(grade) %>%
        dplyr::summarise(Min = min(tot_hi_cred_lim, na.rm = TRUE), 
                  Max = max(tot_hi_cred_lim, na.rm = TRUE), 
                  Mean = mean(tot_hi_cred_lim, na.rm = TRUE),
                  Sd = sd(tot_hi_cred_lim, na.rm = TRUE),
                  Median = median(tot_hi_cred_lim, na.rm = TRUE))
      summary_gen_credlim2 <- data %>%
        dplyr::count(grade)
      summary_gen_credlim <- merge(summary_gen_credlim1, summary_gen_credlim2, by = "grade")
      summary_gen_credlim$lower_CI <- summary_gen_credlim$Mean - qnorm(0.975)* summary_gen_credlim$Sd/sqrt(summary_gen_credlim$n)
      summary_gen_credlim$upper_CI <- summary_gen_credlim$Mean + qnorm(0.975)* summary_gen_credlim$Sd/sqrt(summary_gen_credlim$n)
    }
      else if(input$credit_limit == "by_purpose") {
        summary_gen_credlim1 <- data %>%
          select(tot_hi_cred_lim, g_purpose) %>%
          group_by(g_purpose) %>%
          dplyr::summarise(Min = min(tot_hi_cred_lim, na.rm = TRUE), 
                    Max = max(tot_hi_cred_lim, na.rm = TRUE), 
                    Mean = mean(tot_hi_cred_lim, na.rm = TRUE),
                    Sd = sd(tot_hi_cred_lim, na.rm = TRUE),
                    Median = median(tot_hi_cred_lim, na.rm = TRUE))
        summary_gen_credlim2 <- data %>%
          dplyr::count(g_purpose)
        summary_gen_credlim <- merge(summary_gen_credlim1, summary_gen_credlim2, by = "g_purpose")
        summary_gen_credlim$lower_CI <- summary_gen_credlim$Mean - qnorm(0.975)* summary_gen_credlim$Sd/sqrt(summary_gen_credlim$n)
        summary_gen_credlim$upper_CI <- summary_gen_credlim$Mean + qnorm(0.975)* summary_gen_credlim$Sd/sqrt(summary_gen_credlim$n)
    }
    return(summary_gen_credlim)
  }, options=list(bLengthChange=0, # show/hide records per page dropdown
                  bFilter=0)  # global search box on/off
  )
  
  output$ci_plot4 <- renderPlot({
    if(input$credit_limit == "by_grade") {
      summary_gen_credlim1 <- data %>%
        select(tot_hi_cred_lim, grade) %>%
        group_by(grade) %>%
        dplyr::summarise(Mean = mean(tot_hi_cred_lim, na.rm = TRUE),
                  Sd = sd(tot_hi_cred_lim, na.rm = TRUE))
      summary_gen_credlim2 <- data %>%
        dplyr::count(grade)
      summary_gen_credlim <- merge(summary_gen_credlim1, summary_gen_credlim2, by = "grade")
      summary_gen_credlim$lower_CI <- summary_gen_credlim$Mean - qnorm(0.975)* summary_gen_credlim$Sd/sqrt(summary_gen_credlim$n)
      summary_gen_credlim$upper_CI <- summary_gen_credlim$Mean + qnorm(0.975)* summary_gen_credlim$Sd/sqrt(summary_gen_credlim$n)
      
      ci_plot <- ggplot(summary_gen_credlim, aes(Mean, grade))
      ci_plot <- ci_plot + geom_point() +
        geom_errorbarh(aes(xmax = upper_CI, xmin = lower_CI))+
        theme_minimal() +
        labs(title="CI Plot of Credit Limit by Loan Grade",
          x = "Credit Limit", y="Loan Grade")
    }
      else if(input$credit_limit == "by_purpose") {
        summary_gen_credlim1 <- data %>%
          select(tot_hi_cred_lim, g_purpose) %>%
          group_by(g_purpose) %>%
          dplyr::summarise(Mean = mean(tot_hi_cred_lim, na.rm = TRUE),
                    Sd = sd(tot_hi_cred_lim, na.rm = TRUE))
        summary_gen_credlim2 <- data %>%
          dplyr::count(g_purpose)
        summary_gen_credlim <- merge(summary_gen_credlim1, summary_gen_credlim2, by = "g_purpose")
        summary_gen_credlim$lower_CI <- summary_gen_credlim$Mean - qnorm(0.975)* summary_gen_credlim$Sd/sqrt(summary_gen_credlim$n)
        summary_gen_credlim$upper_CI <- summary_gen_credlim$Mean + qnorm(0.975)* summary_gen_credlim$Sd/sqrt(summary_gen_credlim$n)
      
        ci_plot <- ggplot(summary_gen_credlim, aes(Mean, g_purpose))
        ci_plot <- ci_plot + geom_point() +
          geom_errorbarh(aes(xmax = upper_CI, xmin = lower_CI))+
          theme_minimal() +
          labs(title="CI Plot of Credit Limit by Loan Purpose",
               x = "Credit Limit", y="Loan Purpose") 
    }
    return(ci_plot)
  })
  
  output$boxplot4 <- renderPlot({
    if (input$credit_limit == "by_grade") {
      bplot <- ggplot(data, aes(y = tot_hi_cred_lim, x = grade, fill=grade))+
        geom_boxplot(show.legend = FALSE)+
        scale_fill_brewer()+
        theme_minimal() +
        scale_y_continuous(name = "Credit Limit (thousands)", labels = function(y) y / 1000) +
        labs(title = 'Boxplot of Credit Limit by Loan Grade',
             x = 'Loan Grade', y="Credit Limit") +
        ylim(0, 50000)
    }
      else if(input$credit_limit == "by_purpose") {
        bplot <- ggplot(data, aes(y = tot_hi_cred_lim, x = g_purpose, fill=g_purpose))+
         geom_boxplot(show.legend = FALSE)+
          scale_fill_brewer()+
          theme_minimal() +
          scale_y_continuous(name = "Credit Limit (thousands)", labels = function(y) y / 1000) +
          labs(title = 'Boxplot of Credit Limit by Loan Purpose',
               x = 'Loan Purpose', y="Credit Limit")+
         ylim(0, 50000) +
         theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    return(bplot)
  })        
  

### Loan Amount ###
  output$summary5 <- renderDT({
    if(input$funded == "by_grade") {
      summary_gen_fundamt1 <- data %>%
        select(funded_amnt, grade) %>%
        group_by(grade) %>%
        dplyr::summarise(Min = min(funded_amnt, na.rm = TRUE), 
                  Max = max(funded_amnt, na.rm = TRUE), 
                  Mean = mean(funded_amnt, na.rm = TRUE),
                  Sd = sd(funded_amnt, na.rm = TRUE),
                  Median = median(funded_amnt, na.rm = TRUE))
      summary_gen_fundamt2 <- data %>%
        dplyr::count(grade)
      summary_gen_fundamt <- merge(summary_gen_fundamt1, summary_gen_fundamt2, by = "grade")
      summary_gen_fundamt$lower_CI <- summary_gen_fundamt$Mean - qnorm(0.975)* summary_gen_fundamt$Sd/sqrt(summary_gen_fundamt$n)
      summary_gen_fundamt$upper_CI <- summary_gen_fundamt$Mean + qnorm(0.975)* summary_gen_fundamt$Sd/sqrt(summary_gen_fundamt$n)
    }
    else if(input$funded == "by_purpose") {
      summary_gen_fundamt1 <- data %>%
        select(funded_amnt, g_purpose) %>%
        group_by(g_purpose) %>%
        dplyr::summarise(Min = min(funded_amnt, na.rm = TRUE), 
                  Max = max(funded_amnt, na.rm = TRUE), 
                  Mean = mean(funded_amnt, na.rm = TRUE),
                  Sd = sd(funded_amnt, na.rm = TRUE),
                  Median = median(funded_amnt, na.rm = TRUE))
      summary_gen_fundamt2 <- data %>%
        dplyr::count(g_purpose)
      summary_gen_fundamt <- merge(summary_gen_fundamt1, summary_gen_fundamt2, by = "g_purpose")
      summary_gen_fundamt$lower_CI <- summary_gen_fundamt$Mean - qnorm(0.975)* summary_gen_fundamt$Sd/sqrt(summary_gen_fundamt$n)
      summary_gen_fundamt$upper_CI <- summary_gen_fundamt$Mean + qnorm(0.975)* summary_gen_fundamt$Sd/sqrt(summary_gen_fundamt$n)
    }
    return(summary_gen_fundamt)
  }, options=list(bLengthChange=0, # show/hide records per page dropdown
                  bFilter=0)  # global search box on/off
  )
  
  output$ci_plot5 <- renderPlot({
    if(input$funded == "by_grade") {
      summary_gen_fundamt1 <- data %>%
        select(funded_amnt, grade) %>%
        group_by(grade) %>%
        dplyr::summarise(Mean = mean(funded_amnt, na.rm = TRUE),
                  Sd = sd(funded_amnt, na.rm = TRUE))
      summary_gen_fundamt2 <- data %>%
        dplyr::count(grade)
      summary_gen_fundamt <- merge(summary_gen_fundamt1, summary_gen_fundamt2, by = "grade")
      summary_gen_fundamt$lower_CI <- summary_gen_fundamt$Mean - qnorm(0.975)* summary_gen_fundamt$Sd/sqrt(summary_gen_fundamt$n)
      summary_gen_fundamt$upper_CI <- summary_gen_fundamt$Mean + qnorm(0.975)* summary_gen_fundamt$Sd/sqrt(summary_gen_fundamt$n)
      
      ci_plot <- ggplot(summary_gen_fundamt, aes(Mean, grade))
      ci_plot <- ci_plot + geom_point() +
        geom_errorbarh(aes(xmax = upper_CI, xmin = lower_CI))+
        theme_minimal() +
        labs(title = 'CI Plot of Loan Amount by Loan Grade',
          x = "Loan Amount", y="Loan Grade")
    }
      else if(input$funded == "by_purpose") {
        summary_gen_fundamt1 <- data %>%
          select(funded_amnt, g_purpose) %>%
          group_by(g_purpose) %>%
          dplyr::summarise(Mean = mean(funded_amnt, na.rm = TRUE),
                    Sd = sd(funded_amnt, na.rm = TRUE))
        summary_gen_fundamt2 <- data %>%
          dplyr::count(g_purpose)
        summary_gen_fundamt <- merge(summary_gen_fundamt1, summary_gen_fundamt2, by = "g_purpose")
        summary_gen_fundamt$lower_CI <- summary_gen_fundamt$Mean - qnorm(0.975)* summary_gen_fundamt$Sd/sqrt(summary_gen_fundamt$n)
        summary_gen_fundamt$upper_CI <- summary_gen_fundamt$Mean + qnorm(0.975)* summary_gen_fundamt$Sd/sqrt(summary_gen_fundamt$n)
        
        ci_plot <- ggplot(summary_gen_fundamt, aes(Mean, g_purpose))
        ci_plot <- ci_plot + geom_point() +
          geom_errorbarh(aes(xmax = upper_CI, xmin = lower_CI))+
          theme_minimal() +
          labs(title = 'CI Plot of Loan Amount by Loan Purpose',
               x = "Loan Amount", y="Loan Purpose") 
    }
    return(ci_plot)
  })
  
  output$boxplot5 <- renderPlot({
    if (input$funded == "by_grade") {
      bplot <- ggplot(data, aes(y = funded_amnt, x = grade, fill=grade))+
        geom_boxplot(show.legend = FALSE)+
        scale_fill_brewer()+
        theme_minimal() +
        labs(title = 'Boxplot of Loan Amount by Loan Grade',
             x = 'Loan Grade', y = 'Loan Amount')
    }
      else if(input$funded == "by_purpose") {
        bplot <- ggplot(data, aes(y = funded_amnt, x = g_purpose, fill=g_purpose))+
          geom_boxplot(show.legend = FALSE)+
          scale_fill_brewer()+
          theme_minimal() +
          labs(title = 'Boxplot of Loan Amount by Loan Purpose',
               x = 'Loan Purpose', y = 'Loan Amount')+
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    return(bplot)
  })          
  

##### Term #####
  output$term_bar <- renderPlot({
    if (input$terms1 == "by_grade") {
      bchart <- ggplot(data) + 
        aes(factor(grade), fill=term) + 
        geom_bar(stat="count")+
        scale_y_continuous(name = "Count (thousands)", labels = function(y) y / 1000) +
        labs(title="Count of Terms by Loan Grade", x="Loan Grade", fill="Terms") +
        scale_fill_brewer(palette="Paired")+
        theme_minimal()
    }
      else if(input$terms1 == "by_purpose") {
        bchart <- ggplot(data) + 
          aes(factor(g_purpose), fill=term) + 
          geom_bar(stat="count")+
          scale_y_continuous(name = "Count (thousands)", labels = function(y) y / 1000) +
          labs(title="Count of Terms by Purpose", x="Loan Purpose", fill="Terms") +
          scale_fill_brewer(palette="Paired")+
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
    }
    return(bchart)
  })    

  
####################    
#### Loan Grade ####
####################  
  
  output$overallbarplot = renderPlot({
    tempdata<-data%>%filter(`loan_status` %in% c("Fully Paid","Default","Charged Off"))
    tempdata2<-tempdata%>%mutate(loan_status2= if_else(loan_status=="Fully Paid","Fully Paid","Default"))
    tempdata4<-tempdata2%>%group_by(`grade`,`loan_status2`)
    tempdata5<-tempdata4%>%dplyr::summarise(count = n())%>%
      arrange(`grade`,desc(`loan_status2`))
    
    ggplot(tempdata5,aes(x=`grade`,y=`count`,fill=`loan_status2`,label=`count`))+
      geom_bar(stat='identity')+
      scale_y_continuous(name = "Count (thousands)", labels = function(y) y / 1000) +
      geom_text(size=3,position=position_stack(vjust=0.5))+
      scale_fill_brewer(palette="Paired")+
      theme_minimal() +
      labs(title="Fully Paid vs. Default For Each Grade",
           x="Loan Grade", fill="Loan Status")
    
  })
  
  output$summary_barplot <- renderDT({
    summ<- data%>% group_by(`grade`)%>%
      dplyr::summarise(count = n(),min = min(int_rate), max= max(int_rate),mean = mean(int_rate), 
                       median = median(int_rate),sd= sd(int_rate))
    return(summ)
  }, options=list(bLengthChange=0, # show/hide records per page dropdown
                  bFilter=0)  # global search box on/off
  )
  
  
### Loan Grade vs Interest Rate ###
  output$summary1 <- renderDT({
    test<-tempdata2%>%filter(`grade` %in% c("B","C","D","E","F","G") & int_rate <=7)
    tb_loangrade<- data.frame(test%>%group_by(`grade`,`loan_status2`)%>%dplyr::summarise(count=n()))
    colnames(tb_loangrade) <- c("Grade", "Loan Status", "Count")
    return(tb_loangrade)
    }, options=list(iDisplayLength=20,  # initial number of records
                    bLengthChange=0, # show/hide records per page dropdown
                      bFilter=0)  # global search box on/off
    )

  
  output$boxplot1 <- renderPlot({
    tempdata<-data%>%filter(`loan_status` %in% c("Fully Paid","Default","Charged Off"))
    tempdata2<-tempdata%>%mutate(loan_status2= if_else(loan_status=="Fully Paid","Fully Paid","Default"))
    ggplot(data=tempdata2, aes(x=`grade`,y=`int_rate`,fill=`grade`))+
      geom_boxplot()+
      labs(title = 'Boxplot of Interest Rate by Loan Grade',
           x = 'Loan Grade', y = 'Interest Rate')
    })

  
### Loan Grade by Year ###
  output$bondrate = renderPlot({
    if (input$input_grade == 'A') {
      A<-bondyear%>%filter(`grade`=="A")
      ggplot(A)+
        geom_line(mapping = aes(x=`issue_d2`,y=`mInt_Rate`,colour= `grade`))+
        scale_x_date(expand = c(0, 200))+
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
        labs(title="Temporal Analysis of Loan Grade A", 
             x="Issue Date (Year)", y="Interest Rate", colour="Loan Grade")
    }
    else if(input$input_grade == 'B') {
      B<-bondyear%>%filter(`grade`=="B")
      ggplot(B)+
        geom_line(mapping = aes(x=`issue_d2`,y=`mInt_Rate`,colour= `grade`))+
        scale_x_date(expand = c(0, 200))+
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
        labs(title="Temporal Analysis of Loan Grade B", 
             x="Issue Date (Year)", y="Interest Rate", colour="Loan Grade")
    }
    else if(input$input_grade == 'C') {
      C<-bondyear%>%filter(`grade`=="C")
      ggplot(C)+
        geom_line(mapping = aes(x=`issue_d2`,y=`mInt_Rate`,colour= `grade`))+
        scale_x_date(expand = c(0, 200))+
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
        labs(title="Temporal Analysis of Loan Grade C", 
             x="Issue Date (Year)", y="Interest Rate", colour="Loan Grade")
    }
    else if(input$input_grade == 'D') {
      D<-bondyear%>%filter(`grade`=="D")
      ggplot(D)+
        geom_line(mapping = aes(x=`issue_d2`,y=`mInt_Rate`,colour= `grade`))+
        scale_x_date(expand = c(0, 200))+
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
        labs(title="Temporal Analysis of Loan Grade D", 
             x="Issue Date (Year)", y="Interest Rate", colour="Loan Grade")
    }
    else if(input$input_grade == 'E') {
      E<-bondyear%>%filter(`grade`=="E")
      ggplot(E)+
        geom_line(mapping = aes(x=`issue_d2`,y=`mInt_Rate`,colour= `grade`))+
        scale_x_date(expand = c(0, 200))+
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
        labs(title = "Temporal Analysis of Loan Grade E", 
             x="Issue Date (Year)", y="Interest Rate", colour="Loan Grade")
    }
    
    else if(input$input_grade == 'F') {
      F<-bondyear%>%filter(`grade`=="F")
      ggplot(F)+
        geom_line(mapping = aes(x=`issue_d2`,y=`mInt_Rate`,colour= `grade`))+
        scale_x_date(expand = c(0, 200))+
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
        labs(title="Temporal Analysis of Loan Grade F", 
             x="Issue Date (Year)", y="Interest Rate", colour="Loan Grade")
    }
    
    else if(input$input_grade == 'ALL') {
      ggplot(bondyear)+
        geom_line(mapping = aes(x=`issue_d2`,y=`mInt_Rate`,colour= `grade`))+
        scale_x_date(expand = c(0, 200))+
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
        labs(title="Temporal Analysis of Loan Grade Spreads", 
             x="Issue Date (Year)", y="Interest Rate", colour="Loan Grade")
    }
  })
  
  output$defaultratio = renderPlot({
    ggplot(bondyear)+
      scale_x_date(expand = c(0, 200))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
      geom_line(mapping = aes(x=`issue_d2`,y=`default_ratio`,colour= `grade`))+
      labs(title = "Default Ratio by Grade", 
           x="Issue Date (Year)", y="Default Ratio", colour="Loan Grade")
  })
  
 
####################    
#### Chi-Square ####
####################
  output$chisquareplot1 = renderPlot({
    tempdata<-data%>%filter(`loan_status` %in% c("Fully Paid","Default","Charged Off"))
    tempdata2<-tempdata%>%mutate(loan_status2= if_else(loan_status=="Fully Paid","Fully Paid","Default"))
    tbl<-xtabs(~ loan_status2 + delinquent, data = tempdata2)
    print(mosaic(~loan_status2+delinquent,data=tbl,
                 main="Do Previous Delinquencies Lead to Default?",
                 shade=TRUE,
                 legend=TRUE))
    
  })
  
  output$chisquareplot2 = renderPlot({
    tempdata<-data%>%filter(`loan_status` %in% c("Fully Paid","Default","Charged Off"))
    tempdata2<-tempdata%>%mutate(loan_status2= if_else(loan_status=="Fully Paid","Fully Paid","Default"))
    tempdata3<-tempdata2%>%mutate(emp_length_recoded=case_when(emp_length %in% c("< 1 year","1 year","2 years","3 years")~"<=3yrs",
                                                               emp_length %in% c("4 years","5 years","6 years","7 years","8 years","9 years")~"4to9yrs",
                                                               emp_length== "10+ years"~">=10years"))
    tbl2<-xtabs(~ loan_status2 + emp_length_recoded, data = tempdata3)
    print(mosaic(~loan_status2+emp_length_recoded,data=tbl2,
                 main="Does Employment Length Affect Default?",
                 shade=TRUE,
                 legend=TRUE))
  })
  

  
##########################    
#### Two Samples Test ####
##########################  
  
# Annual Income 

  output$ztest_inc <- renderPlot({
    ggplot(data = remove_missing(data, na.rm = TRUE, vars = 'default'),
           aes(x=default, y=annual_inc, fill=default)) + 
      geom_boxplot() +
      scale_y_continuous(name = "Annual Income (thousands)", labels = function(y) y / 1000) +
      scale_fill_brewer(palette="Paired")+
      theme_minimal() +
      labs(x="Defaulted", fill="Defaulted")
  })
    

# Loan Amount
  
  output$ztest_loanamt <- renderPlot({
    ggplot(data = remove_missing(data, na.rm = TRUE, vars = 'default'),
           aes(x=default, y=loan_amnt, fill=default)) + 
      geom_boxplot() +
      scale_y_continuous(name = "Loan Amount (thousands)", labels = function(y) y / 1000) +
      scale_fill_brewer(palette="Paired")+
      theme_minimal() +
      labs(x="Defaulted", fill="Defaulted")
  })
  

# DTI Ratio
    
  output$ztest_dti <- renderPlot({
    ggplot(data = remove_missing(data, na.rm = TRUE, vars = 'default'),
           aes(x=default, y=dti, fill=default)) + 
      geom_boxplot() +
      scale_fill_brewer(palette="Paired")+
      theme_minimal() +
      labs(y = "Debt-To-Income Ratio", x="Defaulted", fill="Defaulted") +
      ylim(0, 100)
  })  
      
  
    
####################    
#### Word Cloud ####
####################
  word <- reactive({
    myCorpus <- Corpus(VectorSource(data$emp_title))
    myCorpus <- tm_map(myCorpus, removePunctuation)
    myDTM <- TermDocumentMatrix(myCorpus)
    v <- sort(slam::row_sums(myDTM),decreasing = TRUE)
    data.frame(word=names(v),freq=v) 
  })
  
  output$wdcloud <- renderPlot({
    wordcloud(words = word()$word,
              freq = word()$freq, 
              random.order=FALSE, rot.per=0.3, scale=c(5, 0.5),
              min.freq=input$freq, max.words=input$max,
              colors=brewer.pal(8,"Dark2"))
  })


###############
##### Map #####
###############
  data_input <- reactive({mapdata})
  data_input_ordered <- reactive({
    data_input()[order(match(data_input()$addr_state, US_states$STATE_ABBR)),]
  })
  
  labels <- reactive({
    paste("<p>", data_input_ordered()$addr_state, "</p>",
          "<p>", "Default Rate:", 
          round(data_input_ordered()$Pct_default, digits = 3), "</p>", sep = "")
  })
  
  output$mymap <- renderLeaflet(
    
    leaflet() %>% 
      setView(-96, 37.8, 4) %>% 
      addProviderTiles(providers$Stamen.Toner) %>%
      addPolygons( data = US_states, weight = 1, color = "white", 
                   fillOpacity = 0.8, fillColor = pal(data_input_ordered()$Pct_default),
                   label = lapply(labels(), HTML)) %>%
      addLegend(pal = pal, 
                values = data_input_ordered()$Pct_default, 
                opacity = 0.7, 
                position = "topright")
  )  
  
  
############################    
#### Correlation Matrix ####
############################  

  output$cor_m <- renderPlot({
    raw.data[] <- lapply(raw.data,as.integer)
    corr <- round(cor(raw.data), 1)
    ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE) +
      scale_fill_gradient2(low = "red", high = "blue", mid = "white")
    })
  

############################    
#### Predictive Analysis ###
############################  

  #Predictive Result (Feed to Approval Status)
  pred_result <- reactive({
    df_pred <- data.frame(loan_amnt = input$loan_amnt,
                    annual_inc = input$annual_inc,
                    home_ownership = input$home_ownership,
                    emp_length = input$emp_length,
                    grade = input$grade,
                    installment = input$installment,
                    int_rate  = input$int_rate,
                    term  = input$term, 
                    delinq_2yrs = input$delinq_2yrs,
                    dti = input$dti)
    result <- predict(rose.regressor,newdata = df_pred,type = 'response')
    result #print(result)
  })
  
  #Approval Status
  output$approval_status <- renderInfoBox({
    thumb = ifelse(pred_result()<0.5,'thumbs-down','thumbs-up')
    clr = ifelse(pred_result()<0.5,'orange','green')
    probly = paste(100*round(pred_result(),4),'%')
    default = ifelse(pred_result()<0.5,'Risk of Default','Loan Payments will be on time')
    infoBox("Probability of Paying Loan on Time", 
            probly, default,
            icon = icon(thumb, lib = "glyphicon"),color = clr)})
  
  #Description of Variables
  output$Variables <- renderTable({
    df = data.frame(Variables = c("Loan Amount (USD)",
                                  "Annual Income (USD)",
                                  "Home Ownership",
                                  "Employment Length",
                                  "Grade",
                                  "Monthly Installment Amount (USD)",
                                  "Loan Interest Rate (%)",
                                  "Loan Term",
                                  "Number of Delinquencies",
                                  "Debt-to-Income Ratio"),
                    Explanation = c("The listed amount of the loan applied for by the borrower.",
                                    "The self-reported annual income provided by the borrower during registration.",
                                    "The home ownership status provided by the borrower during registration or obtained from the credit report.",
                                    "Employment length in years.",
                                    "LC assigned loan grade.",
                                    "The monthly payment owed by the borrower if the loan originates.",
                                    "Interest rate on the loan.",
                                    "The number of payments on the loan. Values are in months and can be either 36 or 60.",
                                    "The number of 30+ days past-due incidences of delinquency in the borrower's credit file for the past 2 years.",
                                    "A ratio calculated using the borrower's total monthly debt payments on the total debt obligations, excluding mortgage and the requested LC loan, divided by the borrower's self-reported monthly income.")
    )},striped = TRUE,hover = TRUE,bordered = TRUE
    )
  
  #ROC
  output$roc <- renderPlot({
    ROC_lr <- roc(test.data$loan_status, prob_pred_rose)
    plot(ROC_lr, col = "red", main = "ROC For Logistic Regression")
    text(0.6,0.2,paste("AUC=0.7061"))
  })
  
  #AUC
  output$auc <- renderText({
    ROC_lr <- roc(test.data$loan_status, prob_pred_rose)
    ROC_lr_auc <- auc(ROC_lr)
    paste("Area under curve of logistic regression: ", round(ROC_lr_auc,4))
  })
  
  #Model Parameters
  output$parameters <- renderPrint({
    summary(rose.regressor)
    })  
  
    
}
  

        
# Run the application 
shinyApp(ui = ui, server = server)
