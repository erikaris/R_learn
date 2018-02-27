# load the required packages
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(haven)
library(car)
library(stats)

# data preparation for mosaicplot
sen_data1 <- read_sav("/media/erikaris/DATA/BPS/susenas/1302_sen_maret_2017/Kor17ind_a_diseminasi_1302.sav")
sen_data2 <- read_sav("/media/erikaris/DATA/BPS/susenas/1302_sen_maret_2017/SEN2017_Blok41gab_1302.sav")

eduData <-  recode(sen_data1$r515, "c('0', '1', '2', '3')='SD' ; c('4', '5', '6', '7')='SMP' ; c('8', '9', '10', '11', '12', '13')='SMA' ; c('14', '15', '16', '17', '18', '19')='PT'")

jobData <- recode(sen_data1$r804, "'0'='agriculture'; '2'='industry' ; '6'='trade' ; '8'='accommodation' ; c('14', '15', '16')='govEduHealth' ; c('1', '3', '4', '5', '7', '9', '10', '11', '12', '13', '17', '18', '19', '20')='others'")

eduJobData <- table(eduData, jobData)


# data preparation for bar chart
sen_data2_cons = sen_data2[15:18]
sen_data2_cons_sum = colSums(sen_data2_cons)


## shinydashboard ##
# consists of 3 parts: header, sidebar, body. 

# 1st component = header
header <- dashboardHeader(title = "My Shiny Dashboard", titleWidth = 450)

# 2nd component = sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Education vs Work Field", tabName = "edujob", icon = icon("book")), 
    menuItem("Consumption", tabName = "consSum", icon = icon("cutlery")), 
    menuItem("Widgets", tabName = "widgets", icon = icon("th")), 
    menuItem("Visit My GitHub", icon = icon("github"), href = "https://github.com/erikaris"),
    menuItem("Visit My Blog", icon = icon("globe"), href = "http://erikaris.com")
  )
)

body <- dashboardBody(
  tabItems(
    # 1st tab content
    tabItem(
      tabName = "edujob", 
      # we'll have 2 boxes on the same row. 1 for the graph, another one for widget
      fluidRow(
        # 1st graph
        box(title = "Education vs. Job", 
            status = "primary", 
            solidHeader = TRUE, 
            collapsible = TRUE, 
            plotOutput("eduJobPlot"))
      )
    ),
    
    
    # 2nd tab content
    tabItem(
      tabName = "consSum", 
      # we'll have 2 boxes on the same row. 1 for the graph, another one for widget
      fluidRow(
        # 1st graph
        box(title = "Consumption Distribution", 
            status = "primary", 
            solidHeader = TRUE, 
            collapsible = TRUE, 
            plotOutput("consPlot"))
      )
    )
  )
)

ui <- dashboardPage(
  # title = "asdf"    #  --> will serve as the title of the browser page/tab. optional. 
  header,
  sidebar,
  body, 
  skin = "green"
)


server <- function(input, output) { 
  output$eduJobPlot <- renderPlot(
    mosaicplot(eduJobData, color = 2:3, las = 2, shade=TRUE)
  )
  
  output$consPlot <- renderPlot(
    barplot(sen_data2_cons_sum, main="Comparison of Consumption", 
            xlab="Consumption Types", ylab = "values", col = c("lightblue", "mistyrose", "lightcyan","lavender"))
  )
  
  }
  # output$eduJobPlot <- renderPlot({
  #   
  # })
#}
# 
# 
# output$plot1 <- renderPlot({
#   data <- histdata[seq_len(input$slider)]
#   hist(data)
# })

# run the shinyApp
# it needs 2 params: ui and server
shinyApp(ui, server)

## Body content
# dashboardBody(
#   tabItems(
#     # First tab content
#     tabItem(tabName = "dashboard",
#             fluidRow(
#               box(plotOutput("plot1", height = 250)),
#               
#               box(
#                 title = "Controls",
#                 sliderInput("slider", "Number of observations:", 1, 100, 50)
#               )
#             )
#     ),
#     
#     # Second tab content
#     tabItem(tabName = "widgets",
#             h2("Widgets tab content")
#     )
#   )
# )


