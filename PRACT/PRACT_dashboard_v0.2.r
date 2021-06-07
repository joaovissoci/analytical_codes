library(shiny) # load shiny structure
library(shinydashboard) # load shiny dashboard - tutorial (http://rstudio.github.io/shinydashboard/structure.html#boxes)
library(shinyWidgets) # load shiny widgets - tutorial (http://shinyapps.dreamrs.fr/shinyWidgets/)
library(ggplot2)  # for the diamonds dataset
library(DT) #load dynamic tables  - tutorial (https://rstudio.github.io/DT/server.html)
library(plotly) #load dynamic graphics  - tutorial (https://plotly.com/ggplot2/#basic-charts)
library(reshape2) #load something I need and don`t know why. 
library(tidyverse)
library(lubridate)
library(ddply)
library(plyr)
#bootstrap skins (https://bootswatch.com/3/cosmo/) 
# icon sources (https://fontawesome.com/icons?d=gallery&q=result)
#glyphicons source (https://getbootstrap.com/docs/3.3/components/)

data<-read.csv("/Users/joaovissoci/Downloads/DUKEPRACTR01Trial_DATA_2020-11-24_2003.csv")
start_date<-floor_date(today(), "week", 0) # if Monday the current day
data_date_subset <- subset(data, data$date >= start_date)

# Beggining of the APP

#This header contains an image (*.png) file. 
dbHeader <- dashboardHeader(titleWidth = 440)
dbHeader$children[[2]]$children <- tags$img(src="Duke_global_health_small.png", height=56, width=429)

#begin of the user interface of the app (is always composed of three partes: header, sidebar, and body) 

ui <- dashboardPage(
  dbHeader,
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      
      menuItem("Main", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Charts", icon = icon("bar-chart-o"),
               menuSubItem("Categorical questions", tabName = "cat_question"),
               menuSubItem("Numeric questions", tabName = "num_question")
      ),
      menuItem("Descriptive tables", tabName = "tables", icon = icon("th"))
    )#,
    # sidebarMenuOutput("menu"),
    #cria um menu para dar upload de um arquivo para não precisar ligar direto no server. 
    #tags$hr(), #coloca uma quebrinha na pagina
    #fileInput('idArquivo', 'Select a .csv or .xlsx file to be processed', accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','.csv','.tsv','.xlsx')),
    # tags$em("Max file size of 10mb"),
    #checkboxInput('header', 'Does the file have a header?', TRUE),
    #radioButtons('sep', 'Colunm separator', c("Comma" =',',"Semi-colon" =';',"Tab" ='\t'),','),
    #tags$hr()
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBox(length(data$practid), "Total participants enrolled", icon = icon("hospital-user")),
                valueBox(length(data$enrolled), "Total participants enrolled", icon = icon("hospital-user")),
                valueBoxOutput("progressBox")
                #valueBoxOutput("daysElapsed")
              ),
              fluidRow(
                box(
                  title = "Patients enrolled this week", width = 4, solidHeader = TRUE, status = "success", length(data_date_subset$practid)
                ), #primary, success, info, warning, danger.
                box(
                  title = "Patients randomized this week", width = 4, solidHeader = TRUE, status = "warning", length(data_date_subset$enrolled)
                ),
                box(
                  title = "High level of missings", width = 4, solidHeader = TRUE, status = "danger", "need to be a string"
                )
                #box(
                #  title = "High level of missings", width = 4, solidHeader = TRUE, status = "danger", "Number of questions with more than 70% missing"
              ),
              fluidRow(
                box(
                  title = "Enrollment per Day/Week", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("fig1_enrollment_day", height = 250)
                  )
                ),
              fluidRow(                
                box(
                  title = "Enrollment per Week", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("fig2_enrollment_week", height = 250)
                   )
                )
            ), 
      
      # Second tab content
      tabItem(tabName = "tables",
              fluidRow(
                box(
                title = "Descriptive data", width = 12, status = "primary", solidHeader = TRUE,
                pickerInput(
                  inputId = "tables",
                  selected = colnames(mtcars)[1],
                  label = "Select multiple questions to display", 
                  choices = colnames(mtcars), #drive this attribute to a list or vector with question names to be displayed. 
                  multiple = TRUE
                ))),
              fluidRow(
                box(width = 12, status = "primary", solidHeader = FALSE,
                    DT::dataTableOutput("descrip_table")    
                    
                    ))

      
      ),
      
      # thrid tab content
      tabItem(tabName = "cat_question",
              fluidRow(
                box(
                  title = "Categorical questions descriptive data", width = 12, status = "primary", solidHeader = TRUE,
                  pickerInput(
                    inputId = "cat_quest",
                    selected = colnames(mtcars)[1],
                    label = "Select multiple questions to display", 
                    choices = colnames(mtcars), #drive this attribute to a list or vector with question names to be displayed. 
                    multiple = TRUE
                  ))),
              fluidRow(
                box(width = 12, status = "primary", solidHeader = FALSE,
                    plotlyOutput("fig1_catg", height = 250, width = 600)))
      ),
      
      # fourth tab content
      tabItem(tabName = "num_question",
              fluidRow(
                box(
                  title = "Numerical questions descriptive data", width = 12, status = "primary", solidHeader = TRUE,
                  pickerInput(
                    inputId = "num_quest",
                    selected = colnames(mtcars)[1],
                    label = "Select multiple questions to display", 
                    choices = colnames(mtcars), #drive this attribute to a list or vector with question names to be displayed. 
                    multiple = TRUE
                  ))),
              fluidRow(
                box(width = 12, status = "primary", solidHeader = FALSE,
                    plotlyOutput("fig1_numeric", height = 250, width = 600)    
                    
                ))
              
              
      )
    )
  )
)
      
server <- function(input, output, session) {

  # renders all elements regarding the dashboard page
  prop_enrolled<-round(length(data$enrolled)/300*100,0)

  output$progressBox <- renderValueBox({
    valueBox(paste0(prop_enrolled, "%"), "% of total needed in Phase I", icon = icon("poll-h"),color = "purple")
  })
  output$daysElapsed <- renderValueBox({
    valueBox("35", "Days since start", icon = icon("calendar", lib = "glyphicon"),color = "light-blue")
  })
  # categories <- c(1,2,5,7,8,8,8,1,2,4)
  # output$plot1 <- renderPlot(hist(categories, main="Graphic example 1"))
  # output$plot2 <- renderPlot(hist(categories, main="Graphic example 2"))
  
  # renders all elements regarding the descriptive tables
  output$tables <- renderPrint(input$tables)
  output$descrip_table <- DT::renderDataTable({
    DT::datatable(mtcars[, input$tables, drop = FALSE])
  })
  
  # renders all elements regarding the categorical question graphics (third tab)
  output$cat_quest <- renderPrint(input$cat_quest)
  
  #dataset sample for the "fig1_catg" graphic
  # dat1 <- data.frame(
  #   sex = factor(c("Female","Female","Male","Male")),
  #   time = factor(c("Lunch","Dinner","Lunch","Dinner"), levels=c("Lunch","Dinner")),
  #   total_bill = c(13.53, 16.81, 16.24, 17.42)
  # )

  data_enrollment_date<-ddply(data , .(date) , summarise , Count = length(practid))

  data_enrollment_date2 <- data_enrollment_date %>%
  mutate(date = as.Date(date)) %>%
  complete(date = full_seq(date, period = 1), 
              fill = list(Count = 0))
  
  data_enrollment_date2<-subset(data_enrollment_date2, data_enrollment_date2$date >= start_date )


  data_enrollment_date2$rasters <- sprintf("Day[%d]",seq(1:length(data_enrollment_date2$date)))

  # fig <- plot_ly(x = ~data_enrollment_date2$date, 
  #          y = ~data_enrollment_date2$Count, 
  #          mode = 'lines', 
  #          text = paste(data_enrollment_date2$Count, "patients enrolled"))
  
  # Bar graph, time on x-axis, color fill grouped by sex -- use position_dodge()
  output$fig1_enrollment_day <- renderPlotly(
    figure1<-plot_ly(x = ~data_enrollment_date2$date, 
         y = ~data_enrollment_date2$Count, 
         type = 'bar', 
         text = paste(data_enrollment_date2$Count, 
              "patients enrolled")
                )%>% 
     layout(#title=paste0(" Weeks since PRACT start"),
                xaxis = list(title = "Days of the week",
                      # showgrid = FALSE, 
                      #tickangle = 270, 
                      #dtick = 6, 
                      #tickfont = list(size = 11),
                      ticktext = data_enrollment_date2$rasters,
                      tickvals = data_enrollment_date2$date#,
                      #tickmode = "array",
                      #tickangle = 270
                  ),
                yaxis = list(title = "# enrolled"#,
                      # showgrid = TRUE, 
                      #tickangle = 270, 
                      #dtick = 6, 
                      #tickfont = list(size = 11),
                      # ticktext = data_enrollment_date3$rasters,
                      # tickvals = data_enrollment_date3$week#,
                      #tickmode = "array",
                      #tickangle = 270
                  )
            )
      )

  data_enrollment_date3 <- data_enrollment_date %>% 
  group_by(week = format(as.Date(date), "%Y-%U"))%>% 
  summarise_if(is.numeric, sum)

  data_enrollment_date3$rasters <- sprintf("Week[%d]",seq(1:length(data_enrollment_date3$week)))

  # Bar graph, time on x-axis, color fill grouped by sex -- use position_dodge()
  output$fig2_enrollment_week <- renderPlotly(
    # p <- ggplot(data=dat1, aes(x=time, y=total_bill, fill=sex)) +
    # geom_bar(colour="black", stat="identity",
    #          position=position_dodge(),
    #          size=.3) +                        # Thinner lines
    # xlab("Time of day") + ylab("Total bill") + # Set axis labels
    # ggtitle("Average bill for 2 people") +     # Set title
    # theme_bw())
    figure2<-plot_ly(x = ~data_enrollment_date3$week, 
         y = ~data_enrollment_date3$Count, 
         type = 'bar',
         # mode= 'lines',
         text = paste(data_enrollment_date3$Count, 
              "patients enrolled")) %>% 
     layout(#title=paste0(" Weeks since PRACT start"),
                xaxis = list(title = "Weeks since PRACT start",
                      # showgrid = FALSE, 
                      #tickangle = 270, 
                      #dtick = 6, 
                      #tickfont = list(size = 11),
                      ticktext = data_enrollment_date3$rasters,
                      tickvals = data_enrollment_date3$week#,
                      #tickmode = "array",
                      #tickangle = 270
                  ),
                yaxis = list(title = "# enrolled"#,
                      # showgrid = TRUE, 
                      #tickangle = 270, 
                      #dtick = 6, 
                      #tickfont = list(size = 11),
                      # ticktext = data_enrollment_date3$rasters,
                      # tickvals = data_enrollment_date3$week#,
                      #tickmode = "array",
                      #tickangle = 270
                  )
            )
     )
  
  # renders all elements regarding the numeric question graphics (fourth tab)
  output$num_quest <- renderPrint(input$num_quest)
  
  #dataset sample for the "fig1_numeric" graphic
  test_data <-
    data.frame(
      var0 = 100 + c(0, cumsum(runif(49, -20, 20))),
      var1 = 150 + c(0, cumsum(runif(49, -10, 10))),
      date = seq(as.Date("2002-01-01"), by="1 month", length.out=100)
    )
  test_data_long <- melt(test_data, id="date")
  
  
  output$fig1_numeric <- renderPlotly(
    p <- ggplot(data=test_data_long,
                aes(x=date, y=value, colour=variable)) +
      xlab("Day since start") + ylab("Enrolled patients") + 
      geom_line()+
      theme_bw())
  
}

shinyApp(ui, server)
