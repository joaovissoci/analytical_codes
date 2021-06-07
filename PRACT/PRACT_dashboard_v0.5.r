library(shiny) # load shiny structure
library(shinydashboard) # load shiny dashboard - tutorial (http://rstudio.github.io/shinydashboard/structure.html#boxes)
library(shinyWidgets) # load shiny widgets - tutorial (http://shinyapps.dreamrs.fr/shinyWidgets/)
library(ggplot2)  # for the diamonds dataset
library(DT) #load dynamic tables  - tutorial (https://rstudio.github.io/DT/server.html)
library(plotly) #load dynamic graphics  - tutorial (https://plotly.com/ggplot2/#basic-charts)
library(reshape2) #load something I need and don`t know why. 
library(tidyverse)
library(lubridate)
library(plyr)
library(visdat) #predefined missing plots
library(naniar) #predefined missing plots
library(UpSetR) #creates the object that allow the missing combination analysis
library(gtsummary) # creates the cross tabulations to display distribution of data
library(gt) # creates the cross tabulations to display distribution of data
library(redcapAPI)

#bootstrap skins (https://bootswatch.com/3/cosmo/) 
# icon sources (https://fontawesome.com/icons?d=gallery&q=result)
#glyphicons source (https://getbootstrap.com/docs/3.3/components/)

#Tutorial: https://github.com/nutterb/redcapAPI/wiki/Finding-your-REDCap-API-URL
rcon <- redcapConnection(url='https://redcap.kcri.it/api/', token='F5E848AF96D0B5A6658C66260FDC8CC4')
data <- exportRecords(rcon)

reg_data <- read.csv("/Users/joaovissoci/Downloads/TraumaRegistryKCMC20_DATA_2020-12-01_2115.csv")

#data<-read.csv("/Users/joaovissoci/Downloads/DUKEPRACTR01Trial_DATA_2020-11-24_2003.csv")
start_date<-floor_date(today(), "week", 0) # if Monday the current day

reg_data_date_subset <- subset(reg_data, reg_data$date >= start_date)

data_date_subset <- subset(data, data$date >= start_date)

screened<-subset(data,data$enrolled== 'Yes')
screened_week<-subset(data_date_subset,data_date_subset$enrolled== 'Yes')

#data frame object over the missing functions will run
  
  data_useful<-with(data,data.frame(
        date,
        age=as.numeric(data$age),
        female=as.factor(data$female),
        edu_years=as.numeric(data$female),
        drinkb4inj=as.factor(data$female),
        etoh_breath_pos=as.factor(data$female),
        edu=as.numeric(data$female),
        rel=as.factor(data$female),
        employ_yn=as.factor(data$female),
        tribe=as.factor(data$female),
        tribe_not_afr=as.factor(data$female),
        household=as.numeric(data$female),
        income_self=as.numeric(data$female),
        bacpositive=as.factor(data$female),
        auditover8=as.factor(data$female),
        repalc=as.factor(data$female)))

  # data_useful$age<-as.numeric(data_useful$age)
  # data_useful$female<-as.factor(data_useful$female)

  data_useful$NoSelection<-c("No Selection")  

  data_useful_week<-subset(data_useful, data_useful$date >= start_date)

  # Function to create de missing values % ranking. Estimate the % of missing values within a variable
  calc_miss <- function(x){
    round((sum(is.na(x))/dim(data_useful)[1])*100,0)
  }
  
  # Apply the missing values estimate function to all columns of a target dataset and returns a named vector
  missing_rank_week <- sort(sapply(data_useful_week, calc_miss))
  high_missing_week <- missing_rank_week[(length(missing_rank_week)-9):length(missing_rank_week)]
  
  # Create te missing values rank data frame to plot
  plot1_data_week <- data.frame(high_missing_week, names=names(high_missing_week))

  # Apply the missing values estimate function to all columns of a target dataset and returns a named vector
  missing_rank <- sort(sapply(data_useful, calc_miss))
  high_missing <- missing_rank[(length(missing_rank)-9):length(missing_rank)]
  
  # Create te missing values rank data frame to plot
  plot2_data <- data.frame(high_missing, names=names(high_missing))

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
      # menuItem("Charts", icon = icon("bar-chart-o"),
      #         menuSubItem("Categorical questions", tabName = "cat_question"),
      #         menuSubItem("Numeric questions", tabName = "num_question")
      #),
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
                valueBox(length(reg_data$part_id), "Total patients in the registry", icon = icon("user-injured"), width = 3),# you need to check the data variable for this metric                
                valueBox(length(data$practid), "Total patients screened", icon = icon("user-injured"), width = 3),# you need to check the data variable for this metric
                valueBox(length(screened$practid), "Total patients  randomized", icon = icon("hospital-user"), width = 3),# you need to check the data variable for this metric
                # valueBox(length(data$enrolled), "Total patients discharged", icon = icon("procedures"), width = 3), # you need to check the data variable for this metric
                valueBoxOutput("progressBox", width = 3)
                #valueBoxOutput("daysElapsed")
              ),
              fluidRow(
                box(
                  title = "Patients enrolled in the registry this week", width = 4, solidHeader = TRUE, status = "info", length(reg_data_date_subset$part_id)
                ), #primary, success, info, warning, danger.
                box(
                  title = "Patients screened this week", width = 4, solidHeader = TRUE, status = "success", length(data_date_subset$practid)
                ), #primary, success, info, warning, danger.
                box(
                  title = "Patients randomized this week", width = 4, solidHeader = TRUE, status = "warning", length(screened_week$enrolled)
                )
                #box(
                #  title = "High level of missings", width = 4, solidHeader = TRUE, status = "danger", "Number of questions with more than 70% missing"
              ),
              fluidRow(
                box(
                  title = "Enrollment per Day/Week", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("fig1_enrollment_day", height = 250)
                  # )
                ),
              # fluidRow(                
                box(
                  title = "Enrollment per Week", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("fig2_enrollment_week", height = 250)
                   )
                ),
              fluidRow(
                box(
                  title = "Missing values ranking/week", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("plot1", height = 400)
                ),
                box(
                  title = "Missing values overall", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("plot2", height = 400)
                )
              ),
              fluidRow(
                box(
                  title = "High level of missings", width = 12, solidHeader = TRUE, status = "danger", paste(plot1_data_week$names,collapse="\n -")
                )
              )#,
              #  fluidRow(
              #   box(
              #     title = "Patterns of  missing values ranking/week", status = "primary", solidHeader = TRUE,
              #     collapsible = TRUE,
              #     plotlyOutput("plot3", height = 400)
              #   ),
              #   box(
              #     title = "Pattern of missing values overall", status = "primary", solidHeader = TRUE,
              #     collapsible = TRUE,
              #     plotlyOutput("plot4", height = 400)
              #   )
              # )  
            ), 
      
      # Second tab content
      tabItem(tabName = "tables",
              fluidRow(
                box(
                title = "Descriptive data", width = 12, status = "primary", solidHeader = TRUE,
                pickerInput(
                  inputId = "by_var",
                  selected = "NoSelection",
                  label = "Select multiple questions to display", 
                  choices = colnames(data_useful), #drive this attribute to a list or vector with question names to be displayed. 
                  multiple = FALSE
                ))),
              fluidRow(
                box(
                  title = "Descriptive distribution", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("plot5", height = 400)
                ),
              box(
                  title = "Missing values distribution", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("plot6", height = 400)
                )
              ),
              fluidRow(
               pickerInput(
                  inputId = "tbl_rows",
                  selected = "NoSelection", #direct this object to the PRACT data frame
                  label = "Select the second question to display", 
                  choices = colnames(data_useful), #drive this attribute to a list or vector with question names to be displayed. 
                  multiple = TRUE
                )),
              fluidRow(
                box(width = 12, status = "primary", solidHeader = FALSE,
                    gt::gt_output("descrip_table")    
                    ))
      
            )#,
      
      # thrid tab content
      # tabItem(tabName = "cat_question",
      #         fluidRow(
      #           box(
      #             title = "Categorical questions descriptive data", width = 12, status = "primary", solidHeader = TRUE,
      #             pickerInput(
      #               inputId = "cat_quest",
      #               selected = colnames(data_useful)[1],
      #               label = "Select multiple questions to display", 
      #               choices = colnames(mtcars), #drive this attribute to a list or vector with question names to be displayed. 
      #               multiple = TRUE
      #             ))),
      #         fluidRow(
      #           box(width = 12, status = "primary", solidHeader = FALSE,
      #               plotlyOutput("fig1_catg", height = 250, width = 600)))
      # ),
      
      # # fourth tab content
      # tabItem(tabName = "num_question",
      #         fluidRow(
      #           box(
      #             title = "Numerical questions descriptive data", width = 12, status = "primary", solidHeader = TRUE,
      #             pickerInput(
      #               inputId = "num_quest",
      #               selected = colnames(mtcars)[1],
      #               label = "Select multiple questions to display", 
      #               choices = colnames(mtcars), #drive this attribute to a list or vector with question names to be displayed. 
      #               multiple = TRUE
      #             ))),
      #         fluidRow(
      #           box(width = 12, status = "primary", solidHeader = FALSE,
      #               plotlyOutput("fig1_numeric", height = 250, width = 600)    
                    
      #           ))       
      #         ) 
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

  if (dim(data_enrollment_date2)[1]==0){ 

  data_enrollment_date2<-NULL
  data_enrollment_date2$rasters <- c("No enrollment to date")
  data_enrollment_date2$date <- start_date
  data_enrollment_date2$Count <- c(0)
  data_enrollment_date2<-as.data.frame(data_enrollment_date2)

  } else {

  data_enrollment_date2$rasters <- sprintf("Day[%d]",seq(1:length(data_enrollment_date2$date)))

  }

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

   if (dim(data_enrollment_date3)[1]==0){ 

  data_enrollment_date3<-NULL
  data_enrollment_date3$rasters <- c("No enrollment to date")
  data_enrollment_date3$date <- start_date
  data_enrollment_date3$Count <- c(0)
  data_enrollment_date3<-as.data.frame(data_enrollment_date3)

  } else {

  data_enrollment_date3$rasters <- sprintf("Week[%d]",seq(1:length(data_enrollment_date3$week)))

  }

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

    output$plot2 <- renderPlotly(  
    (pt <- ggplot(data=categories, aes(x=time, y=patients, fill=situation)) +
      geom_bar(stat="identity", position=position_dodge(), show.legend = FALSE) +
      scale_fill_manual(values=c("#3C8DBC", "#605CA8", "#00C0EF"))+
      theme_bw()))

  #data frame object over the missing functions will run
  
  # data_useful<-with(data,data.frame(
  #       date,
  #       age,
  #       female,
  #       dateinjury,
  #       drinkb4inj,
  #       etoh_breath_pos,
  #       edu,
  #       rel,
  #       employ_yn,
  #       tribe,
  #       household,
  #       income_self,
  #       audit1,
  #       audit2,
  #       audit3,
  #       audit4,
  #       audit5,
  #       audit6,
  #       audit7,
  #       audit8,
  #       audit9,
  #       audit10))

  # data_useful_week<-subset(data_useful, data_useful$date >= start_date)

  # # Function to create de missing values % ranking. Estimate the % of missing values within a variable
  # calc_miss <- function(x){
  #   round((sum(is.na(x))/dim(data_useful)[1])*100,0)
  # }
  
  # # Apply the missing values estimate function to all columns of a target dataset and returns a named vector
  # missing_rank_week <- sort(sapply(data_useful_week, calc_miss))
  # high_missing_week <- missing_rank_week[(length(missing_rank_week)-9):length(missing_rank_week)]
  
  # # Create te missing values rank data frame to plot
  # plot1_data_week <- data.frame(high_missing_week, names=names(high_missing_week))
  
  # Plot 1 object
  output$plot1 <- renderPlotly(
 p <- ggplot(data=plot1_data_week, aes(x=reorder(names, high_missing_week), y=high_missing_week)) +
    geom_bar(stat="identity", fill="darkblue") +
    ylab("% of missing values") +
    xlab("Top 10 questions")+
    # geom_text(aes(label=high_missing), position = position_dodge(width = 1), vjust=0.25, hjust= -0.25, size=3)+
    coord_flip()+
    theme_bw()
    )

  # # Apply the missing values estimate function to all columns of a target dataset and returns a named vector
  # missing_rank <- sort(sapply(data_useful, calc_miss))
  # high_missing <- missing_rank[(length(missing_rank)-9):length(missing_rank)]
  
  # # Create te missing values rank data frame to plot
  # plot2_data <- data.frame(high_missing, names=names(high_missing))
  
  # Plot 1 object
  output$plot2 <- renderPlotly(
 p <- ggplot(data=plot2_data, aes(x=reorder(names, high_missing), y=high_missing)) +
    geom_bar(stat="identity", fill="darkblue") +
    ylab("% of missing values") +
    xlab("Top 10 questions")+
    # geom_text(aes(label=high_missing), position = position_dodge(width = 1), vjust=0.25, hjust= -0.25, size=3)+
    coord_flip()+
    theme_bw()
    )

# output$plot3 <- renderPlot(
#     gg_miss_upset(data_useful_week, 
#                   nintersects = 5,
#                   # nsets = 10,
#                   main.bar.color = "darkblue",
#                   mainbar.y.label = "Number of missing intersection between questions", 
#                   sets.bar.color = "darkblue", 
#                   sets.x.label = "Observations with missings")
#   )

# output$plot4 <- renderPlot(
#     gg_miss_upset(data_useful, 
#                   nintersects = 5,
#                   # nsets = 10,
#                   main.bar.color = "darkblue",
#                   mainbar.y.label = "Number of missing intersection between questions", 
#                   sets.bar.color = "darkblue", 
#                   sets.x.label = "Observations with missings")
#   )
  
#############################################
# TAB 2
#############################################

  #Determining the correct plot to use

   #  x <- reactive(input$tables)

   #  if (is.numeric(x()) == TRUE) {
     
   #   output$plot5 <- renderPlotly(

   #      p<-ggplot(data_useful, aes(y=data_useful[,x()])) + 
   #  geom_boxplot() +
   #    theme_bw()
   #    )

   #  } else {
    
   #  output$plot5 <- renderPlotly(

   # p<-ggplot(data=data_useful, aes(x=as.factor(data_useful[,x()]))) +
   #     geom_bar(stat="count", width=0.7, fill="steelblue") +
   #       theme_bw()
   #       )

   #  }

  x <- reactive(input$by_var)

  output$plot5 <- renderPlotly(
  
  if (is.factor(data_useful[,x()])==TRUE) {

  p<-ggplot(data=data_useful, aes(x=data_useful[,x()])) +
    geom_bar(stat="count", width=0.7, fill="steelblue") +
      theme_bw()
  }
   else {
  
  p<-ggplot(data_useful, aes(y=data_useful[,x()])) + 
    geom_boxplot() +
      theme_bw()
  }   

  )

  output$plot6 <- renderPlotly({
  
  input_na<-mutate(data_useful, 
                      var_na= case_when(
                        is.na(data_useful[,x()]) ~ 'Missing',
                        TRUE ~ 'Valid'
              ))

  p<-ggplot(data=input_na, aes(x=as.factor(var_na))) +
  geom_bar(stat="count", width=0.7, fill="steelblue") +
    theme_bw()

  })

  # renders all elements regarding the numeric question graphics (fourth tab)
  # output$num_quest <- renderPrint(input$num_quest)
  
  # #dataset sample for the "fig1_numeric" graphic
  # test_data <-
  #   data.frame(
  #     var0 = 100 + c(0, cumsum(runif(49, -20, 20))),
  #     var1 = 150 + c(0, cumsum(runif(49, -10, 10))),
  #     date = seq(as.Date("2002-01-01"), by="1 month", length.out=100)
  #   )
  # test_data_long <- melt(test_data, id="date")
  
  
  # output$fig1_numeric <- renderPlotly(
  #   p <- ggplot(data=test_data_long,
  #               aes(x=date, y=value, colour=variable)) +
  #     xlab("Day since start") + ylab("Enrolled patients") + 
  #     geom_line()+
  #     theme_bw())

 tbl_row_list<-reactive(input$tbl_rows)

 output$descrip_table <- gt::render_gt({

  tbl_data <- data_useful %>% select(tbl_row_list())
  
  tbl_data$by_var<-data_useful[,x()]

  tab_2_display <- tbl_summary(tbl_data, 
                               by = by_var, #variable 1
                               # col = female, #variable 2
                               percent = "row",  # if you want to display % values options are c("none", "column", "row", "cell")
                               missing = "always", # if you want to display missing values, options are c("ifany", "always", "no")
                               missing_text = "Missings"#, # The text to display regarding the missing values colunm or row
                               ) %>% as_gt() # The text to display regarding the total value
  })
  
}

shinyApp(ui, server)
