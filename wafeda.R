library(shiny)
library(tidyverse)
library(data.table)
library(rlang)
library(shinythemes)
library(waiter)

not_sel = "Not Selected"

#main page
#----------------------------------------
main_page <- tabPanel(
  title = "Analysis",
  titlePanel("Analysis"),
  sidebarLayout(
    sidebarPanel(
      autoWaiter(html = spin_orbit()),
      title = "Inputs",
      fileInput("upload", NULL, accept = c(".csv", ".tsv")),
      numericInput("n","Number of rows", value = 5),
      selectInput("num_var_1","Variable 1(numeric)",choices=c(not_sel)),
      selectInput("num_var_2","Variable 2(numeric)",choices=c(not_sel)),
      selectInput("fact_var","Variable 3(factor)",choices=c(not_sel)),
      actionButton("run_button","Analyze",icon=icon("play"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Data",
          fluidRow(strong(textOutput("top_n_rows"))),
          fluidRow(tableOutput("head")),
          fluidRow(strong(textOutput("skim_table"))),
          fluidRow(tableOutput("data_summary"))
        ),
        tabPanel(
          title = "Plot",
          autoWaiter(),
          plotOutput("plot_1")
        ),
        tabPanel(
          title = "Statistics",
          fluidRow(
            column(width = 4, strong(textOutput("num_var_1_title"))),
            column(width = 4, strong(textOutput("num_var_2_title"))),
            column(width = 4, strong(textOutput("fact_var_title")))
          ),
          fluidRow(
            column(width = 4, tableOutput("num_var_1_summary_table")),
            column(width = 4, tableOutput("num_var_2_summary_table")),
            column(width = 4, tableOutput("fact_var_summary_table"))
          ),
          fluidRow(
            column(width = 12, strong("Combined Statistics"))
          ),
          fluidRow(
            column(width = 12, tableOutput("combined_summary_table"))
          )
        )
      )
    )
  )
)

#about page
#-----------------------------------
about_page <- tabPanel(
  title = "About", titlePanel("About"),
  "WAFEDA (Web App For Exploratory Data Analysis) is a shinyapp created to ease the process of
   performing EDA, which can be accessible on mobile and PC", br(),
  "Created with R Shiny by",
  fluidPage(uiOutput("tab")),
  "Source code on",
  fluidPage(uiOutput("tab2")),
  br(), "2022 March")

#function generating plots
#-------------------------------------------------------
draw_plot_1 <- function(data_input, num_var_1, num_var_2, fact_var){
  if(fact_var!=not_sel){#convert non factor columns to factor
    data_input[,(fact_var) := as.factor(data_input[,get(fact_var)])]
  }
  if(num_var_1 != not_sel & num_var_2 != not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_1, y = num_var_2, color = fact_var)) +
      geom_point()
  }
  else if(num_var_1 != not_sel & num_var_2 != not_sel & fact_var == not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_1, y = num_var_2)) +
      geom_point()
  }
  else if(num_var_1 != not_sel & num_var_2 == not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = fact_var, y = num_var_1)) +
      geom_violin()
  }
  else if(num_var_1 == not_sel & num_var_2 != not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = fact_var, y = num_var_2)) +
      geom_violin()
  }
  else if(num_var_1 != not_sel & num_var_2 == not_sel & fact_var == not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_1)) +
      geom_histogram()
  }
  else if(num_var_1 == not_sel & num_var_2 != not_sel & fact_var == not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_2)) +
      geom_histogram()
  }
  else if(num_var_1 == not_sel & num_var_2 == not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = fact_var)) +
      geom_bar()
  }
}

#function generating numeric variable statistics
#------------------------------------------------------
create_num_var_table <- function(data_input, num_var){
  if(num_var != not_sel){
    col <- data_input[,get(num_var)]
    if (length(col)>5000) col_norm <- sample(col,5000) else col_norm <- col
    norm_test <- shapiro.test(col_norm)
    statistic <- c("mean", "median", "5th percentile", "95th percentile",
                   "Shapiro statistic", "Shapiro p-value", "missing values")
    value <- c(round(mean(col),2), round(median(col),2),
               round(quantile(col, 0.05),2), round(quantile(col, 0.95),2),
               norm_test$statistic, norm_test$p.value, sum(is.na(col)))
    data.table(statistic, value)
  }
}

#function generating factor variable statistics
#-------------------------------------------------
create_fact_var_table <- function(data_input, fact_var){
  if(fact_var != not_sel){
    freq_tbl <- data_input[,.N, by = get(fact_var)]
    freq_tbl <- setnames(freq_tbl,c("factor_value", "count"))
    freq_tbl
  }
}

#function generating combined table statistics
#---------------------------------------------------
create_combined_table <- function(data_input, num_var_1, num_var_2, fact_var){
  if(fact_var != not_sel){
    if(num_var_1 != not_sel & num_var_2 != not_sel){
      res_tbl <- data_input[,.(correlation = cor(get(num_var_1), get(num_var_2))), by = fact_var]
    }
    else if(num_var_1 != not_sel & num_var_2 == not_sel){
      res_tbl <- data_input[,.(mean = mean(get(num_var_1))), by = fact_var]
    }
    else if(num_var_1 == not_sel & num_var_2 != not_sel){
      res_tbl <- data_input[,.(mean = mean(get(num_var_2))), by = fact_var]
    }
  }
  else if(num_var_1 != not_sel & num_var_2 != not_sel){
    res_tbl <- data.table(
      statistic = c("correlation"),
      value = c(cor(
        data_input[,get(num_var_1)],
        data_input[,get(num_var_2)])))
  }
  return(res_tbl)
}

#front end
#----------------------------------------
ui <- 
  navbarPage(
  title = "WAFEDA", 
  theme = shinytheme("superhero"),
  main_page, 
  about_page
  )

#back_end
#---------------------------------------------
server <- function(input, output, session) {
  
  #top n rows
  top_n <- eventReactive(input$n, paste("Top", input$n, "rows"))
  output$top_n_rows <- renderText(top_n())
  
  #Data Summary
  output$skim_table <- renderText("Data Summary")
  
  #summary of data
  summary_data <- reactive(skimr::skim(data()))
  
  output$data_summary <- renderTable(summary_data())
  
  #link to Adejumo Ridwan Suleiman
  url <- a("Adejumo Ridwan Suleiman", 
           href="https://www.adejumoridwan.com/about/about-me/",
           target = "_blank")
  output$tab <- renderUI({
    tagList(url)
  })
  
  #link to source code
  url2 <- a("Github", 
           href="https://github.com/adejumoridwan/miniEDA/blob/main/miniEDAapp.R",
           target = "_blank")
  output$tab2 <- renderUI({
    tagList(url2)
  })
  
  #uploaded data
  data <- reactive({
    req(input$upload)
    fread(input$upload$datapath)
  })
  
  #number of rows
  output$head <- renderTable({
    head(data(), input$n)
  })
  
  observeEvent(data(),{
    choices <- c(not_sel,names(data()))
    updateSelectInput(inputId = "num_var_1", choices = choices)
    updateSelectInput(inputId = "num_var_2", choices = choices)
    updateSelectInput(inputId = "fact_var", choices = choices)
  })
  
  num_var_1 <- eventReactive(input$run_button,input$num_var_1)
  num_var_2 <- eventReactive(input$run_button,input$num_var_2)
  fact_var <- eventReactive(input$run_button,input$fact_var)
  
  #plot
  plot_1 <- eventReactive(input$run_button,{
    draw_plot_1(data(), num_var_1(), num_var_2(), fact_var())
  })
  
  output$plot_1 <- renderPlot(plot_1())
  
  #numeric variable 1
  output$num_var_1_title <- 
    renderText(paste("Num Var 1:",num_var_1()))
  
  num_var_1_summary_table <- eventReactive(input$run_button,{
    create_num_var_table(data(), num_var_1())
  })
  
  output$num_var_1_summary_table <- 
    renderTable(num_var_1_summary_table(),colnames = FALSE)
  
  #numeric variable 2
  output$num_var_2_title <- 
    renderText(paste("Num Var 2:",num_var_2()))
  
  num_var_2_summary_table <- eventReactive(input$run_button,{
    create_num_var_table(data(), num_var_2())
  })
  
  output$num_var_2_summary_table <- 
    renderTable(num_var_2_summary_table(),colnames = FALSE)
  
  #factor variable
  output$fact_var_title <- 
    renderText(paste("Factor Var:",fact_var()))
  
  fact_var_summary_table <- eventReactive(input$run_button,{
    create_fact_var_table(data(), fact_var())
  })
  
  output$fact_var_summary_table <- 
    renderTable(fact_var_summary_table(), colnames = FALSE)
  
  #combined summary table
  combined_summary_table <- eventReactive(input$run_button,{
    create_combined_table(data(), num_var_1(), num_var_2(), fact_var())
  })
  
  output$combined_summary_table <- renderTable(combined_summary_table())
}

shinyApp(ui, server)