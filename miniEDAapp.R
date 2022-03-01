library(shiny)

not_sel = "Not Selected"

main_page <- tabPanel(
  title = "Analysis",
  titlePanel("Analysis"),
  sidebarLayout(
    sidebarPanel(
      title = "Inputs",
      fileInput("upload", NULL, accept = c(".csv", ".tsv")),
      selectInput("num_var_1","Variable 1(numeric)",choices=c(not_sel)),
      selectInput("num_var_2","Variable 2(numeric)",choices=c(not_sel)),
      selectInput("fact_var","Variable 3(factor)",choices=c(not_sel)),
      actionButton("run_button","Run Analysis",icon=icon("play"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Plot"
        ),
        tabPanel(
          title = "Statistics",
        )
      )
    )
  )
)

about_page <- tabPanel(
  title = "About", titlePanel("About"),
  "Created with R Shiny", br(), "2022 March")

ui <- 
  navbarPage(
  title = "mini EDA", 
  main_page, 
  about_page)

server <- function(input, output, session) {
  data <- reactive({
    req(input$upload)
    
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })
  
  output$head <- renderTable({
    head(data(), input$n)
  })
}

shinyApp(ui, server)