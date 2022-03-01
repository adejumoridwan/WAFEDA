library(shiny)
library(tidyverse)




ui <- fluidPage(
  fileInput("upload", NULL, accept = c(".csv", ".tsv")),
  numericInput("n", "Rows", value = 5, min = 1, step = 1),
  tableOutput("head"),
  actionButton("summary", "Data Summary"),
  tableOutput("data_summary")
)

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
  
  summary_data <- eventReactive(
    input$summary,
    as.data.frame.matrix(summary(data()))
  )
  
  output$data_summary <- renderTable(summary_data())
}
shinyApp(ui, server)