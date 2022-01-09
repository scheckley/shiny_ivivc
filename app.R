library(shiny)
library(ggplot2)
library(Rivivc)
library(gridExtra)
library(showtext)
showtext_auto()

frank <- data("input")

ui <- fluidPage(
  titlePanel('IVIVC'),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose impulse CSV File", accept = ".csv"),
      fileInput("file2", "Choose response File", accept = ".csv"),
      fileInput("file3", "Choose input File", accept = ".csv")),
    mainPanel(
      plotOutput("plot")
    )
  )
)


server <- function(input, output){
  
  output$plot <- renderPlot({
  
  IV_dat = input$file1
  PK_dat = input$file2
  invitro_dat = input$file3
  
  if is.na(IV_dat) {}

  
  #preparing data matrices
  input_mtx<-as.matrix(IV_dat)
  impulse_mtx<-as.matrix(PK_dat)
  resp_mtx<-as.matrix(invitro_dat)
  #setting accuracy
  accur_explic <- 20
  accur_implic <- 5
  #run deconvolution
  result<-RivivcA(input_mtx,impulse_mtx,resp_mtx,explicit.interp=accur_explic,implicit.interp=accur_implic)
  summary(result$regression)
  print("Raw results of deconvolution")
  print(result$numeric$par)
  predicted<-predict(result$regression)
  deconvolved_data<-unname(predicted)
  orig_data<-input_mtx[, 2]
  
  p1 <- plot(orig_data,result$numeric$par[,2])
  }
  )
  
}

shinyApp(ui = ui, server = server)