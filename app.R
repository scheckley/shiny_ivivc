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
      req(fileInput("IV_file", "Choose IV profile", accept = ".csv")),
      req(fileInput("PK_file", "Choose PK profile", accept = ".csv")),
      req(fileInput("invitro_file", "Choose in vitro profile", accept = ".csv"))),
    mainPanel(
      plotOutput("plot")
    )
  )
)


server <- function(input, output){
  
  output$plot <- renderPlot({
  
  IV_dat <- read.csv(input$IV_file$datapath)
  PK_dat <- read.csv(input$PK_file$datapath)
  invitro_dat <- read.csv(input$invitro_file$datapath)

  if (is.null(IV_dat)) {return(NULL)}
  if (is.null(PK_dat)) {return(NULL)}
  if (is.null(invitro_dat)) {return(NULL)}
  

  #debug
  #IV_dat = read.csv('impulse.csv')
  #PK_dat = read.csv('response.csv')
  #invitro_dat = read.csv('input.csv')
  
  #preparing data matrices
  input_mtx<-as.matrix(invitro_dat)
  impulse_mtx<-as.matrix(IV_dat)
  resp_mtx<-as.matrix(PK_dat)
  
  #setting accuracy
  accur_explic<-20
  accur_implic<-5
  
  #run deconvolution
  result<-RivivcA(input_mtx,impulse_mtx,resp_mtx,explicit.interp=accur_explic,implicit.interp=accur_implic)
  
  predicted<-predict(result$regression)
  deconvolved_data<-unname(predicted)
  orig_data<-input_mtx[,2]
  

  
  p1 = qplot(orig_data,result$numeric$par[,2], xlab='original data', ylab='fitted data', main='IVIVC correlation') +
    geom_line(aes(orig_data,deconvolved_data, col='red')) +
    guides(color = FALSE)
  
  p2 = qplot(input_mtx[,1], input_mtx[,2], xlab='time', ylab='fraction absorbed', main='deconvolution plot (red = computed deconvolution profile)') + 
    geom_line(aes(result$numeric$par[,1], result$numeric$par[,2]), col='red')

  grid.arrange(p1,p2, ncol=2)
  }
  )
  
}

shinyApp(ui = ui, server = server)