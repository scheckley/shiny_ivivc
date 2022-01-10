library(shiny)
library(ggplot2)
library(Rivivc)
library(gridExtra)
library(showtext)
showtext_auto()

ui <- fluidPage(
  titlePanel('IVIVC level A'),
  
  sidebarLayout(
    sidebarPanel(
      req(fileInput("IV_file", "Choose IV profile", accept = ".csv")),
      req(fileInput("PK_file", "Choose PK profile", accept = ".csv")),
      req(fileInput("invitro_file", "Choose in vitro profile", accept = ".csv")),
      numericInput("accur_explic", "explicit accuracy", 20, min = 1, max = 50, step = 1),
      numericInput("accur_implic", "implicit accuracy", 5, min = 1, max = 20, step = 1),
      downloadButton("downloadData", "Download Results")),
    mainPanel(
      plotOutput("plot"),
      tabPanel('Introduction',
               p("This app uses the Rivivc R package. Example data from the package is available ", a("here (zip).",href="https://github.com/scheckley/shiny_ivivc/raw/master/example_data.zip"), 
               br(),
               p("Documentation for Rivivc is available ", a("here (pdf).", href="https://cran.r-project.org/web/packages/Rivivc/Rivivc.pdf"))))
                 
    )
  )
)


server <- function(input, output){
  
  output$plot <- renderPlot({
    
  validate(
      need(input$IV_file != "", "No IV data has been uploaded."),
      need(input$PK_file != "", "No PK data has been uploaded."),
      need(input$invitro_file != "", "No in vitro data has been uploaded.")
  )
  
  IV_dat <- read.csv(input$IV_file$datapath)
  PK_dat <- read.csv(input$PK_file$datapath)
  invitro_dat <- read.csv(input$invitro_file$datapath)

  if (is.null(IV_dat)) {return(NULL)}
  if (is.null(PK_dat)) {return(NULL)}
  if (is.null(invitro_dat)) {return(NULL)}
  
  #preparing data matrices
  input_mtx<-as.matrix(invitro_dat)
  impulse_mtx<-as.matrix(IV_dat)
  resp_mtx<-as.matrix(PK_dat)
  
  #setting accuracy
  accur_explic <- input$accur_explic
  accur_implic <- input$accur_implic
  
  #run de-convolution
  result<-RivivcA(input_mtx,impulse_mtx,resp_mtx,explicit.interp=accur_explic,implicit.interp=accur_implic)
  
  predicted<-predict(result$regression)
  deconvolved_data<-unname(predicted)
  orig_data<-input_mtx[,2]
  
  p1 = qplot(orig_data,result$numeric$par[,2], xlab='in vitro data', ylab='deconvolved data', main='IVIVC correlation') +
    geom_line(aes(orig_data,deconvolved_data, col='red')) +
    guides(color = FALSE)
  
  p2 = qplot(input_mtx[,1], input_mtx[,2], xlab='time', ylab='fraction absorbed', main='deconvolution (points = data, red = computed deconvolution profile)') + 
    geom_line(aes(result$numeric$par[,1], result$numeric$par[,2]), col='red')

  grid.arrange(p1,p2, ncol=2)
  
  # Downloadable csv
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(result$numeric$par, file, row.names = FALSE)
    }
  )
  
  }
  )
  
}

shinyApp(ui = ui, server = server)