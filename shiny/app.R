#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## this is the function to compute the species–area relationship (SAR)
# c and Z values from https://onlinelibrary.wiley.com/doi/10.1111/jbi.12874
# c <- mean(c(2.717, 12.286))
# z <-  0.25
# As <- 1:A
S <- function(A, c=7.5, z=0.25){
  c*A^z
} 


library(shiny)

# Define UI for application
ui <- fluidPage(
  ### HTML to add text box
  tags$head(
    tags$style(HTML("
            code {
                display:block;
                padding:9.5px;
                margin:0 0 10px;
                margin-top:10px;
                font-size:13px;
                line-height:20px;
                word-break:break-all;
                word-wrap:break-word;
                white-space:pre-wrap;
                background-color:#F5F5F5;
                border:1px solid rgba(0,0,0,0.15);
                border-radius:4px; 
                font-family:monospace;
            }"))),
  
  # Application title
  titlePanel("Relação espécie–área (SAR)"),
  # https://stackoverflow.com/a/65588061
  p("Veja como a relação espécie-área muda com a mudança no tamanho da", strong("área"),
    "e nos valores dos parâmetros ", strong(em("z")), "e", strong(em("c."))),
  p("Na natureza, valores de", strong(em("z")), "variam de 0,1 to 0,35, enquanto valores de",
    strong(em("c")), "variam de 2,7 a 12,3, dependendo da diversidade do grupo
        (insetos tem valores de", strong(em("c")), "muito maiores do que aves)."),
      
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("area1", "Area 1", 10, 10000, 1000),
      sliderInput("area2", "Area 2", 10, 10000, 100),
      sliderInput("c2", "Param c", 2.7, 12.3, 7.5),
      sliderInput("z2", "Param z", 0.14, 0.31, 0.25),
    ),
    mainPanel(
      ## plot
      plotOutput("sar"),
    )
  )
)

# Define server logic required to draw SAR plots
server <- function(input, output, session) {
  
  As <- reactive({
    sapply(c(input$area1, input$area2), 
           function(x){
             seq(1, x, length.out=100)
           })
  })
  Ss <- reactive({
    matrix(c(
      S(As()[,1]),
      S(As()[,2], input$c2, input$z2)),
      ncol=2)
  })
  
  output$sar <- renderPlot({
    par(mar=c(3, 3, 2, 3))
    
    # main log plot
    par(fig = c(0,1,0,1))
    plot(log(As()[,1]), log(Ss()[,1]), type="l", lwd=2,
         xlim=range(log(As())), ylim=range(log(Ss())),
         xlab="", ylab="" )
    title(xlab = "log(Area)", line = 2)            # Add x-axis text
    title(ylab = "log(S)", line = 2)            # Add y-axis text
    lines(log(As()[,2]), log(Ss()[,2]), lty=3, col="red", lwd=5)
    
    # inset plot
    par(fig = c(0.045,0.5, 0.61, 0.95), new = T)
    rect(par("usr")[1], par("usr")[3],
         par("usr")[2], par("usr")[4],
         col = "#f7f7f7", border = "#f7f7f7") # Color
    par(fig = c(0.075,0.5, 0.65, 0.98), new = T)
    plot(As()[,1], Ss()[,1], type="l", lwd=2, 
         xlim=range(As()), ylim=range(Ss()),
         xlab="", ylab="" )
  title(xlab = "Area", line = 2)            # Add x-axis text
  title(ylab = "S (riqueza)", line = 2)            # Add y-axis text
    lines(As()[,2], Ss()[,2], lty=3, col="red", lwd=5)
  })
}

# Run the application 
shinyApp(ui, server)