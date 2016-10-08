# word cloud in shiny, check their input. Use a function to change the data to dat. 

library(shiny)
# Loading package for Kappa test

library(fmsb)
# Loading package for Cronbach's alpha
library(psy)

# Loadign concordance package
library(epiR)

# Loading Cramer's V package
library(lsr)

# Generating simulated data
set.seed(1234)
# Ordinal
ordin1<- sample(c(1,2,3,4), 20, TRUE)
ordin2 <- sample(c(5,7,8,9), 20, TRUE)
#Continuous/interval
conti1 <- c(10:29)
conti2 <- round(seq(1, 10, length.out= 20), 2)
# Categorical
categor1 <- rbinom(20,1,0.4)
categor2 <- rbinom(20,1,0.6)
dat <- cbind(ordin1, ordin2, conti1, conti2, categor1, categor2 )
dat <- as.data.frame(dat)

ui <- fluidPage(
  pageWithSidebar(
    headerPanel('Correlation coefficient and scatter plots'),
    sidebarPanel(
      
      selectInput('xcol', 'X Variable', names(dat)),
      selectInput('ycol', 'Y Variable', names(dat),
                  selected=names(dat)[[2]]),
      selectInput(inputId = "measure", label = "Choose the correlation measure that you want to use",
                  choices = c("Spearman correlation" = "spearman",
                              "Pearson correlation" = "pearson",
                              "Kendall's W" = "kendall",
                              "Cohen's kappa" = "kappa",
                              "Concordance correlation coefficient" = "concordance",
                              "Cramer's V" = "cramers", 
                              "Cronbach's alpa" = "cronbach" 
                                ))
    ),
    mainPanel(
      plotOutput('plot1'),
      verbatimTextOutput('stats')
    )
  )
)
server <- function(input, output){
  
  # Creating a function to combine cramer's V and chi-square test
  cram <- function(x) {
    print(cramersV(x))
    chisq.test(x)
  }
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    dat[, c(input$xcol, input$ycol)]
  })
  
  output$plot1 <- renderPlot({
    plot(selectedData(),pch = 20, cex = 3, col = "red")
  })
  
  output$stats <- renderPrint({
    measure <- input$measure
    mydat <- selectedData()
    if(measure == "kappa") {
      Kappa.test(mydat[,1], mydat[,2], conf.level = 0.95)
    } else if (measure == "cronbach"){
      cronbach(mydat[,])
    } else if (measure == "concordance" ) {
      epi.ccc(mydat[,1], mydat[,2], ci = "z-transform", conf.level = 0.95)
    } else if (measure == "cramers" ) {
      cram(mydat[,1:2])
    } else {
      round(cor(mydat, method = measure), 3)
    }
  })
}

shinyApp(ui = ui, server = server)


