#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(BatchGetSymbols)
library(dplyr)
library(ggplot2)
library(scales)
library(shinythemes)
library(ggpmisc)
acoes <- BatchGetSymbols::GetIbovStocks()$tickers
acoes <- paste0(acoes, ".SA")

# Define UI for application that draws a histogram
ui <- fluidPage(
 

    # Application title
    titlePanel("Beta"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("input_acoes",
                        label = "Selecione a Ação",
                        choices = acoes
                        ),
            sliderInput("dias", 
                        "Número de dias",
                        min = 30,
                        max = 365*2,
                        value = 365)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("out")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    bench <- "^BVSP"
     
    output$out <- renderPlot({
        
        
        ibov30 <- BatchGetSymbols("^BVSP",
                                  first.date = Sys.Date()-input$dias ,
                                  bench.ticker = bench)$df.tickers %>% 
            select(ref.date,ret.adjusted.prices )
    
       
        
        ticker30 <- BatchGetSymbols(input$input_acoes,
                                    first.date = Sys.Date()-input$dias,
                                    bench.ticker = bench)$df.tickers %>% 
            select(ref.date,ret.adjusted.prices )
        
        df30 <- left_join(ticker30, ibov30, by = "ref.date") 
        df30 <- df30[complete.cases(df30),]
        
        beta30 <- cov(df30$ret.adjusted.prices.x, df30$ret.adjusted.prices.y)/
            var(df30$ret.adjusted.prices.y)
        
        regressao <- lm(ret.adjusted.prices.x ~ ret.adjusted.prices.y, data = df30)

        df30 %>% 
            ggplot(aes(ret.adjusted.prices.y, ret.adjusted.prices.x )) +
            geom_point()+
            labs(title = paste("Beta =", round(beta30,4)),
                 y = paste("Retorno", input$input_acoes),
                 x = paste("Retorno IBOV"),
                 subtitle = paste("R2=", round(summary(regressao)$r.squared,4)))+
            geom_smooth(method = "lm", se = FALSE)+
            scale_y_continuous(labels = percent_format(),
                               limits = symmetric_limits)+
            scale_x_continuous(labels = percent_format(),
                               limits = symmetric_limits)+
            theme_minimal()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
