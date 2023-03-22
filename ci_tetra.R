library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(DT)
library(tidyverse)
library(statpsych)
library(markdown)
library(gridExtra)

ui <- fluidPage(

    # Application title
    titlePanel("Confidence Intervals for Tetrachoric Correlations"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
        
            h3("Number of participants"),
            DTOutput("my_datatable"),
            
            h3("Sample size"),
            sliderInput("n", "", min = 50, max = 5000, step = 10, value = c(100,2000)),
            
            h3("Alpha level"),
            numericInput("alpha", "", .05, min = .001, max = 1),
            verbatimTextOutput("value"),
    
            # horizontal line ----
            tags$hr(),
      
            actionButton("go",label = "Run simulation"),
            
            # horizontal line ----
            tags$hr(),
    
            # License
            HTML("luc.watrin[at]uni-ulm.de <br>
                 CC-By Attribution 4.0 International <br>
                 Code available at https://github.com/luc-w/ci-tetra")
    
  
        ),

        # Show plot
        mainPanel(
          
           h4("How to use this app"),
           includeMarkdown('main.md'),

           plotOutput("plot_results")
        
           )
    )
)



server <- function(input, output) {
    
    # initialize an empty dataframe
    df <- reactiveValues(data = { 
                         
                         data.frame(x = numeric(0),y = numeric(0)) %>% 
                         add_row(x = rep(0,2),y = rep(0,2))
    
      })

    # output the datatable based on the dataframe (and make it editable)
    output$my_datatable <- renderDT({
      
            datatable(df$data, 
                      colnames = c("0", "1"),
                      rownames = c("0", "1"),
                      options = list(dom = "t"),
                      editable = TRUE)
    
      })
    
    # write any edit to initial dataframe
    # automatically convert negative values to positive
    
    observeEvent(input$my_datatable_cell_edit, {
        
                # get values
                info = input$my_datatable_cell_edit
                i = as.numeric(info$row)
                j = as.numeric(info$col)
                k = as.numeric(info$value)
                
                if(k < 0){ 
                    k <- k * -1
                }
        
        # write values to reactive
        df$data[i,j] <- k
        
    })
    
    # render plot
    
    output$plot_results <- renderPlot({
      
        req(input$go) # plot only if input button is non-zero
        
        static_data <- isolate(df$data)
        alpha <- isolate(input$alpha)
        n_min <- isolate(input$n[1])
        n_max <- isolate(input$n[2])
        
        range <- NULL
        
        for (i in n_min:n_max) {
          
            cit <- ci.tetra(alpha = alpha, # number of participants with y = 0 and x = 0
                                           static_data[1,1]*i/sum(static_data), 
                                           # number of participants with y = 0 and x = 1
                                           static_data[1,2]*i/sum(static_data), 
                                           # number of participants with y = 1 and x = 0
                                           static_data[2,1]*i/sum(static_data), 
                                           # number of participants with y = 1 and x = 1
                                           static_data[2,2]*i/sum(static_data))
            range <- rbind(range, cit)
          
        }
        
        range <- range %>% 
                 data.frame() %>% 
                 mutate(x = (n_min:n_max)/sum(static_data),
                        ci_width = UL-LL) 
        
        g1 <- ggplot(range, aes(x = x, y = Estimate)) +
              geom_line() +
              geom_ribbon(aes(x, ymin = LL, ymax = UL), alpha = 0.4) +
              labs(y = "Tetrachoric Correlation")
        
        g2 <- ggplot(range, aes(x = x, y = ci_width)) +
              geom_line() + 
              labs(y = "Width of Confidence Inteval")
                      
        grid.arrange(g1, g2, nrow = 2)
        

    }, width = 500, height = 800)
    

}

# Run the application 
shinyApp(ui = ui, server = server)