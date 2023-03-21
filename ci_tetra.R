library(shiny)
library(DT)
library(tidyverse)

ui <- fluidPage(

    # Application title
    titlePanel("CI Tetra"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
          
            DTOutput("my_datatable"),
            
            numericInput("alpha", "Alpha level for 1-alpha confidence:", .05, min = .001, max = 1),
            verbatimTextOutput("value"),
  
            actionButton("go",label = "Run simulation")
  
        ),

        # Show plot
        mainPanel(
          
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
      
        DT::datatable(df$data, 
                      colnames = c("0", "1"),
                      rownames = c("0", "1"),
                      options = list(dom = "t"),
                      editable = TRUE
                      #editable = list(target = "column", disable = list(columns=0))
                      )
    
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
        
        alpha <- isolate(input$alpha)
        static_data <- isolate(df$data)
        
        range <- NULL
        
        for (i in 100:2000) {
          
          cit <- ci.tetra(alpha = alpha, static_data[1,1]*i/135, 
                                         static_data[1,2]*i/135, 
                                         static_data[2,1]*i/135, 
                                         static_data[2,2]*i/135)
          range <- rbind(range, cit)
          
        }
        
        range %>% 
        data.frame() %>% 
        mutate(x = (100:2000)/135) %>% 
        ggplot(aes(x = x, y = Estimate)) +
        geom_line() + 
        geom_ribbon(aes(x, ymin = LL, ymax = UL), alpha = 0.4) +
        scale_y_continuous(breaks = seq(-1, 1, .05)) +
        labs(y = "Tetrachoric Correlation")

    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)