#----------------------------------------------------------------------------------------------------------

# some libs
library(tidyverse)
library(usmap)
library(magrittr)
library(dplyr)
library(babynames)
library(shiny)
source("DATA_Prepare_Shiny.R")
source("vis.r")
#----------------------------------------------------------------------------------------------------------


# The most basic shiny skeleton.






ui <- fluidPage(

    
    #Navbar structure for UI
    navbarPage("Shiny Demonstration",
               tabPanel("Observation Distribution", fluid = TRUE, icon = icon("globe-americas"),
                        
                        selectInput(inputId = "YEARS", # This might be used in our map making where 3 years are for selection.
                                    label = "YEARS:",
                                    choices = list("2016" = "x2016", #these values need to be exactly how they show in the dataset.
                                                   "2018" = "x2018",
                                                   "2019" = "x2019")),
                        submitButton(text = "Create my plot!"),
                        plotOutput(outputId = "nameplot")
                        
                      
                        ),
               tabPanel("Percentage of High Bee Toxin in California",
                       
                        plotOutput(outputId = "nameplot2")
                        
                        )
               
               )
  )            
  
  
  
  
  
  
  
  
         
                
#----------------------------------------------------------------------------------------------------------


# 给state一个vector
state<-c("CALIFORNIA","FLORIDA","OREGON","WASHINGTON")


# df_shiny <- cbind(state, shiny_map$x2016)
# 


 
#----------------------------------------------------------------------------------------------------------



server <- function(input, output){
  

  output$nameplot <- renderPlot({


    
    plot_usmap(data = shi_func(input$YEARS), values = "observations", color = "red") +
    scale_fill_continuous(low = "white", high = "green", name = "number of observations", label = scales::comma) +
    theme(legend.position = "right")
 
  })
  
  output$nameplot2 <- renderPlot({
    
    
    ggplot(cali_chem_summary_ordered, aes(fill = `Bee Toxins`, y = percentage, x = Year)) + 
      geom_bar(position="stack", stat="identity")
    
    

  })
  
  }

shinyApp(ui = ui, server = server)
