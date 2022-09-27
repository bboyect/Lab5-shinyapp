#'  shiny app
#' 
#'  shinyapp
#'
#' @import ggplot2
#' @import ggmap
#' @import dplyr
#' @import  shiny 
#' @importFrom shinyjs useShinyjs show hide
#' @import Lab5 
#' @export 
#=================== DESING THE UI =======================


  
ui <- fluidPage(
  
  useShinyjs(),
  selectInput( "option", label = "Select what you want to see",
               
               choices = list("Winds in Huston", "Crimes in huston by month and type", "Crimes in huston by month", "Crimes in huston by type"),
               selected = NULL, width = "20%"),
  
  
  textInputRow(inputId="min_wind", label="Minimum of wind", value = 0, class="input-small"),
  textInputRow(inputId="max_wind", label="Maximum of wind", value = 100),
  plotOutput(outputId="probs")
)

#================= CREATING BACKEND ======================

server <- function(input, output, session) {
  result <- ""
  observe({  result <- input$option
  
  print(result)
  output$probs <- renderPlot(
    
    # Change plot depending on the option selected
    {
      if (input$option == "Winds in Huston"){
        show("max_wind")
        updateActionButton(session, "min_wind",
                           label = "Minumum of wind")
        updateActionButton(session, "max_wind",
                           label = "Maximum of wind")
        
        # updateNumericInput(session, "min_wind", value = "0")
        # updateNumericInput(session, "max_wind", value = "100")
        
        if(input$min_wind == ""| input$max_wind == ""){
          
          stop("Insert a value")
        }
        else if (is.na(as.integer(input$min_wind)) | is.na(as.integer(input$max_wind))){
          
          stop("Insert integer not charachters")
        }
        else{
          
          wind_plot(as.integer(input$min_wind), as.integer(input$max_wind))
        }
      }
      else if(input$option == "Crimes in huston by month and type"){
        
        show("max_wind")
        show("min_wind")
        
        
        
        updateActionButton(session, "min_wind",label = "Month")
        
        updateActionButton(session, "max_wind",
                           label = "Type of crime")
        
        if(as.double(input$min_wind) %% 1 == 0)
        {
          
          plot(find_crime_from_time_and_type(ggmap::crime, input$max_wind, as.integer(input$min_wind)))
        }
        else{
          
          stop("Month should be an integer from 1 to 8" )
        }
      }
      else if(input$option == "Crimes in huston by month"){
        
        hide("max_wind")
        updateActionButton(session, "max_wind",
                           label = "")
        updateActionButton(session, "min_wind",
                           label = "Month")
        if(as.double(input$min_wind) %% 1 == 0)
        {
          
          find_crime_from_month(ggmap::crime, as.integer(input$min_wind))
        }
        else{
          
          stop("Month should be an integer from 1 to 8" )
        }
      }
      else if(input$option == "Crimes in huston by type"){
        shinyjs::hide("min_wind")
        shinyjs::show("max_wind")
        
        
        updateActionButton(session, "max_wind",
                           label = "Type of crime")
        updateActionButton(session, "min_wind",
                           label = "")
        find_crime_from_type(ggmap::crime,input$max_wind)
        
      }
      
    })
  })
  
}

shinyApp(ui, server)