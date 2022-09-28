#'  shiny app
#' 
#'  shinyapp
#'
#' @import ggplot2
#' @import ggmap
#' @import dplyr
#' @import  shiny 
#' @importFrom shinyjs useShinyjs show hide
#' @import  Lab5
#' @export 
#=================== DESING THE UI =======================

if (!requireNamespace("Lab5")) {
  stop("Please install Lab5 package first. Install using devtools::install_github(\"kiriakospapa/Lab-5\")")
}
if (!requireNamespace("shinyjs")) {
  stop("Please install shinyjs package first. Install using install.package(\"shinyjs\")")
}
  

ui <- shiny::fluidPage(
  
  shinyjs::useShinyjs(),
  shiny::selectInput( "option", label = "Select what you want to see",
               
               choices = list("Winds in Huston", "Crimes in huston by month and type", "Crimes in huston by month", "Crimes in huston by type"),
               selected = NULL, width = "20%"),
  
  
  Lab5::textInputRow(inputId="min_wind", label="Minimum of wind", value = 0, class="input-small"),
  Lab5::textInputRow(inputId="max_wind", label="Maximum of wind", value = 100),
  shiny::plotOutput(outputId="probs")
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
        shinyjs::show("max_wind")
        shiny::updateActionButton(session, "min_wind",
                           label = "Minumum of wind")
        shiny::updateActionButton(session, "max_wind",
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
          
          Lab5::wind_plot(as.integer(input$min_wind), as.integer(input$max_wind))
        }
      }
      else if(input$option == "Crimes in huston by month and type"){
        
        shinyjs::show("max_wind")
        shinyjs::show("min_wind")
        
        
        
        shiny::updateActionButton(session, "min_wind",label = "Month")
        
        shiny::updateActionButton(session, "max_wind",
                           label = "Type of crime")
        
        if(as.double(input$min_wind) %% 1 == 0)
        {
          
          plot( Lab5::find_crime_from_time_and_type(ggmap::crime, input$max_wind, as.integer(input$min_wind)))
        }
        else{
          
          stop("Month should be an integer from 1 to 8" )
        }
      }
      else if(input$option == "Crimes in huston by month"){
        
        shinyjs::hide("max_wind")
        shiny::updateActionButton(session, "max_wind",
                           label = "")
        shiny::updateActionButton(session, "min_wind",
                           label = "Month")
        if(as.double(input$min_wind) %% 1 == 0)
        {
          
          Lab5::find_crime_from_month(ggmap::crime, as.integer(input$min_wind))
        }
        else{
          
          stop("Month should be an integer from 1 to 8" )
        }
      }
      else if(input$option == "Crimes in huston by type"){
        shinyjs::hide("min_wind")
        shinyjs::show("max_wind")
        
        
        shiny::updateActionButton(session, "max_wind",
                           label = "Type of crime")
        shiny::updateActionButton(session, "min_wind",
                           label = "")
        Lab5::find_crime_from_type(ggmap::crime,input$max_wind)
        
      }
      
    })
  })
  
}

shiny::shinyApp(ui, server)