#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param workDir PARAM_DESCRIPTION
#' @param tag PARAM_DESCRIPTION
#' @param launch.browser PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @rdname qibble_app
#' @export 
#' @import shiny
qibble_app <- function(workDir,tag,launch.browser=TRUE){
  
  ### UI ----
  
  ui <- shiny::fluidPage(
    shiny::titlePanel("qibble Tracker"),
      shiny::column(3,
             shiny::sliderInput(inputId = 'timer',label = 'Set Polling Time',min = 1,max = 10,step = 1,value = 5),
             shiny::radioButtons(inputId = 'tunit',label = 'Time Interval',
                                 choices = split(c(1,2),factor(c('Seconds','Minutes'),
                                                               levels=c('Seconds','Minutes'))),
                                 selected = 1,inline = TRUE)
             ),
      shiny::column(9,shiny::plotOutput('plot',height = '600px'))
    )
  
  ### SERVER ----
  
  server <- function(input, output, session) {
    
    trackerData <- shiny::reactive({
      
      shiny::reactivePoll(input$timer*10^(3+as.numeric(input$tunit)), session,
                   # This function returns the time that the logfile was last
                   # modified
                   checkFunc = function() {
                     Sys.time()
                   },
                   # This function returns the content of the logfile
                   valueFunc = function() {
                     qibble(workDir = workDir,tag=tag)
                   }
      )})
    
    shiny::observeEvent(trackerData()(),{
      output$plot <- shiny::renderPlot({
        plot(trackerData()())
    })
    
})
  }
  
  shiny::shinyApp(ui = ui, server = server,options = list(launch.browser=launch.browser))
}
