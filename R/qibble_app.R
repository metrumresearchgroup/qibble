#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param workDir PARAM_DESCRIPTION
#' @param tag PARAM_DESCRIPTION
#' @param viewer PARAM_DESCRIPTION, Default: shiny::paneViewer()
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @rdname qibble_app
#' @export 
#' @import shiny
#' @import shinydashboard
qibble_app <- function(workDir,tag, viewer = shiny::paneViewer()){
  
  ### UI ----
  
  app <- shiny::shinyApp(
    ui = shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(title = "qibble Tracker"),
      shinydashboard::dashboardSidebar(
             shiny::sliderInput(inputId = 'timer',label = 'Set Polling Time',min = 1,max = 10,step = 1,value = 5),
             shiny::radioButtons(inputId = 'tunit',label = 'Time Interval',
                                 choices = split(c(1,2),factor(c('Seconds','Minutes'),
                                                               levels=c('Seconds','Minutes'))),
                                 selected = 1,inline = TRUE),
             shinydashboard::menuItem("Qstat", tabName = "qstat", icon = icon("th")),
             shinydashboard::menuItem("Plots", tabName = "plot", icon = icon("th")),
             shinydashboard::menuItem("Current Jobs", tabName = "jobs", icon = icon("th"))
             ),
      shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem(tabName = 'plot',
                          shiny::plotOutput('plot',height = '600px')
                          ),
          shinydashboard::tabItem(tabName = 'jobs',
                          shiny::dataTableOutput('jobs_tbl')
          ),
          shinydashboard::tabItem(tabName = 'qstat',
                          shiny::dataTableOutput('qstat_tbl')
          )
        )
      )
    ),
  
  ### SERVER ----
  
  server = function(input, output, session) {
    
    trackerData <- shiny::reactive({
      
      shiny::reactivePoll(input$timer*10^(3+as.numeric(input$tunit)),
                          session,
                          checkFunc = function() {
                             Sys.time()
                          },
                          valueFunc = function() {
                            qibble(workDir = workDir,tag=tag)
                          }
      )})
    
    shiny::observeEvent(trackerData()(),{
      
      output$plot <- shiny::renderPlot({
        plot(trackerData()())
      })
    
      output$jobs_tbl <- shiny::renderDataTable({
        current_jobs(trackerData()())
      })
      
      output$qstat_tbl <- shiny::renderDataTable({
        trackerData()()%>%select(group:slot_ratio_char)
      })
      
})
  })
  
  shiny::runGadget(app,viewer = viewer)
}
