# app/view/CalendarPlot.R

box:: use(
  shiny[NS, moduleServer, tagList, icon, htmlOutput],
  googleVis[renderGvis, gvisCalendar]
)

#' CalendarPlot module (UI)
#'
#' A Shiny module containing calendar plot built using googleVis
#'
#' @param id id
#' @return ui to build a calendar plot in Shiny
#' @export
#'
ui <-  function(id, data){
  ns <-  NS(id)
  tagList(
    htmlOutput(ns("calendarplot"))
  )
}

#' CalendarPlot module (server)
#'
#' @param id id
#' @param data data with date column and value column
#' @param coldate string with the name of the Date column
#' @param colvalue string with the name of the Value column
#' @return server for CalendarPlot module with the plot displayed
#' @export

server <- function(id, data, coldate, colvalue){
  moduleServer(id, function(input, output, session){

    output$calendarplot <- renderGvis({
      Cal <-  gvisCalendar(data,
                           datevar = coldate,
                           numvar = colvalue,
                           options = list(
                             title = "",
                             calendar = "{yearLabel: { fontName: 'Times-Roman',
                                      fontSize: 32, color: '#1A8763', bold: true},
                                      cellSize: 10,
                                      cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                                      focusedCellColor: {stroke:'red'}}"
                           ))
      Cal
    })

  })
}
