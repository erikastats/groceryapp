# app/main.R

box::use(
  bslib[page_navbar, nav_panel, nav_menu],
  shiny[NS, tagList, moduleServer, br ]
)

box::use(
  app/view/overview
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_navbar(
    header = tagList(
      id = ns("navbar_id"),
      title = "Grocery analytics",
      nav_panel("Overview",
                icon = icon("magnifying-glass-dollar"),
                overview$ui(ns("overview_page"), #add values
                            )
                )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {



# modules -----------------------------------------------------------------
    overview$server("overview_page",)
  #add table
  })
}
