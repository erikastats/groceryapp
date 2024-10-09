# app/view/product_page

box::use(
  shiny[NS, moduleServer, tagList, icon, uiOutput, renderUI],
  shinyWidgets[airDatepickerInput, pickerInput],
  bslib[layout_columns, value_box, card, card_header,
        page_sidebar, sidebar],
  lubridate[floor_date],
  dplyr[filter]
)

box::use(
  app/logic/importing_data[filters_choices]
)

#' Product page (UI)
#'
#' A Shiny module containing analysis of products an their
#' categories from the grocery data#'
#' @param id id
#' @return ui for product page
#' @export

ui <-  function(id){
  ns <- NS(id)
  tagList(
    page_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        airDatepickerInput(
          inputId = ns("datestart_filter"),
          label = "Select start date:",
          value = c(floor_date(filters_choices$date_range$max_date, unit = "year"))
        ),
        airDatepickerInput(
          inputId = ns("dateend_filter"),
          label = "Select end date:",
          value = c(filters_choices$date_range$max_date)
        ),
        pickerInput(
          inputId = ns("supermarket_filter"),
          label = "Select a supermarket",
          choices = filters_choices$supermarkets,
          # selected = c("Tesco", "Lidl"),
          options = list(
            title = "Nothing selected",
            size = 5),
          multiple = TRUE
        ),
        pickerInput(
          inputId = ns("category_filter"),
          label = "Select a category",
          choices = unique(filters_choices$product_category$product_category),
          options = list(
            title = "Nothing selected",
            size = 5),
          multiple = TRUE
        ),
        uiOutput( ns("subcategory_filter"))
      )
    )
  )
}



#' Product page (server)
#'
#' @param id id
#' @return server for Product page
#' @export

server <- function(id){
  moduleServer(id, function(input, output, session){

    ns <- NS(id)


  # output ------------------------------------------------------------------

    output$subcategory_filter <- renderUI({

      data = filters_choices$product_category

      if (is.null(input$category_filter)){
        data = data |>
          filter(product_category %in% input$category_filter)
      }
      subcategory_choices = data$product_subcategory |> unique()

      pickerInput(
        inputId = ns("subcategory_filter2"),
        label = "Select a subcategory",
        choices = subcategory_choices,
        options = list(
          title = "Nothing selected",
          size = 5),
        multiple = TRUE
      )
    })

  })
}

