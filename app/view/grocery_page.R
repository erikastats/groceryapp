# app/view/grocery_page

box:: use(
  shiny[NS, moduleServer, tagList, icon, uiOutput, renderUI,
        br, h6, radioButtons, reactive],
  shinyWidgets[airDatepickerInput, pickerInput, checkboxGroupButtons,
               switchInput],
  bslib[page_sidebar, sidebar, layout_columns, value_box,
        card, card_header, card_body],
  dplyr[filter, summarise, ungroup, group_by, mutate, pull, n, arrange],
  lubridate[today, floor_date],
  plotly[plot_ly, plotlyOutput, renderPlotly, layout]

)

box::use(
  app/logic/importing_data[filters_choices, grocery_by_day_super],
  app/view/valuebox_analytics,
)

#' Analytics page (UI)
#'
#' A Shiny module containing analysis of grocery data
#'
#' @param id id
#' @return ui for Analytics groceries page with several
#'  insights about grocery data
#' @export

ui <- function(id){
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
          selected = c("Tesco", "Lidl"),
          options = list(
            title = "Nothing selected",
            size = 5),
          multiple = TRUE
        ),
        br(),
        h6("Select favorite products:"),
        switchInput(
          inputId = ns("favorite_filter"),
          label = icon("star")
        ),
        radioButtons(ns("groupby_filter"),
                     label = "Select the group",
                     selected = "Month",
                     choices = c("Week", "Month", "Year" ))
      ),
      valuebox_analytics$ui(ns("valueBox")),
      layout_columns(
        card(
          card_header("Daily amount spent"),
          plotlyOutput(ns("frequency_bar"))
          ),
        card(
          card_header("Periodic amount"),
          plotlyOutput(ns("frequency_bar_period"))
        )
      )
    )
  )
}

#' Analytics page (server)
#'
#' @param id id
#' @param groc_data groceries overview data
#' @return server for Analytics page
#' @export
server <-  function(id){
  moduleServer(id, function(input, output, session){

    ns <- NS(id)

  # reactive data -----------------------------------------------------------
    data_grocery <- reactive({
      data = grocery_by_day_super |>
        filter(grocery_day >= input$datestart_filter &
                 grocery_day <= input$dateend_filter)

      if (!is.null(input$supermarket_filter)){
        data = data |>
          filter(supermarket_name %in% input$supermarket_filter)
      }
      data
    })

    avg_grocery_val <- reactive({
      data_grocery()$value_grocery |>
        mean(na.rm = TRUE) |>
        round(2)
    })

    grocery_grouped <- reactive({
      data = data_grocery()
      if (input$groupby_filter == "Week"){
        data = data |>
          group_by(Year, Month, Week)
      } else {
        if (input$groupby_filter == "Month"){
          data = data |>
            group_by(Year, Month)
        } else {
          data = data |>
            group_by(Year)
        }
      }

      data |>
        summarise(count_trips = n(),
                  total_value = sum(value_grocery)) |>
        ungroup() |>
        summarise(avg_trips = mean(count_trips) |> round(2),
                  avg_value = mean(total_value) |> round(2))
    })


  # module ------------------------------------------------------------------
    valuebox_analytics$server(id = "valueBox",
                              avg_grocery = reactive({avg_grocery_val()}),
                              grocery_trips = reactive({grocery_grouped()$avg_trips}),
                              avg_grocery_period = reactive({grocery_grouped()$avg_value}),
                              period_f = reactive({input$groupby_filter})
                              )


  # output ------------------------------------------------------------------


    output$frequency_bar <- renderPlotly({
      data = data_grocery()

      data |>
        plot_ly(x = ~grocery_day, y = ~value_grocery,
                color = ~supermarket_name, type = "bar") |>
        layout(
          xaxis = list(title = "Date"),
          yaxis = list(title = "Amount (€)")
          )
      })

    output$frequency_bar_period <- renderPlotly({
      data = data_grocery()
      if (input$groupby_filter == "Week"){
        data = data |>
          group_by(Year, Month, Week, supermarket_name) |>
          summarise(total_grocery = sum(value_grocery)) |>
          ungroup() |>
          mutate(new_date = paste(Year,   Month,Week , sep = "-")) |>
          arrange(Year,   Month,Week)

      } else {
        if (input$groupby_filter == "Month"){
          data = data |>
            group_by(Year, Month, supermarket_name) |>
            summarise(total_grocery = sum(value_grocery)) |>
            ungroup() |>
            mutate(new_date = paste(Month,Year , sep = "-"))
        } else {
          data = data |>
            group_by(Year, supermarket_name) |>
            summarise(total_grocery = sum(value_grocery)) |>
            ungroup() |>
            mutate(new_date = Year)
        }
      }

      data |>
        plot_ly(x = ~new_date, y = ~total_grocery, type = 'bar',
                color = ~supermarket_name) |>
        layout(
          xaxis = list(title = "Date"),
          yaxis = list(title = paste0("Amount (€) per", input$groupby_filter))
        )

    })


  })
}
