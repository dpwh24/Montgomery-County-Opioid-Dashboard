risky_prescribe <- function(data){
  
  risky.measure <- data$risky_measure %>%
    levels()
  
  risky.dates <- data$Date %>%
    sort() %>%
    unique() %>%
    format("%m/%d/%y")
  
  layout_sidebar(
    fillable = TRUE,
    sidebar = sidebar(
      shinyWidgets::sliderTextInput("risky.date",
                                    label = "Select Date:",
                                    choices = risky.dates,
                                    selected = risky.dates[length(risky.dates)]),
      selectInput("riskyMeasure",
                  label = "Measure:",
                  choices = risky.measure,
                  selected = risky.measure[1])),
    layout_columns(
      value_box( 
        title = "County Risky Prescribing Practice Rate", 
        "Individuals per 10,000 population",
        uiOutput("date4"),
        showcase = plotlyOutput("sparkline.risky.rate"),
        value = uiOutput("risky.montco.rate"),  
        theme = "bg-gradient-indigo-orange",
        class = "border",
        showcase_layout = "bottom"
      ),
      value_box( 
        title = "County Rank Risky Prescribing Practice", 
        uiOutput("risky.county.count"),
        uiOutput("date5"),
        "*Total counties may be reduced due to missing data",
        showcase = icon("ranking-star"), 
        value = uiOutput("risky.montco.rank"),  
        theme = "bg-gradient-pink-purple",
        class = "border"
      ), 
      value_box( 
        title = "County Risky Prescribing Practice Rate",
        "Percentage of State Rate",
        uiOutput("date6"),
        showcase = plotlyOutput("sparkline.risky.rate.compare"),
        showcase_layout = "bottom",
        value = uiOutput('risky.montco.compare.pa'),  
        theme = "bg-gradient-teal-indigo",
        class = "border"
      ),
      col_widths = c(4,4,4)
    ),
    layout_columns(
      card(plotlyOutput("risky.state.compare" ) %>% withSpinner()),
      card(plotlyOutput("risky.rate" ) %>% withSpinner()),
      col_widths = c(6,6)
    )
  )
}