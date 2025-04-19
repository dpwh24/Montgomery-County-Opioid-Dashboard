frontpage <- function(ed.data){
  
  # Get list of dates to display in slider
  dates <- ed.data$Date %>%
    sort() %>%
    unique() %>%
    format("%m/%d/%y")
  
  # Get OD type for input control
  od.types <- ed.data$od_type %>%
    levels()
  
  # Get ages for input control
  ages <- ed.data$Age %>%
    levels()

  # Get genders for input control
  genders <- ed.data$Gender %>%
    levels()
  
  # Get races for input control
  races <- ed.data$Race %>%
    levels()
  
  # Get rate types for input control
  rates <- ed.data$rate_type %>%
    levels()
  
  layout_sidebar(
    fillable = TRUE,
    sidebar = sidebar(
      shinyWidgets::sliderTextInput("date",
                                    label = "Select Date:",
                                    choices = dates,
                                    selected = dates[length(dates)]),
      selectInput("odType",
                  label = "Overdose Type:",
                  choices = od.types,
                  selected = od.types[1]),
      selectInput("age",
                  label = "Age:",
                  choices = ages,
                  selected = ages[length(ages)]),
      selectInput("gender",
                  label = "Gender:",
                  choices = genders,
                  selected = genders[1]),
      # uiOutput("age.choice"),
      # uiOutput("gender.choice"),
      selectInput("race",
                  label = "Race:",
                  choices = races,
                  selected = races[1]),
      shinyWidgets::awesomeRadio("rateType",
                                 "Rate Type:",
                                 rates,
                                 selected = rates[1],
                                 inline = TRUE),
      
    ),
    layout_columns(
      value_box( 
        title = "Rate of Emergency Department Visits for Drug Overdose", 
        uiOutput("od.rate.denom"),
        uiOutput("date1"),
        showcase = plotlyOutput("sparkline.rate"), 
        value = uiOutput("od.rate"),  
        theme = "bg-gradient-indigo-orange",
        class = "border",
        showcase_layout = "bottom"
      ), 
      value_box( 
        title = "County Rank of Emergency Department Visits for Drug Overdose", 
        uiOutput("od.rank.denom"),
        uiOutput("date2"),
        "*Total counties may be reduced due to missing data",
        showcase = icon("ranking-star"), 
        value = uiOutput("od.rank"),  
        theme = "bg-gradient-pink-purple",
        class = "border"
      ), 
      value_box( 
        title = "Rate of Emergency Department Visits for Drug Overdose",
        "Percentage of State Rate",
        uiOutput("date3"),
        showcase = plotlyOutput("sparkline.rate.compare"), 
        showcase_layout = "bottom",
        value = uiOutput('od.rate.compare'),  
        theme = "bg-gradient-teal-indigo",
        class = "border"
      ),
      col_widths = c(4,4,4)
    ),
    layout_columns(
      card(leafletOutput(outputId = "map") %>% withSpinner(),
           card_footer("Counties are colored according to their ER rates by the selections at the left. Take back boxes are added for information. Layers can be toggled with the control at the top-right.")),
      card(plotlyOutput("plot.rates") %>% withSpinner(),
           card_footer("Counties are ordered by rate. Montgomery county is shaded differently from the other counties. Counties with no reported data are excluded.")),
      col_widths = c(6,6)
    ),
    layout_columns(
      card(plotlyOutput("er.od.by.race") %>% withSpinner(),
           card_footer()),
      card(plotlyOutput("er.od.by.gender") %>% withSpinner(),
           card_footer()),,
      col_widths = c(6,6)
    ),
    card(plotlyOutput("bup.er.od") %>% withSpinner(),
         card_footer("Period over Period percent change. Positive values represent increasing rates from the previous period, a negative values indicate decreasing rates from the previous period.")),
    layout_columns(
      card(plotlyOutput("bup.er.od.scatter.gender") %>% withSpinner(),
           card_footer("Rates for Males and Females are separated into distinct clusters. This indicates that Males tend to be prescribed Buprenorphine at higher rates. Similarly, Males appear to have slightly higher rates than Females of ER OD visits.")),
      card(plotlyOutput("bup.er.od.scatter.age") %>% withSpinner(),
           card_footer("ER OD visits have an approximately linear relationship with rates of Buprenorphine prescriptions and dispensations. Most ages cluster into distinct regions which indicates the population is not changing significantly over time.")),
      col_widths = c(6,6)
    )
    
    
    
    
  )
}