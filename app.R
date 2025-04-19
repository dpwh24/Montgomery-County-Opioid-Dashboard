# Montgomery County Opioid Dashboard example
# Author: David Walsh, PhD
# 18 Apr 2024


library(shiny)
library(leaflet)
library(tidyverse)
library(bslib)
library(shinycssloaders)
library(plotly)
library(DT)

# Set constants for county ID numbers of interest
MONTCO = 46
PA = 0

# Set up buprenorphine data
buprenorphine.data <- "data/Buprenorphine_Dispensation_Data_Quarter_3_2016_-_Current_Quarterly_Statewide_Health_20250417.csv"
buprenorphine.data <- read_csv(buprenorphine.data)
buprenorphine.data <- buprenorphine.data %>%
    filter(`County Code Number` %in% c(PA, MONTCO)) %>%
    mutate(measure_type = as.factor(`Type of Rate or Count Measure`),
           measure = as.factor({
               str_remove(measure_type, " \\(.*") %>%
                   str_remove(".*of ")}),
           measure_type = as.factor(str_remove(measure_type, "\\s.*")),
           Date = mdy(`Quarter Date Start`),
           Age = as.factor(`Age Group`),
           Gender = as.factor(Gender)) %>%
    select(`County Name`, Date, Age, Gender, measure_type, measure, `Rate or Count`) %>%
    # pivot_wider(names_from = `County Name`, values_from = `Rate or Count`) %>%
    # pivot_wider(values_from = c(Montgomery, Pennsylvania), names_from = measure) %>%
    group_by(`County Name`, Age, Gender, measure_type, measure) %>%
    arrange(Date, .by_group = TRUE) %>%
    mutate(buprenorphine_pct_chg = (`Rate or Count`/lag(`Rate or Count`) - 1) * 100) %>%
    ungroup() %>%
    mutate(across(everything(), ~replace(., is.nan(.), NA))) %>%
    mutate(across(everything(), ~replace(., is.infinite(.), NA))) %>%
    rename(buprenorphine_value = `Rate or Count`,
           County = `County Name`)

# Set up risky prescribing data
risky.prescribing <- "data/Risky_Prescribing_Measures_Quarter_3_2016_-_Current_Quarterly_County___Statewide_Health_20250416.csv"
risky.prescribing <- read_csv(risky.prescribing)
risky.prescribing <- risky.prescribing %>%
    mutate(County = as.factor(County),
           risky_type = as.factor(`Risky Measure Type`),
           risky_measure = as.factor({
               str_remove(risky_type, " per .*") %>%
                   str_remove("\\w+.*(Seeing|with) ")}),
           risky_type = as.factor(str_remove(risky_type, "\\s.*")),
           Date = mdy(`Quarter Date Start`)) %>%
    rename(county_code = `County Code Number`,
           Value = `Rate or Count`) %>%
    select(County, county_code, Value, risky_type, risky_measure, Date) 

risky.prescribing.pa <- risky.prescribing %>%
    filter(county_code == PA) %>%
    rename(state_value = Value)

risky.prescribing <- risky.prescribing %>%
    filter(county_code != PA) %>%
    group_by(risky_type, risky_measure, Date) %>%
    mutate(rank = rank(-Value,
                       na.last = "keep",
                       ties.method = "max")) %>%
    ungroup() %>%
    left_join(risky.prescribing.pa %>%
                  select(-County, -county_code),
              by=c("risky_type" = "risky_type",
                   "risky_measure" = "risky_measure",
                   "Date" = "Date")) %>%
    mutate(state_compare = round((Value/state_value-1)*100, 2)) %>%
    select(-state_value) %>%
    union_all(risky.prescribing.pa %>%
                  mutate(state_compare = NA,
                         rank = NA) %>%
                  rename(Value = state_value))

# Read in the Takeback Box data
takeback.boxes <- "data/Prescription_Drug_Take-Back_Box_Locations_County_Drug_and_Alcohol_Programs_20250415.csv"
takeback.boxes <- read_csv(takeback.boxes)

# Read in the PA county shape file
shapefile <- "data/Pennsylvania_County_Boundaries.shp"
# Convert the PA county codes to numeric for easier joining later
shapefile <- sf::st_read(shapefile) %>%
    mutate(PA_CTY_COD = as.numeric(PA_CTY_COD))
# Make shape file usable as a geospatial object
shapefile <- sf::st_transform(shapefile, crs = '+proj=longlat +datum=WGS84')

# Read in the ED Overdose Data
ed.data <- "data/Emergency_Department__ED__Visits_for_Drug_Overdose_Identified_Through_Syndromic_Surveillance_SFY_Quarter_3_2016_-_Current_Quarterly_County_Health_20250414.csv"
ed.data <- read_csv(ed.data)
ed.data <- ed.data %>%
    mutate(Age = as.factor(`Age Group`),
           Gender = as.factor(Gender),
           Race = as.factor(race_group),
           rate_type = factor(`Type of Rate`, labels=c("Home", "Facility")),
           Date = mdy(`Quarter Date Start`),
           od_type = as.factor(`Overdose Type`),
           `County Name` = as.factor(`County Name`)) %>%
    select(Age, Gender, Race, rate_type, od_type, Date, `County Name`, Rate, `County Code Number`, `State FIPS Code`, `County FIPS Code`)

# Split out the state ED overdose data
state.ed.data <- ed.data %>%
    filter(`County Code Number` == PA) %>%
    select(Age, Gender, Race, rate_type, od_type, Date, Rate) %>%
    rename(state_rate = Rate)

# Join the state data back to the ED data to compare counties to state
ed.data <- ed.data %>%
    filter(`County Code Number` != PA) %>%
    group_by(Age, Gender, Race, rate_type, od_type, Date) %>%
    mutate(Rank = rank(-Rate,
                       na.last = "keep",
                       ties.method = "max")) %>%
    ungroup() %>%
    inner_join(state.ed.data) %>%
    mutate(state_compare = round((Rate/state_rate-1)*100, 2))

montco.base.data <- ed.data %>%
    filter(`County Code Number` == MONTCO) %>%
    group_by(Age, Gender, Race, rate_type, od_type) %>%
    arrange(Date, .by_group = TRUE) %>%
    mutate(pct_chg = (Rate/lag(Rate)-1)*100) %>%
    ungroup() %>%
    mutate(across(everything(), ~replace(., is.nan(.), NA))) %>%
    mutate(across(everything(), ~replace(., is.infinite(.), NA)))

# montco.population <- buprenorphine.data %>%
#     select(-buprenorphine_pct_chg) %>%
#     filter(measure == "Prescriptions") %>%
#     group_by(`County Name`, Date, Age, Gender) %>%
#     pivot_wider(names_from = measure_type,
#                 values_from = buprenorphine_value) %>%
#     mutate(population = round(1/(Rate/Number)*10000, 0)) %>%
#     ungroup() %>%
#     select(`County Name`, Date, Age, Gender, population) %>%
#     mutate(population = replace(population, is.nan(population), NA)) %>%
#     arrange(Date)

# montco.base.data2 <- montco.base.data %>%
#     left_join(montco.population, by=c("County Name" = "County Name",
#                                       "Age" = "Age",
#                                       "Gender" = "Gender",
#                                       "Date" = "Date")) %>%
#     mutate(count = Rate*(population/10000))

# Join the ED overdose data to the shapefile
map.ed.data <- merge(ed.data,
                     shapefile,
                     by.x = 'County Code Number',
                     by.y = 'PA_CTY_COD') %>%
    sf::st_as_sf()

# Get ages for input control
ages <- ed.data$Age %>%
    levels()

# Get genders for input control
genders <- ed.data$Gender %>%
    levels()

# Get races for input control
races <- ed.data$Race %>%
    levels()

# Get OD type for input control
od.types <- ed.data$od_type %>%
    levels()


montco.bup <- montco.base.data %>% 
    filter(Race == races[1]) %>%
    select(-Race) %>%
    inner_join(buprenorphine.data %>% 
                   filter(measure_type=="Rate") %>% 
                   mutate(measure = if_else(measure == "Dispensations", "Facility", "Home")),
               by=c('County Name'='County', 
                    'Age'='Age', 
                    'Gender'='Gender',
                    'rate_type'='measure',
                    'Date'='Date')) %>%
    rename(er_rate = Rate)

# bup.facility.model <- montco.bup %>%
#     filter(Age %in% ages[3:7], 
#            rate_type == "Facility") %>%
#     lm(er_rate~buprenorphine_value, .)
# 
# bup.home.model <- montco.bup %>%
#     filter(Age %in% ages[3:7], 
#            rate_type == "Home") %>%
#     lm(er_rate~buprenorphine_value, .)

# source("mod/mod_map_setStyle.R")
# source("scripts/setShapeStyle.R")
source("mod/mod_layout_frontpage.R")
source("mod/mod_layout_risky_prescribe.R")

# Begin UI section
ui <- fluidPage(
    # setStyle,
    bootstrapLib(bs_theme()),
    
    navset_tab( 
        nav_panel("ER OD Overview", frontpage(ed.data)), 
        nav_panel("ER OD Data", dataTableOutput("datatable") %>% withSpinner()),
        nav_panel("Buprenorphine/ER OD Data", dataTableOutput("bup.er.data") %>% withSpinner()),
        nav_panel("Risky Prescribing", risky_prescribe(risky.prescribing)),
        nav_panel("Risky Prescribing Data", dataTableOutput("risky.data.table") %>% withSpinner()))
    
)

# Begin server section
server <- function(input, output){
    
    # Reactive variable to filter the data
    selectedData <- reactive({
        map.ed.data %>%
            filter(Date == mdy(input$date),
                   rate_type == input$rateType,
                   Race == input$race,
                   Age == input$age,
                   Gender == input$gender,
                   od_type == input$odType
                   )
    })
    
    # Extract longitudinal data for Montgomery county
    montcoData <- reactive({
        montco.base.data %>%
            filter(rate_type == input$rateType,
                   Race == input$race,
                   Age == input$age,
                   Gender == input$gender,
                   od_type == input$odType) %>%
            select(Date, Rate, Rank, `County Code Number`, state_compare, pct_chg)
        # map.ed.data %>%
        #     filter(`County Code Number` %in% c(PA, MONTCO),
        #            rate_type == input$rateType,
        #            Race == input$race,
        #            Age == input$age,
        #            Gender == input$gender,
        #            od_type == input$odType) %>%
        #     select(Date, Rate, Rank, `County Code Number`, state_compare)
    })
    
    
    map_palette <- reactive({
        colorNumeric(palette = "Blues",
                     domain = selectedData()$Rate)
    })
    
    # Get the total number of available counties in the filtered data as not all counties have data to rank
    county_count <- reactive({
        selectedData() %>%
            summarise(n=sum(!is.na(Rank))) %>%
            pull(n)
    })
    
    
    per_capita_type <- reactive({
        ifelse(input$rateType == "Facility", "Visits", "Population")
    })
    
    montco.data <- reactive({
        selectedData() %>%
            select(`County Name`, Rate, Rank, `County Code Number`, state_compare) %>%
            filter(`County Code Number` == MONTCO)
    })
    
    riskyFiltered <- reactive({
        risky.prescribing %>%
            filter(risky_measure == input$riskyMeasure)
    })
    
    montcoRisky <- reactive({
        riskyFiltered() %>%
            filter(county_code == MONTCO,
                   Date == mdy(input$risky.date),
                   risky_type == "Rate")
    })
    
    buprenorphineFiltered <- reactive({
        buprenorphine.data %>%
            filter(measure == ifelse(input$rateType == "Facility", "Dispensations", "Prescriptions"),
                   Age == input$age,
                   Gender == input$gender)
    })
    # plot_ranks <- reactive({
    #     selectedData() %>%
    #         select(`County Name`,Rate) %>%
    #         order_by(desc(Rate))
    # })
    
    # Set highlighting options for hovering on counties
    hl_opts <- highlightOptions(
        color = "#CC0000",
        weight = 3,
        bringToFront = FALSE)   
    
    marker_options <- markerOptions(
        zIndexOffset = max(ed.data$`County Code Number`) * 1000)
    
    gender.choice <- reactive({
        input$gender == genders[1]
    })
    
    age.choice <- reactive({
        input$age == ages[length(ages)]
    })
    
    race.choice <- reactive({
        input$race == races[1]
    })
    
    od.choice <- reactive({
        input$odType %in% od.types[1:2]
    })
    
    observeEvent(gender.choice(), {
        if(!gender.choice()){
            updateSelectInput(inputId = "age",
                              selected = ages[length(ages)])
            updateSelectInput(inputId = "race",
                              selected = races[1])
            if(input$odType %in% od.types[2:3]){
                updateSelectInput(inputId = "odType",
                                  selected = od.types[1])
            }
        }
    })
    
    observeEvent(age.choice(), {
        if(!age.choice()){
            updateSelectInput(inputId = "gender",
                              selected = genders[1])
            updateSelectInput(inputId = "race",
                              selected = races[1])
            if(input$odType %in% od.types[2:3]){
                updateSelectInput(inputId = "odType",
                                  selected = od.types[1])
            }
        }
    })
    
    observeEvent(race.choice(), {
        if(!race.choice()){
            updateSelectInput(inputId = "gender",
                              selected = genders[1])
            updateSelectInput(inputId = "age",
                              selected = ages[length(ages)])
            if(input$odType %in% od.types[2:3]){
                updateSelectInput(inputId = "odType",
                                  selected = od.types[1])
            }
        }
    })
    
    observeEvent(od.choice(), {
        if(!od.choice()){
            updateSelectInput(inputId = "age",
                              selected = ages[length(ages)])
            updateSelectInput(inputId = "race",
                              selected = races[1])
            updateSelectInput(inputId = "gender",
                              selected = genders[1])
        }
    })
    
    # output$gender.choice <- renderUI({
    #     selectInput("gender",
    #                 label = "Gender:",
    #                 choices = ifelse(input$age == ages[length(ages)], genders, genders[1]),
    #                 selected = genders[1])
    # })
    # 
    # output$age.choice <- renderUI({
    #     selectInput("age",
    #                 label = "Age:",
    #                 choices = ifelse(input$gender == genders[1], ages, ages[length(ages)]),
    #                 selected = ages[length(ages)])
    # })
    
    output$od.rate <- renderText({ 
        glue::glue("{montco.data()$Rate}")
    })
    
    output$od.rate.compare <- renderText({ 
        glue::glue("{montco.data()$state_compare}%")
    })
    
    output$od.rate.denom <- renderText({
        glue::glue("per 10,000 {per_capita_type()}")
    })
    
    output$od.rank <- renderText({ 
        glue::glue("{montco.data()$Rank}")
    })
    
    output$od.rank.denom <- renderText({
        req(input$date)
        glue::glue("of {county_count()}")
    })
    
    output$date1 <- renderText({
        glue::glue("as of {input$date}")
    })
    
    output$date2 <- renderText({
        glue::glue("as of {input$date}")
    })
    
    output$date3 <- renderText({
        glue::glue("as of {input$date}")
    })
    
    output$date4 <- renderText({
        glue::glue("as of {input$risky.date}")
    })
    
    output$date5 <- renderText({
        glue::glue("as of {input$risky.date}")
    })
    
    output$date6 <- renderText({
        glue::glue("as of {input$risky.date}")
    })
    
    # Create the map
    output$map <- renderLeaflet({
        leaflet() %>%
            # Set the center of the map to geographical center of PA
            setView(lng = -77.74667,
                    lat = 40.89667,
                    zoom = 7) %>% 
            # Add the map view background
            addTiles(options = tileOptions(minZoom=7)) %>%
            # Add the county shapes using the shapefile
            addPolygons(data=shapefile,
                        color = "black",
                        weight = 1,
                        fillOpacity = 0,
                        # highlightOptions = hl_opts,
                        layerId = ~COUNTY_NAM,
                        group = "county_map"
                        ) %>%
            # Add markers to indicate location of takeback boxes
            addCircleMarkers(data=takeback.boxes,
                             lat = ~Latitude,
                             lng = ~Longitude,
                             weight = 1,
                             popup = ~paste(`Drug Take-Back Site`,"<br/>",
                                            Address,"<br/>",
                                            City,", PA",`Zip Code`, sep=""),
                             group = "takeback_boxes",
                             # options = markerOptions(zIndexOffset = 100)
                             ) %>%
            # Add option to toggle the takeback box and rate data overlays
            addLayersControl(baseGroups = "county_map",
                             overlayGroups = c("county_data", "takeback_boxes"),
                             options = layersControlOptions(autoZIndex = FALSE))
    })
    
    # Create a bar chart showing the rate and rank of each county for the selected input options
    output$plot.rates <- renderPlotly({
        # Plot relies on reactive selections which don't exist at initial render
        # req(input$date)
        
        if(input$age != ages[length(ages)] & input$gender != genders[1] & input$race != races[1]){
            validate("Invalid Demographic combination")
        }
        
        if(input$age != ages[length(ages)] & input$gender != genders[1] & input$race != races[1]){
            validate("Invalid Demographic combination")
        }
        
        # Create bar chart, exclude NAs to avoid confusion with counties with 0 rate
        data <- selectedData() %>%
            select(`County Name`, Rate, Rank, `County Code Number`) %>%
            drop_na() %>%
            arrange(desc(Rate)) %>%
            mutate(`County Name` = fct_reorder(`County Name`,
                                               Rate,
                                               na.rm=TRUE),
                   county_color = ifelse(`County Code Number` == MONTCO, "blue", "orange"))
        
        county.color <- data$county_color

        p <-data  %>%
            ggplot() +
            geom_bar(aes(y=`County Name`,
                         x=Rate,
                         fill= county_color,
                         text= paste("<b>", `County Name`, "</b><br>",
                                     "Rate: ", Rate, "<br>",
                                     "Rank: ", Rank, " of ", county_count(), sep="")),
                     stat = "identity") +
            # scale_fill_manual(values=county.color) +
            ylab("") +
            xlab(paste("Rate per 10,000 ", per_capita_type(), sep="")) +
            theme(legend.position = "none") +
            ggtitle("ER Visit for OD Rate per 10,000 population by County")
        ggplotly(p, tooltip = "text")
    })
    
    output$er.od.by.race <- renderPlotly({
        
        p <-montco.base.data %>%
            filter(Race != races[1]) %>%
            ggplot(aes(x=Date, y=Rate, color=Race)) + 
            geom_line() +
            facet_grid(rate_type~od_type,
                       scales="free_y") +
            ylab("Rate per 10,000 Population") +
            xlab("") +
            ggtitle("Rates of ER Overdose visits by Race over Time in Montgomery County") +
            theme(legend.position = "bottom")
        
        ggplotly(p) %>%
            layout(legend = list(orientation = 'h'))
    })
    
    output$er.od.by.gender <- renderPlotly({
        
        if(input$odType %in% od.types[3:4]){
            validate("No gendered data exists for the selected OD Type.")
        }
        
        p <-montco.base.data %>%
            filter(Gender != genders[1],
                   od_type == input$odType,
                   rate_type == input$rateType) %>%
            pivot_longer(cols = c(Rate, state_rate), names_to = "County", values_to = "Rate") %>%
            mutate(County = ifelse(County == "Rate", "Montgomery", "PA")) %>%
            ggplot(aes(x=Date, y=Rate, fill=County)) + 
            geom_bar(stat="identity", position="dodge") +
            facet_grid(~Gender,
                       scales="free_y") +
            ylab("Rate per 10,000 Population") +
            xlab("") +
            ggtitle("Rates of ER Overdose visits by Gender over Time in Montgomery County and PA") +
            theme(legend.position = "bottom")
        
        ggplotly(p) %>%
            layout(legend = list(orientation = 'h', title=""))
    })
    
    output$datatable <- renderDataTable({
        datatable(ed.data, filter = "top")
    })
    
    output$sparkline.rate <- renderPlotly({
        
        if(input$age != ages[length(ages)] & input$gender != genders[1] & input$race != races[1]){
            validate("Invalid Age/Gender/Race selection")
        }
        
        p <- montcoData() %>%
            # filter(`County Code Number` == MONTCO) %>%
            ggplot() +
            geom_line(aes(y=Rate, x=Date)) +
            geom_vline(xintercept=mdy(input$date), linetype='dashed') +
            geom_ribbon(aes(x=Date , y=Rate, ymin = 0, ymax=Rate), fill = "white", alpha = 0.2) +
            scale_x_continuous(expand=c(0,0)) +
            scale_y_continuous(expand=c(0,0)) +
            theme_void() +
            theme(legend.position = "none")
        
        ggplotly(p) %>%
            style(line = list(color = "#FFFFFF",
                              # fill = "tozeroy",
                              # alpha = 0.2,
                              hovertemplate = '%{x}: %{y:.2f}<extra></extra>')) %>%
            style(line = list(color = "#FFFFFF",
                              dash = 'dash'),
                  traces = 2) %>%
            style(hoverinfo = "none", traces = 2:3) %>%
            layout(
                xaxis = list(visible = FALSE, showgrid = FALSE),
                yaxis = list(visible = FALSE, showgrid = FALSE),
                hovermode = "x",
                margin = list(t = 0, r = 0, l = 0, b = 0),
                paper_bgcolor = "transparent",
                plot_bgcolor = "transparent"
            ) %>%
            config(displayModeBar = FALSE)
        
    })
    
    output$sparkline.rate.compare <- renderPlotly({
        
        if(input$age != ages[length(ages)] & input$gender != genders[1] & input$race != races[1]){
            validate("Invalid Age/Gender/Race selection")
        }
        
        p <- montcoData() %>%
            ggplot() +
            geom_line(aes(y=state_compare, x=Date)) +
            geom_vline(xintercept=mdy(input$date), linetype='dashed') +
            geom_ribbon(aes(x=Date , y=0, ymin = state_compare, ymax=0), fill = "white", alpha = 0.2) +
            scale_x_continuous(expand=c(0,0)) +
            scale_y_continuous(expand=c(0,0)) +
            theme_void() +
            theme(legend.position = "none")
        
        ggplotly(p) %>%
            style(line = list(color = "#FFFFFF",
                              hovertemplate = '%{x}: %{y:.2f}%<extra></extra>')) %>%
            style(line = list(color = "#FFFFFF",
                              dash = 'dash'),
                  traces = 2) %>%
            style(hoverinfo = "none", traces = 2:3) %>%
            layout(
                xaxis = list(visible = FALSE, showgrid = FALSE),
                yaxis = list(visible = FALSE, showgrid = FALSE),
                hovermode = "x",
                margin = list(t = 0, r = 0, l = 0, b = 0),
                paper_bgcolor = "transparent",
                plot_bgcolor = "transparent"
            ) %>%
            config(displayModeBar = FALSE)
        
    })
    
    output$sparkline.risky.rate <- renderPlotly({
        
        p <- riskyFiltered() %>%
            filter(county_code == MONTCO,
                   risky_type == "Rate") %>%
            ggplot() +
            geom_line(aes(y=Value, x=Date)) +
            geom_vline(xintercept=mdy(input$risky.date), linetype='dashed') +
            geom_ribbon(aes(x=Date , y=Value, ymin = 0, ymax=Value), fill = "white", alpha = 0.2) +
            scale_x_continuous(expand=c(0,0)) +
            scale_y_continuous(expand=c(0,0)) +
            theme_void() +
            theme(legend.position = "none")
        
        ggplotly(p) %>%
            style(line = list(color = "#FFFFFF",
                              # fill = "tozeroy",
                              # alpha = 0.2,
                              hovertemplate = '%{x}: %{y:.2f}<extra></extra>')) %>%
            style(line = list(color = "#FFFFFF",
                              dash = 'dash'),
                  traces = 2) %>%
            style(hoverinfo = "none", traces = 2:3) %>%
            layout(
                xaxis = list(visible = FALSE, showgrid = FALSE),
                yaxis = list(visible = FALSE, showgrid = FALSE),
                hovermode = "x",
                margin = list(t = 0, r = 0, l = 0, b = 0),
                paper_bgcolor = "transparent",
                plot_bgcolor = "transparent"
            ) %>%
            config(displayModeBar = FALSE)
        
    })
    
    output$sparkline.risky.rate.compare <- renderPlotly({
        
        p <- riskyFiltered() %>%
            filter(county_code == MONTCO,
                   risky_type == "Rate") %>%
            ggplot() +
            geom_line(aes(y=state_compare, x=Date)) +
            geom_vline(xintercept=mdy(input$risky.date), linetype='dashed') +
            geom_ribbon(aes(x=Date , y=0, ymin = state_compare, ymax=0), fill = "white", alpha = 0.2) +
            scale_x_continuous(expand=c(0,0)) +
            scale_y_continuous(expand=c(0,0)) +
            theme_void() +
            theme(legend.position = "none")
        
        ggplotly(p) %>%
            style(line = list(color = "#FFFFFF",
                              hovertemplate = '%{x}: %{y:.2f}%<extra></extra>')) %>%
            style(line = list(color = "#FFFFFF",
                              dash = 'dash'),
                  traces = 2) %>%
            style(hoverinfo = "none", traces = 2:3) %>%
            layout(
                xaxis = list(visible = FALSE, showgrid = FALSE),
                yaxis = list(visible = FALSE, showgrid = FALSE),
                hovermode = "x",
                margin = list(t = 0, r = 0, l = 0, b = 0),
                paper_bgcolor = "transparent",
                plot_bgcolor = "transparent"
            ) %>%
            config(displayModeBar = FALSE)
        
    })
    
    output$risky.montco.rate <- renderText({
        glue::glue("{round(montcoRisky()$Value,2)}")
    })

    output$risky.montco.rank <- renderText({
        glue::glue("{montcoRisky()$rank}")
    })

    output$risky.county.count <- renderText({
        n <- riskyFiltered() %>%
            filter(risky_type == "Rate") %>%
            summarise(n=max(rank, na.rm = T)) %>%
            pull(n)

        glue::glue("of {n}")
    })

    output$risky.montco.compare.pa <- renderText({
        glue::glue("{montcoRisky()$state_compare}%")
    })
    
    output$risky.state.compare <- renderPlotly({
        p <- riskyFiltered() %>%
                filter(risky_type == "Rate",
                       county_code %in% c(PA, MONTCO)) %>%
            ggplot() +
            geom_bar(aes(x=Date, y=Value, fill=County),
                     stat = "identity",
                     position='dodge') +
            ylab("Rate of Individuals per 10,000 population") +
            xlab("")
        
        ggplotly(p) %>%
            layout(legend=list(title=list(text='')),
                   title="Risky Presciption Practice Rate for Montgomery County vs. PA overall")
        
    })
    
    output$risky.rate<- renderPlotly({
        plot_ly(
            riskyFiltered() %>%
                filter(Date == mdy(input$risky.date),
                       risky_type == "Rate",
                       county_code != PA) %>%
                arrange(desc(Value)) %>%
                drop_na() %>%
                mutate(County = fct_reorder(County,
                                            Value,
                                            na.rm=TRUE),
                       county_color = ifelse(County == "Montgomery", "maroon", "lightblue")),
            type='bar',
            marker = list(color = ~county_color),
            x=~Value,
            y=~County) %>%
            layout(xaxis = list(
                       title = "Persons per 10,000 population"),
                   yaxis = list(
                       title = ""),
                   title="Risky Presciption Practice Rate for Montgomery County vs. PA counties")
    })
    
    output$risky.data.table <- renderDataTable({
        datatable(risky.prescribing, filter = "top")
    })
    
    output$bup.er.od <- renderPlotly({
        
        if(input$age != ages[length(ages)] & input$gender != genders[1] & input$race != races[1]){
            validate("Invalid Age/Gender/Race selection")
        }
        
        p<- montcoData() %>%
            ggplot() +
            geom_line(aes(x = Date, y = pct_chg, color = "ER OD Visit")) +
            geom_line(data = buprenorphineFiltered() %>%
                          filter(measure_type == "Rate"), 
                      aes(x = Date, y = buprenorphine_pct_chg, color = County)) +
            # geom_hline(yintercept = 0) +
            ylab("Percent Change from previous time point") +
            xlab("") + 
            ggtitle("Percent Change Over Time for County and State Buprenorphine Rates Against County ER OD Rates")
            
        ggplotly(p) %>%
            layout(legend=list(title=list(text=''), orientation = 'h'))
    })
    
    output$bup.er.od.scatter.age <- renderPlotly({
        
        p <- montco.bup %>%
            filter(Age != "All Ages") %>%
            ggplot(aes(x=buprenorphine_value, y=er_rate, color=Age)) + 
            geom_point() + 
            facet_grid(rate_type~od_type,
                       scales="free_y") +
            ylab("ER Visits for OD per 10,000 population") +
            xlab("Prescriptions/Dispensations per 10,0000 population") +
            ggtitle("Buprenorphine rate vs. ER OD visit rate by patient home for all races by age")
        
        ggplotly(p) %>% 
            layout(
                legend=list(title=list(text=''),
                            orientation = 'h'))
        # title="Prescriptions vs. ER vists by patient home for all races by age")
    })
    
    output$bup.er.od.scatter.gender <- renderPlotly({
        
        # bup.type <- ifelse(input$rateType == "Home", "Prescriptions", "Dispensations")
        
        p <- montco.bup %>%
            filter(Gender != "All Genders") %>%
            ggplot(aes(x=buprenorphine_value, y=er_rate, color=Gender)) + 
            geom_point() + 
            facet_grid(rate_type~od_type,
                       scales="free_y") +
            ylab("ER Visits for OD per 10,000 population") +
            xlab("Prescriptions/Dispensations per 10,0000 population") +
            ggtitle("Buprenorphine rate vs. ER OD visit rate for all races by gender")
        
        ggplotly(p) %>% 
            layout(legend=list(title=list(text=''), orientation = 'h'),
                   margin = list(
                       b = 0, 
                       l = 60, 
                       r = 0, 
                       t = 80, 
                       pad = 0, 
                       autoexpand = TRUE
                   ))
                   # title="Prescriptions vs. ER vists by patient home for all races by gender")
    })
    
    output$bup.er.data <- renderDataTable({
        datatable(montco.bup, filter = "top")
    })
    
    # Observe method to update map with county rate data based on input selections
    observe({
        leafletProxy(mapId = "map", data=selectedData()) %>%
            # setShapeStyle(layerId = ~COUNTY_NAM,
            #               fillColor = ~map_palette()(Rate),
            #               fillOpacity = 0.7,
            #               highlightOptions = hl_opts,
            #               # group = "county_data",
            #               popup = ~paste("<strong>",COUNTY_NAM,"</strong><br/>",
            #                              "Rate per 10k: ",Rate,"<br/>",
            #                              "County rank: ",Rank," of ",county_count(),"<br/>",
            #                              "State compare: ",state_compare,"%", sep="")
            #               )
            clearGroup(group="county_data") %>%
            removeControl("legend") %>%
            addPolygons(data=selectedData(),
                        color = "black",
                        weight = 1,
                        fillColor = ~map_palette()(Rate),
                        fillOpacity = 0.7,
                        highlightOptions = hl_opts,
                        group = "county_data",
                        popup = ~paste("<strong>",COUNTY_NAM,"</strong><br/>",
                                       "Rate per 10k: ",Rate,"<br/>",
                                       "County rank: ",Rank," of ",county_count(),"<br/>",
                                       "State compare: ",state_compare,"%", sep=""),
                        # options = markerOptions(zIndexOffset = -10000)
            ) %>%
            addLegend(position = "bottomright",
                      pal = map_palette(),
                      values = ~Rate,
                      title = "ER Rate",
                      group = "county_data",
                      opacity = 1,
                      layerId = "legend")
    })
    
}

shinyApp(ui, server)
