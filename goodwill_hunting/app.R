#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# options(scipen = 999) prevents numbers from appearing in scientific notation
knitr::opts_chunk$set(options(scipen = 999)) 


library(shiny)
library(shinythemes)
library(tidyverse)
library(gt)
library(janitor)
library(leaflet)
library(plotly)
library(gt)
library(sf)

# loading data (please fckn work holy shit)

good <- read.csv("goodwill_regions.csv") %>%
    drop_na() # they were in there and causin problems so...snip snip

zip_latlon <- read.csv("US Zip Codes from 2013 Government Data") %>%
    clean_names()

zillow <- read.csv("zillow_regions.csv")

goodwill_ratings <- read.csv("goodwill_ratings.csv")




# Define UI for application 
ui <- fluidPage(
    
    # set project title
    navbarPage("Goodwill Hunting",
               
               # TAB: MAP
               tabPanel(("Goodwill Map"),         
                        # Show a plot of the generated distribution
                        fillPage(h2("Goodwill Map"),
                                 tags$div("It is well known among secondhand shoppers that thrift stores located in wealthier neighborhoods tend to yield better
                                          finds. Use this tool to find Goodwill locations in neighborhoods with higher home values (a strong proxy for income),
                                          and explore the relationship between housing price and store reviews."),
                                 br(),
                                 tags$div("Input your zip code to view Goodwill locations in your area. Goodwill locations on the map are colored by 
                                          the quartile of the average home value in their zip code."),
                                 br(),
                                 textInput("zip_input", "Input Zip Code"),
                                 selectInput("q_input", "Select Home Value Quartile", choices = c("All",
                                                                                                     "q1",
                                                                                                     "q2",
                                                                                                     "q3",
                                                                                                     "q4")),
                                 leafletOutput("goodwill_map", height=500),
                                 tags$div("*Note that zip codes beginning with zero are not available to search"))),                                  
               
               
               # TAB: STATE BY STATE
               
               tabPanel("Search By State",
                        
                        mainPanel(h2("Search by State"),
                                  br(),
                                  tags$div("Search for a state to view a list of Goodwill locations ranked by the average home value in their 
                                           zip code. Please use a capitalized, two letter state code (e.g. CA for California)"),
                                  br(),
                                  textInput("state_input3", "Input State Code"),
                                  gt_output("by_state"))),
               
               
               # TAB: PLOTS
               tabPanel("Analysis",
                        h1("Analysis"),
                        tabsetPanel(
                            tabPanel("Yelp Analysis",
                                     mainPanel(h3("Relationship Between Zip Code Home Value and Yelp Rating"),
                                     textInput("state_input_yelp", "Input State Code", value = "USA"),
                                               plotlyOutput("yelp_plot")),
                                     sidebarPanel(h3("Model"), 
                                                  gt_output("yelp_gt"),
                                                  br(),
                                                  h3("About"),
                                                  br(),
                                                  tags$div("Since Yelp does not allow scraping of its site, data on location ratings was collected by manually inputting
                                                           star ratings of 500 randomly sampled locations. Those that did not have star ratings or were not listed on the Yelp
                                                           site were omitted. An analysis of this sample did not reveal a statistically significant relationship between average 
                                                           zip code home value and Yelp rating; the p-value was well in excess of 0.05. The given correlation coefficient was 
                                                           positive but very small."),
                                                  br(),
                                                  tags$div("However, it is worth considering the following before entirely discounting the relationship between neighborhood income and location rating.
                                                            The largest aggregate variation in home value happens mostly *between* regions, not within them. For instance, 
                                                           since home values (and living costs overall) are very high in Los Angeles, all of its Goodwill locations are in zip codes
                                                           in the highest home value quartile. Meanwhile, western Pennsylvania's locations are almost all in the bottom two. Shoppers in
                                                           each region aren't comparing stores to others nationwide; they're comparing stores to others in their area. Perhaps an analysis
                                                           grouped by city or region would reveal a stronger relationship."))
                                     
                                     ),
                            tabPanel("Home Value",
                                     
                                     fillPage( h3("Comparing Goodwill Neighborhood Home Values to Nationwide Neighborhood Home Values"),
                                         sliderInput("x_good", "Home Value Range", min = 0, max = 3000000, step = 10000, value = 300000),
                                         selectInput("region_input", "Select Region", choices = c("United States", 
                                                                                                  "South",
                                                                                                  "Midwest",
                                                                                                  "West",
                                                                                                  "Northeast")), 
                                         fluidRow(
                                             splitLayout(cellWidths = c("50%", "50%"), 
                                                         plotlyOutput("goodwills_by_housing_price"),
                                                         plotlyOutput("overall_housing_price"))),
                                         br(),
                                         fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                             gt_output("goodwill_gt"), gt_output("zillow_gt")))
                                         
                                     )))),
               
               # TAB: ABOUT
               tabPanel(("About"),
                        h2("About This Project"),
                        tags$div("It is a well-known fact in the thrifting community that the bougier the neighborhood, the badder the donations, the better the finds. In other words, since most donation-based thrift stores like Goodwill and the Salvation Army source from the immediate surrounding areas, it logically follows that wealthier neighborhoods will tend to have nicer items. This tool allows users to optimize their thrift store selections by identifying which Goodwills near them are in neighborhoods with higher average real estate prices, which tends to correspond to household income. The *Goodwill Map* and *Search by State tabs are two ways to do this. Originally, the intention was to build a search function that pulled all of the locations within a certain radius of an inputted zip code, and sort those locations by average zip code home value. I was able to build a proof-of-concept in an Rmd file, but have yet to figure out how to transfer it to a Shiny App."),
                        br(),
                        tags$div("This project also explores the relationship between a location's neighborhood income and its Yelp rating. Since Yelp does not allow scraping of its site, data on location ratings was collected by manually inputting star ratings of 500 randomly sampled locations. Those that did not have star ratings or were not listed on the Yelp site were omitted. An analysis of this sample did not reveal a statistically significant relationship between average zip code home value and Yelp rating, but it is still possible that there is a more significant relationship when examining locations within certain regions, rather than nationwide where for various reasons the distribution may appear more random (more detail in the *Analysis* tab"),
                        h2("Data Sources"),
                        tags$div("Data on average zip code home values was sourced from ",a("Zillow", href = "https://www.zillow.com/research/data/"),". Note that for all zip codes that began with zero, the Zillow dataset dropped the first digit which complicated the process of joining data sets by the zip column."),
                        br(),
                        tags$div("Goodwill location data was scraped from ",a("the Goodwill Store Locator", href = "https://www.goodwill.org/locator/"),". Note that 500 locations are omitted from this project. There actually does exist a cleaned, premade ",a("data set", href = "https://www.aggdata.com/aggdata/complete-list-goodwill-industries-locations")," of all Goodwill locations in the US, including their neighborhood, zip code, and store type. It is $99. So that's a no."),
                        br(),
                        tags$div("I also used a list of zip code coordinates from ",a("opendatasoft", href = "https://public.opendatasoft.com/explore/dataset/us-zip-code-latitude-and-longitude/export/")," and a list of states by US Census region from a lovely guy called Chris Halpert's ",a("GitHub", href = "https://github.com/cphalpert/census-regions/blob/master/us%20census%20bureau%20regions%20and%20divisions.csv"),"."),
                        br(),
                        tags$div("Finally, data on Yelp reviews was manually pulled from ",a("Yelp.com", href = "yelp.com"),"."),
                        br(),
                        h2("About Me"),
                        tags$div("My name is Ella Michaels and I am a sophomore at Harvard University. This is my final project for Gov 1005.
                                 I study Government and am an avid thrift shopper; I pretty much have not purchased a new item of clothing since 2016 (which admittedly is more a reflection of my shallow bank balance more than my deep commitment to sustainability).
                                 My GitHub account is linked ",a("here", href = "https://github.com/ellamichaels"),"."))))






# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    # color factor
    
    q_color <- colorFactor(pal = c("#1b9e77", "#d95f02", "#7570b3", "coral1"), 
                           domain = c("q1", "q3", "q2", "q4"))
    
    
    
    # plot all goodwill locations with leaflet
    output$goodwill_map <- renderLeaflet({
        
        zlat <- zip_latlon %>%
            filter(zip == input$zip_input) %>%
            pull(lat)
        
        zlon <- zip_latlon %>%
            filter(zip == input$zip_input) %>%
            pull(lng)
        
        if(input$q_input == "All") {
            
            good <- good
            
        }
        
        else {   
            good <- good %>%
                filter(quartile == input$q_input)
        }
        
        if(input$zip_input == "") {
            (leaflet(good) %>% 
                 addCircles(lng = ~lon, lat = ~lat) %>% 
                 addTiles() %>%
                 addCircleMarkers(data = good, lat =  ~lat, lng = ~lon, 
                                  radius = 1, color = ~q_color(quartile), opacity = .9) %>%
                 addLegend(pal = q_color, values=good$quartile, opacity=1) %>%
                 
                 # set zoom to continental USA (temporary)
                 setView(lat = 39.8283
                         , lng = -98.5795 # coordinates for the middle of the country according to google
                         , zoom = 4 )
            )}
        
        else {(
            
            leaflet(good) %>% 
                addTiles() %>%
                addCircleMarkers(data = good, lat =  ~lat, lng = ~lon, 
                                 radius = 1, opacity = .9, color = ~q_color(quartile)) %>%
                addLegend(pal = q_color, values=good$quartile, opacity=1) %>%
                
                # set zoom to zip code
                setView(lat = zlat
                        , lng = zlon # coordinates for the middle of the country according to google
                        , zoom = 12 )
        )}
    })
    
    # Round 2 motherfuckers
    
    output$goodwill_map2 <- renderLeaflet({
        
        zlat2 <- zip_latlon %>%
            filter(zip == input$zip_input2) %>%
            pull(lat)
        
        zlon2 <- zip_latlon %>%
            filter(zip == input$zip_input2) %>%
            pull(lng)
        
        if(input$zip_input2 == "") {
            (
                
                leaflet(good) %>% 
                    addTiles() %>%
                    addCircleMarkers(data = good, lat =  ~lat, lng = ~lon, 
                                     radius = 1, color = ~q_color(quartile), opacity = .9) %>%
                    addLegend(pal = q_color, values=good$quartile, opacity=1) %>%
                    
                    # set zoom to continental USA (temporary)
                    setView(lat = 39.8283
                            , lng = -98.5795 # coordinates for the middle of the country according to google
                            , zoom = 4 )
            )}
        
        else {(
            
            leaflet(good) %>% 
                addCircles(lng = ~lon, lat = ~lat) %>% 
                addTiles() %>%
                addCircleMarkers(data = good, lat =  ~lat, lng = ~lon, 
                                 radius = 1, color = ~q_color(quartile)) %>%
                addLegend(pal = q_color, values=good$quartile, opacity=1) %>%
                
                # set zoom to zip code
                setView(lat = zlat2
                        , lng = zlon2 # coordinates for the middle of the country according to google
                        , zoom = 12 )
        )}
    })
    
    # this search function is gonna fckn kill me 
    
    zlat3 <- reactive(zip_latlon %>%
                          filter(zip == input$zip_input2) %>%
                          pull(lat))
    
    zlon3 <- reactive(zip_latlon %>%
                          filter(zip == input$zip_input2) %>%
                          pull(lng))
    
    point <- reactive(data_frame(mylon = zlon3, mylat = zlat3))
    
    point_sf <- reactive(st_as_sf(point, coords = c("mylon", "mylat"), crs = 4326))
    
    target <- reactive(good %>%
                           filter(state == input$state_input2) %>%
                           select(lat, lon) %>%
                           drop_na())
    
    target_sf <- reactive(st_as_sf(target, coords = c("lon", "lat"), crs = 4326))
    
    target_sf2 <- reactive(target_sf %>%
                               mutate(Dist = as.numeric(st_distance(point_sf, target_sf, by_element = TRUE))))
    
    target_sf2 <- reactive(target_sf2 %>%
                               # Filter the records with Dist <= 10 miles in meters
                               filter(Dist <= 16093.4))
    
    output$goodwill_search <- renderDataTable({
        
        expr = target_sf2
        
    })
    
    
    # State by State
    
    output$by_state <- render_gt(
        
        if(input$state_input3 == "") {
            
            good_gt <- good %>%
                arrange(desc(x2020_01)) %>%
                select(address, state, city, zip, x2020_01) %>%
                slice(1:15)
            
            good_gt <- tibble::rowid_to_column(good_gt, "Rank")

            good_gt %>%
                gt() %>%
                cols_label(address = "Address",
                           state = "State",
                           city = "City",
                           zip = "Zip Code",
                           x2020_01 = "Average House Price in Zip Code")
            
        }
        
        else {
            good_gt <- good %>%
                filter(state == input$state_input3) %>%
                arrange(desc(x2020_01)) %>%
                select(address, state, city, zip, x2020_01)
            
            good_gt <- tibble::rowid_to_column(good_gt, "Rank")
            
            good_gt %>%
                gt() %>%
                cols_label(address = "Address",
                           state = "State",
                           city = "City",
                           zip = "Zip Code",
                           x2020_01 = "Average House Price in Zip Code")
    })
    
    
    # lets try this plotting thing shall we
    
    output$goodwills_by_housing_price <- renderPlotly({
        if(input$region_input == "United States") {
            ggplotly(good %>%
                         ggplot(aes(x = x2020_01, fill = region)) +
                         geom_bar(binwidth = 20000) +
                         theme_classic() +
                         xlim(0, input$x_good) +
                         labs(x = "Housing Price in Location Zipcode",
                              y = "Number of Locations by Housing Price",
                              title = "Distribution of Goodwill Locations \nby Zip Code Housing Price",
                              fill = "Region"))
            
        }
        
        else {
            ggplotly(good %>%
                         filter(region == input$region_input) %>%
                         ggplot(aes(x = x2020_01)) +
                         geom_bar(binwidth = 20000, fill = "darkturquoise") +
                         theme_classic() +
                         xlim(0, input$x_good) +
                         labs(x = "Housing Price in Location Zipcode",
                              y = "Number of Locations by Housing Price",
                              title = "Distribution of Goodwill Locations \nby Zip Code Housing Price",
                              subtitle = paste("In the", input$region_input, sep = " ")))
        }
    })
    
    # overall housing price plot
    
    output$overall_housing_price <- renderPlotly({
        
        if(input$region_input == "United States") {
            ggplotly(zillow %>%
                         ggplot(aes(x = x2020_01, fill = region)) +
                         geom_bar(binwidth = 20000) +
                         theme_classic() +
                         xlim(0, input$x_good) +
                         labs(x = "Housing Price",
                              y = "Number of Zipcodes",
                              title = "Distribution of Zip Codes by Housing Price",
                              subtitle = paste("In the", input$region_input, sep = " ")))
        }
        
        else {
            ggplotly(zillow %>%
                         filter(region == input$region_input) %>%
                         ggplot(aes(x = x2020_01)) +
                         geom_bar(binwidth = 20000, fill = "coral1") +
                         theme_classic() +
                         xlim(0, input$x_good) +
                         labs(x = "Housing Price",
                              y = "Number of Zipcodes",
                              title = "Distribution of Zip Codes by Housing Price",
                              subtitle = paste("In the", input$region_input, sep = " ")))
            
        }
    })
    
    # Summary statistics for overall zipcode housing price average
    
    output$zillow_gt <- render_gt(
        zillow %>%
            summarize(min = min(x2020_01),
                      mean = mean(x2020_01),
                      q1 = quantile(x2020_01, c(.25)),
                      median = median(x2020_01),
                      q3 = quantile(x2020_01, c(.75)),
                      max = max(x2020_01)) %>%
            gt() %>%
            cols_label(min = "Minimum",
                       mean = "Average",
                       q1 = "First Quartile",
                       median = "Median",
                       q3 = "Third Quartile",
                       max = "Maximum") %>%
            tab_header(title = "Summary Statistics: Nationwide")
    )
    
    # Summary statistics for goodwill zipcode housing price average
    
    output$goodwill_gt <- render_gt(
        
        good %>%
            summarize(min = min(x2020_01),
                      mean = mean(x2020_01),
                      q1 = quantile(x2020_01, c(.25)),
                      median = median(x2020_01),
                      q3 = quantile(x2020_01, c(.75)),
                      max = max(x2020_01)) %>%
            gt() %>%
            cols_label(min = "Minimum",
                       mean = "Average",
                       q1 = "First Quartile",
                       median = "Median",
                       q3 = "Third Quartile",
                       max = "Maximum") %>%
            tab_header(title = "Summary Statistics: Goodwills")
    )
    
    
    # Yelp plots
    
    output$yelp_plot <- renderPlotly({
        
        if(input$state_input_yelp == "" | input$state_input_yelp == "USA") {
            
            goodwill_ratings %>%
                drop_na(x2020_01, rating) %>%
                ggplot(aes(y = rating, x = x2020_01, color = region)) +
                xlim(0,1000000) +
                geom_point() +
                geom_smooth(method = "glm", level = 0, aes(group = 1), color = "black") +
                theme_classic() +
                labs(x = "Average Housing Price in Store Zip Code",
                     y = "Yelp Star Rating",
                     color = "Region")
            
        }
        
        else {
            
            goodwill_ratings %>%
                drop_na(x2020_01, rating) %>%
                filter(state == input$state_input_yelp) %>%
                ggplot(aes(y = rating, x = x2020_01)) +
                xlim(0,1000000) +
                geom_point(color = "blue") +
                geom_smooth(method = "glm", level = 0, aes(group = 1), color = "black") +
                theme_classic() +
                labs(x = "Average Housing Price in Store Zip Code",
                     y = "Yelp Star Rating",
                     title = "Relationship Between Neighborhood Housing Price and Yelp Star Rating",
                     color = "Region")
        }
        
    })
    
    # Yelp gt tables
    
    output$yelp_gt <- render_gt( {
        
        good_ratings_yelp <- goodwill_ratings %>%
            rename(zip_price = x2020_01)
        
        ratings_model <- lm(rating ~ zip_price, data = good_ratings_yelp)
        
        ratings_model %>%
            tidy(conf.int = TRUE) %>%
            mutate(p.value = round(p.value, 2)) %>%
            select(term, estimate, std.error, conf.low, conf.high, p.value) %>%
            gt() %>%
            fmt_scientific(columns = vars(estimate, std.error, conf.low, conf.high)) %>%
            cols_label(term = "Variable", estimate = "Estimate", std.error = "Standard Error", 
                       conf.low = "Lower Bound", conf.high = "Upper Bound", p.value = "p-value") %>%
            tab_header(title = "Effect of Housing Price on Store Review")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
