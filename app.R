library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)

#Data formatting
#cData <- read.csv("COVIDSummaryData.csv")
cData <-readRDS("COVIDSummaryData.rds")
cData <- cData %>%
  filter(County != "Grand Total", Sex != "Total", Onset.Date != "") %>%
  mutate(gDates = mdy(Onset.Date))

cData <- cData %>%
  #mutate(Onset.Date = mdy(Onset.Date)) %>%
  mutate(Case.Count = as.numeric(Case.Count)) %>%
  mutate(Death.Due.to.Illness.Count = as.numeric(Death.Due.to.Illness.Count)) %>%
  mutate(Hospitalized.Count = as.numeric(Hospitalized.Count)) 
  
# cData$Case.Count <- as.numeric(as.character(cData$Case.Count))
# cData$Death.Due.to.Illness.Count <-
#   as.numeric(as.character(cData$Death.Due.to.Illness.Count))
# cData$Hospitalized.Count <-
#   as.numeric(as.character(cData$Hospitalized.Count))

countyNames <- unique(cData$County)
countyNames <- c(countyNames)

#Groups the data for all tab plots except choro
groupedData <- cData %>%
  group_by(County, gDates, Age.Range) %>%
  summarise(
    Cases = sum(Case.Count),
    Deaths = sum(Death.Due.to.Illness.Count),
    Hospitalizations = sum(Hospitalized.Count)
  )


#Choropleth Data formatting
#countyPop <- readxl::read_excel("co-est2019-annres-39.xlsx")
countyPop <-readRDS("co-est2019-annres-39.rds", refhook = NULL)

map.county <- map_data('county')  # turns maps pkg data into DF
ohio.county <- subset(map.county, region == "ohio")

countyPop <- countyPop[order(countyPop$County),]
countyPop <- countyPop %>%
  mutate(lCounty = str_to_lower(County))


covid <- cData %>%
  filter(County != "Grand Total") %>%
  mutate(Onset.Date = mdy(Onset.Date)) %>%
  mutate(Case.Count = as.numeric(Case.Count)) %>%
  mutate(lCounty = str_to_lower(County)) %>%
  group_by(lCounty) %>%
  summarize(
    Cases = sum(Case.Count),
    Hospitalized = sum(Hospitalized.Count),
    Deaths = sum(Death.Due.to.Illness.Count)
  )

covid <- covid %>%
  mutate(countyPop["Population"])

newCounty <- str_to_title(covid$lCounty)

#https://www.ohio-demographics.com/counties_by_population
formatMap <- merge(ohio.county, covid,
                   by.x = "subregion",
                   by.y = "lCounty")

formatMap <- formatMap %>%
  mutate(
    CasePercent = (Cases / Population) * 1000,
    HospPercent = (Hospitalized / Population) * 1000,
    DeathsPercent = (Deaths / Population) * 1000
  )


ui <- navbarPage(
  #Tab 1
  "Dynamic Project",
  tabPanel("Ohio Choropleth",
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 inputId = "respvar1",
                 "Select a Variable",
                 choices = c("Cases", "Deaths", "Hospitalizations")
               )
             ),
             mainPanel(plotlyOutput(
               'ohioChoro', height = 550, width = 650
             ))
           )),
  #Tab 2
  tabPanel(
    "Covid Cases by County",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "selectCounty2",
          "Select a County",
          choices = c(newCounty)
        ),
        selectInput(
          inputId = "selectResponse2",
          "Select a Variable",
          choices = c("Cases", "Deaths", "Hospitalizations"),
          selected = "Cases"
        ),
        checkboxInput(inputId = "tab2Options", label = "Moving Average"),
      ),
      mainPanel(plotOutput("barplot"))
    )
  ),
  #Tab 3 
  tabPanel("Compare Counties",
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 inputId = "selectCounty3",
                 "Select a County",
                 choices = c(newCounty),
                 multiple = TRUE,
                 selected = "Adams"
               ),
               selectInput(
                 inputId = "selectResponse3",
                 "Select a Variable",
                 choices = c("Cases", "Deaths", "Hospitalizations"),
                 selected = "Cases"
               ),
               
             ),
             mainPanel(plotOutput("compareCounty3"))
           )),
  #Tab 4
  tabPanel(
    "Covid in Respect to Age",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "selectCounty4",
          "Select a County",
          choices = c(newCounty)
        ),
        selectInput(
          inputId = "selectResponse4",
          "Select a Variable",
          choices = c("Cases", "Deaths", "Hospitalizations"),
          selected = "Cases"
        ),
        sliderInput(
          "datetab4",
          "Select Date Range:",
          min = min(mdy(cData$Onset.Date)),
          max = max(mdy(cData$Onset.Date)),
          value = mdy("11/05/2020")
        ),
      ),
      mainPanel(plotOutput("agePlot"))
    )
  ),
  #Tab 5
  # tabPanel(
  #   "Cases greater than >",
  #   textInput(
  #     "numberChoice5",
  #     "Enter the amount of Covid Cases to see high risk counties",
  #     "1000"
  #   ),
  #   mainPanel(
  #     plotOutput("populationPlot", height = 600),
  #     p("Note that TRUE and FALSE change frequently on the bottom")
  #   )
  # ),
  #Tab 6
  tabPanel(
    "Credit",
    titlePanel("Joseph Odhiambo"),
    
    mainPanel(
      h5("12/04/2020"),
      h3("Packages Used"),
      p(
        " Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2020). shiny: Web Application Framework for
  R. R package version 1.5.0. https://CRAN.R-project.org/package=shiny"
      ),
      p(
        "Wickham et al., (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686,
  https://doi.org/10.21105/joss.01686"
      ),
      p(
        "Garrett Grolemund, Hadley Wickham (2011). Dates and Times Made Easy with lubridate. Journal of Statistical
  Software, 40(3), 1-25. URL http://www.jstatsoft.org/v40/i03/."
      ),
      p(
        "C. Sievert. Interactive Web-Based Data Visualization with R, plotly, and shiny. Chapman and Hall/CRC Florida, 2020."
      ),
      h3("Datasets"),
      p("Covid Data from Ohio Department of Health"),
      p(
        "https://coronavirus.ohio.gov/wps/portal/gov/covid-19/dashboards/overview"
      ),
      p("Ohio Demogrpahics County Population"),
      p("https://www.ohio-demographics.com/counties_by_population")
    ),
    
  )
)


server <- function(input, output) {

  #Tab5
  output$populationPlot <- renderPlot({
    formatMap %>%
      ggplot(aes(x = subregion, y = (Cases > input$numberChoice5))) +
      labs(y = input$numberChoice5) +
      geom_tile() +
      coord_flip()
    
  })
  
  
  #Tab 2
  #Same plot, selectInput box time lagged and didn't always work
  output$barplot <- renderPlot({
    p1 <- groupedData %>%
      filter(County == input$selectCounty2) %>%
      ggplot(aes(x = gDates, y = Cases))  +
      geom_col() +
      theme_classic()
    
    
    p2 <- groupedData %>%
      filter(County == input$selectCounty2) %>%
      ggplot(aes(x = gDates, y = Deaths))  +
      geom_col() +
      theme_classic()
    
    p3 <- groupedData %>%
      filter(County == input$selectCounty2) %>%
      ggplot(aes(x = gDates, y = Hospitalizations))  +
      geom_col() +
      theme_classic()
    
    if (input$tab2Options) {
      p1 <- groupedData %>%
        filter(County == input$selectCounty2) %>%
        ggplot(aes(x = gDates, y = Cases))  +
        geom_col() +
        theme_classic() +
        tidyquant::geom_ma(n = 7,
                           color = "red",
                           size = .8)
      
      p2 <- groupedData %>%
        filter(County == input$selectCounty2) %>%
        ggplot(aes(x = gDates, y = Deaths))  +
        geom_col() +
        theme_classic() +
        tidyquant::geom_ma(n = 7,
                           color = "red",
                           size = .8)
      
      p3 <- groupedData %>%
        filter(County == input$selectCounty2) %>%
        ggplot(aes(x = gDates, y = Hospitalizations))  +
        geom_col() +
        theme_classic() +
        tidyquant::geom_ma(n = 7,
                           color = "red",
                           size = .8)
    }
    
    if (input$selectResponse2 == "Cases") {
      p1
    } else if (input$selectResponse2 == "Deaths") {
      p2
    } else if (input$selectResponse2 == "Hospitalizations") {
      p3
    } else {
      #Not needed but fail safe
      p1
    }
    
    
  })
  
  #Tab 1
  output$ohioChoro <- renderPlotly ({
    ohioplot <-
      ggplot(formatMap,
             aes(
               x = long,
               y = lat,
               group = subregion,
               fill = CasePercent,
               label = Cases
             )) +
      scale_fill_gradient2(
        low = "black",
        mid = "grey90",
        high = "red",
        midpoint = median(formatMap$CasePercent)
      ) +
      geom_polygon(color = "white") +
      coord_map("polyconic") +
      labs(title = "Covid Cases per 1000 People",
           fill = "Cases \n Percent",
           y = "") +
      theme_void()
    
    if (input$respvar1 == "Cases") {
      ohioplot
    } else if (input$respvar1 == "Hospitalizations") {
      ggplot(
        formatMap,
        aes(
          x = long,
          y = lat,
          group = subregion,
          fill = HospPercent,
          label = Hospitalized
        )
      ) +
        scale_fill_gradient2(
          low = "black",
          mid = "grey90",
          high = "red",
          midpoint = median(formatMap$HospPercent)
        ) +
        geom_polygon(color = "white") +
        coord_map("polyconic") +
        labs(title = "Covid Hospitalizations Per 1000 People", fill = "Hospitalization \n Percent") +
        theme_void()
      
    } else if (input$respvar1 == "Deaths") {
      ggplot(
        formatMap,
        aes(
          x = long,
          y = lat,
          group = subregion,
          fill = DeathsPercent,
          label = Deaths
        )
      ) +
        scale_fill_gradient2(
          low = "black",
          mid = "grey90",
          high = "red",
          midpoint = median(formatMap$DeathsPercent)
        ) +
        geom_polygon(color = "white") +
        coord_map("polyconic") +
        labs(title = "Covid Deaths Per 1000 People", fill = "Death \n Percent") +
        theme_void()
      
    } else {
      #Fail Safe
      ohioplot
    }
    
  })
  
  #Tab 4
  output$agePlot <- renderPlot({
    #Facet grid instead of wrap do to date problems
    p1 <- groupedData %>%
      filter(County == input$selectCounty4) %>%
      filter(gDates < input$datetab4) %>%
      ggplot(aes(x = gDates, y = Cases))  +
      geom_col() +
      facet_grid(~ Age.Range) +
      labs(x = "Date") +
      theme_classic()
    
    
    p2 <- groupedData %>%
      filter(County == input$selectCounty4) %>%
      ggplot(aes(x = gDates, y = Deaths))  +
      geom_col() +
      facet_grid(~ Age.Range) +
      labs(x = "Date") +
      theme_classic()
    
    p3 <- groupedData %>%
      filter(County == input$selectCounty4) %>%
      ggplot(aes(x = gDates, y = Hospitalizations))  +
      geom_col() +
      facet_grid(~ Age.Range) +
      labs(x = "Date") +
      theme_classic()
    
    
    if (input$selectResponse4 == "Cases") {
      p1
    } else if (input$selectResponse4 == "Deaths") {
      p2
    } else if (input$selectResponse4 == "Hospitalizations") {
      p3
    } else {
      #Not needed but fail safe
      p1
    }
    
    
  })
  
  
  #Tab3
  output$compareCounty3 <- renderPlot ({
    countyList3 <- c(input$selectCounty3)
    p1 <- groupedData %>%
      filter(County %in% countyList3) %>%
      ggplot(aes(x = gDates, y = Cases))  +
      geom_col() +
      facet_grid(~ County) +
      labs(x = "Date") +
      theme_classic()
    
    p2 <- groupedData %>%
      filter(County %in% countyList3) %>%
      ggplot(aes(x = gDates, y = Deaths))  +
      geom_col() +
      facet_grid(~ County) +
      labs(x = "Date") +
      theme_classic()
    
    p3 <- groupedData %>%
      filter(County %in% countyList3) %>%
      ggplot(aes(x = gDates, y = Hospitalizations))  +
      geom_col() +
      facet_grid(~ County) +
      labs(x = "Date") +
      theme_classic()
    
    if (input$selectResponse3 == "Cases") {
      p1
    } else if (input$selectResponse3 == "Deaths") {
      p2
    } else if (input$selectResponse3 == "Hospitalizations") {
      p3
    } else {
      #Not needed but fail safe
      p1
    }
  })
  
  
}
shinyApp (ui = ui, server = server)