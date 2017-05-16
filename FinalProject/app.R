library(shiny)
library(dplyr)
suppressPackageStartupMessages(library(googleVis))
library(shinythemes)
library(ggplot2)
library(plotly)
library(tidyr)
library(markdown)


# load project data from Github
wb.df <- read.csv("https://raw.githubusercontent.com/jtopor/CUNY-MSDA-608/master/FinalProject/DataFiles/wb2.csv", 
                  header=TRUE, sep = ",", stringsAsFactors = FALSE)

###################################################################################

ui <- shinyUI(
        navbarPage(theme = shinytheme("flatly"), 
                   "How Tuberculosis Infection Rates Relate to Specific Per Capita Metrics: 2000 - 2014",
            
            # NOTE: overview.md needs to be in current working directory or else
            # needs a fixed path !!!  
            tabPanel(
                     title = 'Overview',
                     # includeMarkdown("c:/SQLData/608/overview.md"),
                     includeMarkdown("overview.md"),
                     hr(),
                     helpText("All data provided courtesy of the World Bank: http://data.worldbank.org/indicator")
                   ), # end 1st tabPanel
            
            tabPanel("A Global Perspective",
              sidebarLayout(
                sidebarPanel(
                  radioButtons("mapMetric", "Select Metric:",
                               c("TB Infection Rate Per 100,000 People" = "tb_per100K",
                                 "Healthcare Expenditure Per Capita" = "hc_exp",
                                 "Gross National Income Per Capita" = "gni_percap",
                                 "Average Life Expectancy at Birth" = "life_exp")
                  ), # end radioButtons
                  # add a slider that lets the user select a year: default is 2014
                  sliderInput("inp_year", "Year:", animate = TRUE,
                              min=2000, max=2014, value=2014),
                  h4(textOutput("statHeading")),
                  # print R summary statistics for selected variable
                  verbatimTextOutput("summaryTable"),
                  # plot histogram for tb or curve for others
                  plotOutput("summaryPlot")
                ), # end sidebarPanel
                        
                mainPanel(
                    h3(textOutput("geoCaption")),
                    htmlOutput("heatmapPlot"),
                    verbatimTextOutput("geoSubhead"),
                    hr(),
                    helpText("All data provided courtesy of the World Bank: http://data.worldbank.org/indicator")
                ) # end mainPanel
              )#, # end sidebarLayout
              
              
              # add a slider that lets the user select a year: default is 2014
              #sliderInput("inp_year", "Year:", animate = TRUE,
              #            min=2000, max=2014, value=2014)
            ), # end tabPanel
            
    ################################################################################
            
            tabPanel("Individual Countries vs. The World",
                     # titlePanel("Compare Individual Countries vs the Rest of the World"),
                     
                     fluidRow(
                       column(6, h4("Compare Individual Countries vs the Rest of the World")),
                       column(4,
                              selectInput("c_Country", "Select Country:",
                                          choices = unique(as.character(wb.df$country)) )
                       ) # end column
                     ), # end fluidRow
                     
                     
                     # Create 2 new rows for the 4 plots
                     fluidRow(
                       column(6,
                              plotlyOutput("cPlot_1", height="350px")
                       ), # end column
                       column(6,
                              plotlyOutput("cPlot_2", height="350px")
                       ) # end column
                     ), # end fluidRow

                     hr(),
                     
                     fluidRow(
                       column(6,
                              plotlyOutput("cPlot_3", height="350px")
                       ), # end column
                       column(6,
                              plotlyOutput("cPlot_4", height="350px")
                       ) # end column
                     ), # end fluidRow
                     hr(),
                     helpText("All data provided courtesy of the World Bank: http://data.worldbank.org/indicator")
                                          
            ), # end tabPanel
            
    ################################################################################
                    
            tabPanel("The Full Data Set",
              titlePanel("World Bank Data: 2000-2014"),
              
              fluidRow(
                column(4,
                       selectInput("chCountry", "Select Country:",
                                   c("All", 
                                     unique(as.character(wb.df$country))))
                       ), # end column
                column(4,
                       selectInput("chYear", "Select Year:",
                                   c("All", 
                                     unique(as.character(wb.df$year))))
                       ) # end column
                
              ), # end fluidRow

              # Create a new row for the table.
              fluidRow(
                dataTableOutput("table")
              ),
              hr(),
              helpText("All data provided courtesy of the World Bank: http://data.worldbank.org/indicator")
              
            ) # end tabPanel
                    
        ) # end navbarPage

) # end shiny ui

###############################################################
################################################################################

server <- shinyServer(function(input, output) {

output$geoCaption <- renderText({
  
  myYear <- reactive({input$inp_year})
  mapMetric <- reactive({input$mapMetric})
  
  if(mapMetric() == 'tb_per100K') {
      paste('Tuberculosis Cases per 100,000 People: ', myYear())

  } else if (mapMetric() == 'hc_exp') {
      paste('Per Capita Healthcare Expenditures ($USD): ', myYear())
    
  } else if (mapMetric() == 'gni_percap') {
      paste('Gross National Income per Capita ($USD): ', myYear())
    
  } else if (mapMetric() == 'life_exp') {
      paste('Average Life Expectancy at Birth (in Years): ', myYear())
    
  }
  
    
}) # end geoCaption

#####################################################

output$statHeading <- renderText({
  
  myYear <- reactive({input$inp_year})
  
  paste('Summary Statistics: ', myYear())

}) # end statHeading

######################################################

output$summaryTable <- renderPrint({
  
  # options(width=40)
  
  myYear <- reactive({input$inp_year})
  mapMetric <- reactive({input$mapMetric})
  
  if(mapMetric() == 'tb_per100K') {
    yearData <- subset(wb.df, year == myYear() & !is.na(tb_per100K) )
    summary(yearData$tb_per100K)
    
  } else if (mapMetric() == 'hc_exp') {
    yearData <- subset(wb.df, year == myYear() & !is.na(hc_exp) )
    summary(yearData$hc_exp)
    
  } else if (mapMetric() == 'gni_percap') {
    yearData <- subset(wb.df, year == myYear() & !is.na(gni_percap) )
    summary(yearData$gni_percap)
    
  } else if (mapMetric() == 'life_exp') {
    yearData <- subset(wb.df, year == myYear() & !is.na(life_exp) )
    summary(yearData$life_exp)
  }
  
  
}) # end summaryTable

######################################################

# generate either a histogram (for tb variable) or scatter plot w/ curve

output$summaryPlot <- renderPlot({
  
  # options(width=40)
  
  myYear <- reactive({input$inp_year})
  mapMetric <- reactive({input$mapMetric})
  
  if(mapMetric() == 'tb_per100K') {
    
    titleStr <- paste("Histogram of TB Infection Rates: ", myYear())
    yearData <- subset(wb.df, year == myYear() & !is.na(tb_per100K) )
    
    ggplot(yearData, aes(x=tb_per100K))+
      geom_histogram(color="black", fill = "blue", bins = 30) +
      labs(title=titleStr,
           x="TB Infection Rate (per 100K people)", 
           y = "Number of Countries") +
      xlim(0, 1500) +
      ylim(0, 50) 
    
  } else if (mapMetric() == 'hc_exp') {
    
    titleStr <- paste("Per Capita HC Exp. vs TB Infection Rates: ", myYear())
    
    yearData <- subset(wb.df, year == myYear() & !is.na(hc_exp) )
    ggplot(yearData, aes(x = hc_exp, y = tb_per100K)) + 
      ggtitle(titleStr) +
      geom_point() +
      stat_smooth(method = "loess", col = "red") +
      xlab("PerCap HC Exp ($USD)") +
      ylab("TB Infection Rate per 100K People") +
      ylim(0, 1400) 
    

  } else if (mapMetric() == 'gni_percap') {
    
    titleStr <- paste("Gross National Income vs TB Infection Rates: ", myYear())
    
    yearData <- subset(wb.df, year == myYear() & !is.na(gni_percap) )
    ggplot(yearData, aes(x = gni_percap, y = tb_per100K)) + 
      ggtitle(titleStr) +
      geom_point() +
      stat_smooth(method = "loess", col = "red") +
      xlab("Gross National Income ($USD)") +
      ylab("TB Infection Rate per 100K People") +
      ylim(0, 1400) 
    
  } else if (mapMetric() == 'life_exp') {

    titleStr <- paste("Avg Life Exp. at Birth vs TB Infection Rates: ", myYear())

    yearData <- subset(wb.df, year == myYear() & !is.na(life_exp) )
    ggplot(yearData, aes(x = life_exp, y = tb_per100K)) + 
      ggtitle(titleStr) +
      geom_point() +
      stat_smooth(method = "loess", col = "red") +
      xlab("Avg Life Expectancy at Birth") +
      ylab("TB Infection Rate per 100K People") +
      ylim(0, 1400) 
  }
  
}) # end summaryTable


#######################################################

# create main chloropleth chart using gVis

#######################################################

output$heatmapPlot <- renderGvis({
  
  # add a progress bar
  progress = shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Creating image. Please wait.", value = 0)

  myYear <- reactive({input$inp_year})
  mapMetric <- reactive({input$mapMetric})
  
  if (mapMetric() == "tb_per100K") {
      # fetch only those rows for year + tb rate is not NA
      yearData <- subset(wb.df, year == myYear() & !is.na(tb_per100K) )
      
      # Calculate the rank of TB infection rates for the given year
      oScore <- order(yearData$tb_per100K)
      dat1 <- yearData[oScore,]
      dat1$tbRank <- rank(-dat1$tb_per100K, ties.method= "min")
      maxtbRank <- max(dat1$tbRank)
      
      dat1$hover <- with(dat1, paste(country, myYear(), ':', 
                                     "TB Infections Per Capita were #", tbRank, 
                                     " highest out of ", maxtbRank,
                                     " Countries" ))
                                           
      gvisGeoChart(data = dat1, 
                     locationvar = "country", 
                     colorvar = as.character(mapMetric()),
                     hovervar = "hover",
                     options = list(width=800, height=600, 
                                    colorAxis = "{values:[10, 40, 70, 100, 150, 200, 300, 500, 1400]}",
                                    colors="['blue', 'skyblue', 'cyan', 'lightcyan', 'greenyellow', 'yellow', 'orange','orangered','red']")
                     # chartid = "foo"
                     ) # end gvisGeoChart
      
  } else if (mapMetric() == "hc_exp") {
      # fetch only those rows for year + hc_exp is not NA
      yearData <- subset(wb.df, year == myYear() & !is.na(hc_exp) )
    
    
      # Calculate the rank of TB infection rates for the given year
      oScore <- order(yearData$tb_per100K)
      dat1 <- yearData[oScore,]
      dat1$tbRank <- rank(-dat1$tb_per100K, ties.method= "min")
      maxtbRank <- max(dat1$tbRank)
      
      # now add rank of hc exp per cap
      oScore <- order(yearData$hc_exp)
      dat2 <- yearData[oScore,]
      # dat1$hcRank <- rank(dat2$hc_exp, ties.method= "min")
      
      dat2$hcRank <- rank(-dat2$hc_exp, ties.method= "min")
      dat2 <- dat2[order(match(dat2$country, dat1$country)),]
      
      # dat1$gniRank <- rank(dat2$gni_percap, ties.method= "min")
      dat1$hcRank <- dat2$hcRank
      
      dat1$hover <- with(dat1, paste(country, myYear(), ':', 
                                     "Per Capita Healthcare Spending was #", hcRank, "highest,",
                                     "TB Infections Per Capita were #", tbRank, 
                                     " highest out of ", maxtbRank,
                                     " Countries" ))
      
      gvisGeoChart(data = dat1, 
                 locationvar = "country", 
                 colorvar = as.character(mapMetric()),
                 hovervar = "hover",
                 options = list(width=800, height=600, 
                                colorAxis = "{values:[50, 100, 200, 300, 500, 750, 1000, 2000, 4000, 10000]}",
                                colors="['red', 'orangered', 'orange', 'yellow', 'yellow', 'lightcyan', 'cyan', 'skyblue', 'blue', 'purple']")
                 # chartid = "foo"
      ) # end gvisGeoChart
      
  } else if (mapMetric() == "gni_percap") {
    
      # fetch only those rows for year + gni_percap is not NA
      yearData <- subset(wb.df, year == myYear() & !is.na(gni_percap) )
    
      # Calculate the rank of TB infection rates for the given year
      oScore <- order(yearData$tb_per100K)
      dat1 <- yearData[oScore,]
      dat1$tbRank <- rank(-dat1$tb_per100K, ties.method= "min")
      maxtbRank <- max(dat1$tbRank)
      
      # now add rank of GNI per capita
      oScore <- order(yearData$gni_percap)
      dat2 <- yearData[oScore,]
      dat2$gniRank <- rank(-dat2$gni_percap, ties.method= "min")
      dat2 <- dat2[order(match(dat2$country, dat1$country)),]
      
      # dat1$gniRank <- rank(dat2$gni_percap, ties.method= "min")
      dat1$gniRank <- dat2$gniRank
      
      dat1$hover <- with(dat1, paste(country, myYear(), ':', 
                                     "GNI Per Capita was #", gniRank, "highest,",
                                     "TB Infections Per Capita were #", tbRank, 
                                     " highest out of ", maxtbRank,
                                     " Countries" ))
      
      gvisGeoChart(data = dat1, 
                 locationvar = "country", 
                 colorvar = as.character(mapMetric()),
                 hovervar = "hover",
                 options = list(width=800, height=600, 
                                colorAxis = "{values:[1000, 2000, 3500, 5000, 7500, 10000, 15000, 20000, 30000, 40000, 50000, 70000]}",
                                colors="['red', 'red', 'orangered', 'orangered', 'orange', 'yellow', 'yellow', 'lightcyan', 'cyan', 'skyblue', 'blue', 'purple']")
                 # chartid = "foo"
      ) # end gvisGeoChart
    
  } else if (mapMetric() == "life_exp") {
    
      # fetch only those rows for year + life_exp is not NA
      yearData <- subset(wb.df, year == myYear() & !is.na(life_exp) )
      
      # Calculate the rank of TB infection rates for the given year
      oScore <- order(yearData$tb_per100K)
      dat1 <- yearData[oScore,]
      dat1$tbRank <- rank(-dat1$tb_per100K, ties.method= "min")
      maxtbRank <- max(dat1$tbRank)
      
      # now add rank of avg life expectancy
      oScore <- order(yearData$life_exp)
      dat2 <- yearData[oScore,]
      # dat1$leRank <- rank(dat2$life_exp, ties.method= "min")
      
      dat2$leRank <- rank(-dat2$life_exp, ties.method= "min")
      dat2 <- dat2[order(match(dat2$country, dat1$country)),]
      
      # dat1$gniRank <- rank(dat2$gni_percap, ties.method= "min")
      dat1$leRank <- dat2$leRank
      
      dat1$hover <- with(dat1, paste(country, myYear(), ':', 
                                     "Avg Life Exp. at Birth was #", leRank, "highest,",
                                     "TB Infections Per Capita were #", tbRank, 
                                     " highest out of ", maxtbRank,
                                     " Countries" ))
    
      gvisGeoChart(data = dat1, 
                 locationvar = "country", 
                 colorvar = as.character(mapMetric()),
                 hovervar = "hover",
                 options = list(width=800, height=600, 
                                colorAxis = "{values:[40, 45, 50, 55, 60, 65, 70, 75, 80, 90]}",
                                colors="['red', 'orangered', 'orange', 'yellow', 'yellow', 'lightcyan', 'cyan', 'skyblue', 'blue', 'purple']")
                                
                 # chartid = "foo"
      ) # end gvisGeoChart    
    
  } # end if else

}) # end heatmapPlot

##########################################################################
#
# This function generates the subheading that appears below the geoplot
# Each time a metric/year combo is selected, this function will ID the 
# two countries having the maximum and minimum values for the selected
# metric / year combo and then generate a text string containing that 
# info for display in the graphic
#
#######################################################################

output$geoSubhead <- renderPrint({
  
  myYear <- reactive({input$inp_year})
  mapMetric <- reactive({input$mapMetric})
  
  if(mapMetric() == 'tb_per100K') {
    # fetch only those rows for year + tb_per100K is not NA
    yearData <- subset(wb.df, year == myYear() & !is.na(tb_per100K) )
    
    # Calculate the rank of TB infection rates for the given year
    oScore <- order(yearData$tb_per100K)
    dat1 <- yearData[oScore,]
    dat1$tbRank <- rank(-dat1$tb_per100K, ties.method= "min")
    worstTBRate <- subset(dat1, tbRank == min(dat1$tbRank) )
    bestTBRate <- subset(dat1, tbRank == max(dat1$tbRank) )
    
    # format the text string to be displayed below the heatmap
    cat(worstTBRate$country, "had the highest TB Infection Rate for", myYear(), 
        "with", worstTBRate$tb_per100K, "cases per 100,000 people\n",
        bestTBRate[1,]$country, "had the lowest TB Infection Rate for", myYear(), 
        "with", bestTBRate[1,]$tb_per100K, "cases per 100,000 people")
    
  } else if (mapMetric() == 'hc_exp') {
    # fetch only those rows for year + hc_exp is not NA
    yearData <- subset(wb.df, year == myYear() & !is.na(hc_exp) )
    
    # Calculate the rank of HC expenditures for the given year
    oScore <- order(yearData$hc_exp)
    dat1 <- yearData[oScore,]
    dat1$hcRank <- rank(dat1$hc_exp, ties.method= "min")
    worstHCRate <- subset(dat1, hcRank == min(dat1$hcRank) )
    bestHCRate <- subset(dat1, hcRank == max(dat1$hcRank) )
    
    # format the text string to be displayed below the heatmap
    cat(worstHCRate$country, "had the lowest Per Capita Healthcare Spending for", myYear(), 
        "with $", worstHCRate$hc_exp, "per capita \n",
        bestHCRate[1,]$country, "had the highest Per Capita Healthcare Spending for", myYear(), 
        "with $", bestHCRate[1,]$hc_exp, "per capita")
    
  } else if (mapMetric() == 'gni_percap') {
    # fetch only those rows for year + gni_percap is not NA
    yearData <- subset(wb.df, year == myYear() & !is.na(gni_percap) )
    
    # Calculate the rank of HC expenditures for the given year
    oScore <- order(yearData$gni_percap)
    dat1 <- yearData[oScore,]
    dat1$gniRank <- rank(dat1$gni_percap, ties.method= "min")
    worstGNI <- subset(dat1, gniRank == min(dat1$gniRank) )
    bestGNI <- subset(dat1, gniRank == max(dat1$gniRank) )
    
    # format the text string to be displayed below the heatmap
    cat(worstGNI$country, "had the lowest GNI Per Capita for", myYear(), 
        "with $", worstGNI$gni_percap, "per capita \n",
        bestGNI[1,]$country, "had the highest GNI Per Capita for", myYear(), 
        "with $", bestGNI[1,]$gni_percap, "per capita")
    
  } else if (mapMetric() == 'life_exp') {
    # fetch only those rows for year + life_exp is not NA
    yearData <- subset(wb.df, year == myYear() & !is.na(life_exp) )
    
    # Calculate the rank of HC expenditures for the given year
    oScore <- order(yearData$life_exp)
    dat1 <- yearData[oScore,]
    dat1$leRank <- rank(dat1$life_exp, ties.method= "min")
    worstLE <- subset(dat1, leRank == min(dat1$leRank) )
    bestLE <- subset(dat1, leRank == max(dat1$leRank) )
    
    # format the text string to be displayed below the heatmap
    cat(worstLE$country, "had the lowest average life expectancy at birth for", myYear(), 
        "with", worstLE$life_exp, "years \n",
        bestLE[1,]$country, "had the highest average life expectancy at birth for", myYear(), 
        "with", bestLE[1,]$life_exp, "years")
    
  }
  
  
}) # end geoCaption

####################################################################
####################################################################
#
# Output for Tab Panel w/ Data Table defined below
#
####################################################################
####################################################################
 
# generate a shiny data table containing the full data set

output$table <- renderDataTable({
    data <- wb.df
    
    # Filter data based on selections
    if (input$chCountry != "All") {
      data <- data[data$country == input$chCountry,]
    }
    if (input$chYear != "All") {
      data <- data[data$year == input$chYear,]
    }

    data
})

####################################################################
####################################################################
#
# Output for Tab Panel w/ Country Specific Plots defined below
# All plots are generated using Plotly
#
####################################################################
####################################################################

# Generate a plot for the TB infection rate
output$cPlot_1 <- renderPlotly({
  
  # get data specific to the selected country
  data <- wb.df[wb.df$country == input$c_Country,]
  
  title_out <- paste(input$c_Country, "TB Infection Rates vs World")

  # get the min, max, and median for tb_per100K for each year
  vMin <- list()
  vMax <- list()
  vMed <- list()
  vAvg <- list()
  
  for (yr in 2000:2014) {
    vMin <- c(vMin, min(subset(wb.df, year == yr & !is.na(tb_per100K), select=c(tb_per100K)), na.rm = TRUE) )
    vMax <- c(vMax, max(subset(wb.df, year == yr & !is.na(tb_per100K), select=c(tb_per100K)), na.rm = TRUE) )
    vMed <- c(vMed, median(as.matrix(subset(wb.df, year == yr & !is.na(tb_per100K), select=c(tb_per100K)) )  ) )
    vAvg <- c(vAvg, colMeans(subset(wb.df, year == yr & !is.na(tb_per100K), select=c(tb_per100K)) ) )
  }
  
  # -----------------------------------------------------
  # plot min, max, median and country values for each year
  
  # need to first convert all lists to data frames
  vMin <- do.call(rbind, lapply(vMin, data.frame, stringsAsFactors=FALSE))
  vMax <- do.call(rbind, lapply(vMax, data.frame, stringsAsFactors=FALSE))
  vMed <- do.call(rbind, lapply(vMed, data.frame, stringsAsFactors=FALSE))
  vAvg <- do.call(rbind, lapply(vAvg, data.frame, stringsAsFactors=FALSE))
  
  # combine national and state metrics into a single data frame for plotting
  plot_dat <- data.frame("Country" = data$tb_per100K, 
                         "Min" = vMin, 
                         "Max" = vMax,
                         "Med" = round(vMed, 2),
                         "Avg" = round(vAvg, 2))
  colnames(plot_dat) <- c("Country", "Min", "Max", "Med", "Avg")
  
  # round avg and median vals for plotting
  plot_dat$Med <- round(plot_dat$Med, 2)
  plot_dat$Avg <- round(plot_dat$Avg, 2)
  
  # add a year column
  plot_dat$Year <- seq(2000, 2014, by = 1)
  
  pg <- plot_ly(plot_dat, x = ~Year, y = ~Country, name = "Country", type='scatter',
                mode = 'lines', line = list(color = 'black', width = 4) ) %>%
    
    add_trace(y = ~Max, name = 'World Max', line = list(color = 'red', dash = 'dot') ) %>%
    add_trace(y = ~Med, name = 'World Median', line = list(color = 'green', dash = 'dot' )) %>%
    add_trace(y = ~Avg, name = 'World Mean', line = list(color = 'purple', dash = 'dot' )) %>%
    add_trace(y = ~Min, name = 'World Min', line = list(color = 'blue', dash = 'dot')) %>%
    
    layout(title = title_out,
           yaxis = list (title = "TB Infection Rate per 100K People")) %>%
    
    config(displayModeBar = F)
  
})

##############################################

# Generate a plot for the hc_exp
output$cPlot_2 <- renderPlotly({
  
  title_out <- paste(input$c_Country, "Healthcare Exp. Per Capita vs World")
  
  # get data specific to the selected country
  data <- wb.df[wb.df$country == input$c_Country,]
  
  # get the min, max, and median for hc_exp for each year
  vMin <- list()
  vMax <- list()
  vMed <- list()
  vAvg <- list()
  
  for (yr in 2000:2014) {
    vMin <- c(vMin, min(subset(wb.df, year == yr & !is.na(hc_exp), select=c(hc_exp)), na.rm = TRUE) )
    vMax <- c(vMax, max(subset(wb.df, year == yr & !is.na(hc_exp), select=c(hc_exp)), na.rm = TRUE) )
    vMed <- c(vMed, median(as.matrix(subset(wb.df, year == yr & !is.na(hc_exp), select=c(hc_exp)) )  ) )
    vAvg <- c(vAvg, colMeans(subset(wb.df, year == yr & !is.na(hc_exp), select=c(hc_exp)) ) )
  }
  
  # -----------------------------------------------------
  # plot min, max, median and country values for each year

  # need to first convert all lists to data frames
  vMin <- do.call(rbind, lapply(vMin, data.frame, stringsAsFactors=FALSE))
  vMax <- do.call(rbind, lapply(vMax, data.frame, stringsAsFactors=FALSE))
  vMed <- do.call(rbind, lapply(vMed, data.frame, stringsAsFactors=FALSE))
  vAvg <- do.call(rbind, lapply(vAvg, data.frame, stringsAsFactors=FALSE))
  
  # combine national and state metrics into a single data frame for plotting
  plot_dat <- data.frame("Country" = data$hc_exp, 
                         "Min" = vMin, 
                         "Max" = vMax,
                         "Med" = round(vMed, 2),
                         "Avg" = round(vAvg, 2))
  colnames(plot_dat) <- c("Country", "Min", "Max", "Med", "Avg")
  
  # round avg and median vals for plotting
  plot_dat$Med <- round(plot_dat$Med, 2)
  plot_dat$Avg <- round(plot_dat$Avg, 2)
  
  # add a year column
  plot_dat$Year <- seq(2000, 2014, by = 1)
  
  pg <- plot_ly(plot_dat, x = ~Year, y = ~Country, name = "Country", type='scatter',
                mode = 'lines', line = list(color = 'black', width = 4) ) %>%
    
    add_trace(y = ~Max, name = 'World Max', line = list(color = 'red', dash = 'dot') ) %>%
    add_trace(y = ~Med, name = 'World Median', line = list(color = 'green', dash = 'dot' )) %>%
    add_trace(y = ~Avg, name = 'World Mean', line = list(color = 'purple', dash = 'dot' )) %>%
    add_trace(y = ~Min, name = 'World Min', line = list(color = 'blue', dash = 'dot')) %>%
    
    layout(title = title_out,
           yaxis = list (title = "Per Capita Healthcare Spending ($USD)")) %>%
    
    config(displayModeBar = F)

})


##################################################

# Generate a plot for the gni_percap
output$cPlot_3 <- renderPlotly({
  
  title_out <- paste(input$c_Country, "GNI Per Capita vs World")
  
  # get data specific to the selected country
  data <- wb.df[wb.df$country == input$c_Country,]
  
  # get the min, max, and median for gni_percap for each year
  vMin <- list()
  vMax <- list()
  vMed <- list()
  vAvg <- list()
  
  for (yr in 2000:2014) {
    vMin <- c(vMin, min(subset(wb.df, year == yr & !is.na(gni_percap), select=c(gni_percap)), na.rm = TRUE) )
    vMax <- c(vMax, max(subset(wb.df, year == yr & !is.na(gni_percap), select=c(gni_percap)), na.rm = TRUE) )
    vMed <- c(vMed, median(as.matrix(subset(wb.df, year == yr & !is.na(gni_percap), select=c(gni_percap)) )  ) )
    vAvg <- c(vAvg, colMeans(subset(wb.df, year == yr & !is.na(gni_percap), select=c(gni_percap)) ) )
  }
  # plot min, max, median and country values for each year
  
  # need to first convert all lists to data frames
  vMin <- do.call(rbind, lapply(vMin, data.frame, stringsAsFactors=FALSE))
  vMax <- do.call(rbind, lapply(vMax, data.frame, stringsAsFactors=FALSE))
  vMed <- do.call(rbind, lapply(vMed, data.frame, stringsAsFactors=FALSE))
  vAvg <- do.call(rbind, lapply(vAvg, data.frame, stringsAsFactors=FALSE))
  
  # combine national and state metrics into a single data frame for plotting
  plot_dat <- data.frame("Country" = data$gni_percap, 
                         "Min" = vMin, 
                         "Max" = vMax,
                         "Med" = vMed,
                         "Avg" = round(vAvg, 2))
  
  colnames(plot_dat) <- c("Country", "Min", "Max", "Med", "Avg")
  
  # round avg and median vals for plotting
  plot_dat$Med <- round(plot_dat$Med, 2)
  plot_dat$Avg <- round(plot_dat$Avg, 2)
  
  
  # add a year column
  plot_dat$Year <- seq(2000, 2014, by = 1)
  
  pg <- plot_ly(plot_dat, x = ~Year, y = ~Country, name = "Country", type='scatter',
                mode = 'lines', line = list(color = 'black', width = 4) ) %>%
    
    add_trace(y = ~Max, name = 'World Max', line = list(color = 'red', dash = 'dot') ) %>%
    add_trace(y = ~Med, name = 'World Median', line = list(color = 'green', dash = 'dot' )) %>%
    add_trace(y = ~Avg, name = 'World Mean', line = list(color = 'purple', dash = 'dot' )) %>%
    add_trace(y = ~Min, name = 'World Min', line = list(color = 'blue', dash = 'dot')) %>%
    
    layout(title = title_out,
           yaxis = list (title = "Per Capita Gross National Income ($USD)")) %>%
    
    config(displayModeBar = F)

})

#####################################################################

# Generate a plot for the life_exp
output$cPlot_4 <- renderPlotly({
  
  title_out <- paste(input$c_Country, "Avg Life Exp. at Birth vs World")
  
  # get data specific to the selected country
  data <- wb.df[wb.df$country == input$c_Country,]
  
  # get the min, max, and median for life_exp for each year
  vMin <- list()
  vMax <- list()
  vMed <- list()
  vAvg <- list()
  
  for (yr in 2000:2014) {
    vMin <- c(vMin, min(subset(wb.df, year == yr & !is.na(life_exp), select=c(life_exp)), na.rm = TRUE) )
    vMax <- c(vMax, max(subset(wb.df, year == yr & !is.na(life_exp), select=c(life_exp)), na.rm = TRUE) )
    vMed <- c(vMed, median(as.matrix(subset(wb.df, year == yr & !is.na(life_exp), select=c(life_exp)) )  ) )
    vAvg <- c(vAvg, colMeans(subset(wb.df, year == yr & !is.na(life_exp), select=c(life_exp)) ) )
  }
  # plot min, max, median and country values for each year
  
  # need to first convert all lists to data frames
  vMin <- do.call(rbind, lapply(vMin, data.frame, stringsAsFactors=FALSE))
  vMax <- do.call(rbind, lapply(vMax, data.frame, stringsAsFactors=FALSE))
  vMed <- do.call(rbind, lapply(vMed, data.frame, stringsAsFactors=FALSE))
  vAvg <- do.call(rbind, lapply(vAvg, data.frame, stringsAsFactors=FALSE))
  
  # combine national and state metrics into a single data frame for plotting
  plot_dat <- data.frame("Country" = data$life_exp, 
                         "Min" = vMin, 
                         "Max" = vMax,
                         "Med" = vMed,
                         "Avg" = vAvg)
  
  colnames(plot_dat) <- c("Country", "Min", "Max", "Med", "Avg")
  
  # round avg and median vals for plotting
  plot_dat$Med <- round(plot_dat$Med, 2)
  plot_dat$Avg <- round(plot_dat$Avg, 2)
  

  # add a year column
  plot_dat$Year <- seq(2000, 2014, by = 1)
  
  pg <- plot_ly(plot_dat, x = ~Year, y = ~Country, name = "Country", type='scatter',
          mode = 'lines', line = list(color = 'black', width = 4) ) %>%

        add_trace(y = ~Max, name = 'World Max', line = list(color = 'red', dash = 'dot') ) %>%
        add_trace(y = ~Med, name = 'World Median', line = list(color = 'green', dash = 'dot' )) %>%
        add_trace(y = ~Avg, name = 'World Mean', line = list(color = 'purple', dash = 'dot' )) %>%
        add_trace(y = ~Min, name = 'World Min', line = list(color = 'blue', dash = 'dot')) %>%
    
        layout(title = title_out,
           yaxis = list (title = "Avg Life Expectancy at Birth (Years)")) %>%
    
        config(displayModeBar = F)

})

###################################################################


}) # end shiny Server

###################################################################

shinyApp(ui, server)