# R Server script for shiny app 608 HW3 Question 1

library(ggplot2)

# load data from Github
m_df <- read.csv("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture3/data/cleaned-cdc-mortality-1999-2010-2.csv", header = TRUE, stringsAsFactors = FALSE)

# find cause of death 2010 for all states
DR_2010 <- subset(m_df, Year == 2010)

# select distinct causes of death from data frame
causes <- as.data.frame(unique(DR_2010$ICD.Chapter))
colnames(causes) <- 'cause'

##############################

function(input, output) {
  
  # fetch required data for display
  crude <- reactive({ crude <- subset(DR_2010, ICD.Chapter == input$cause)   })
  
  # Define Cleveland plot
  output$causePlot <- renderPlot({
    
    # display graphic of company counts by state in descending order
    ggplot(crude(), aes(x= Crude.Rate, y= reorder(State, Crude.Rate))) +
      scale_x_continuous(limits=c(0, max(crude()$Crude.Rate) + 4), expand = c(0, 0)) +
      geom_segment(aes(yend=State), xend=0, colour="grey50", size = 1.1) +
      geom_point(size=4, colour = "firebrick") +
      theme_bw() +
      theme(panel.grid.major.y = element_blank(), axis.title=element_text(size=14,face="bold"), 
            axis.text.y = element_text(size = 11, face = "bold"), 
            plot.title = element_text(size=16, face = "bold")) +
      xlab("2010 Crude Mortality Rate") +
      ylab("State") +
      ggtitle(input$cause)
  }) # end renderPlot
  
  # print summary stats of crude rate below plot
  output$stats <- renderPrint({
    print('Summary Statistics')
    summary(crude()$Crude.Rate)
  })
}