# R server script for shiny app for HW3, Problem #2

library(ggplot2)

# load data from github
m_df <- read.csv("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture3/data/cleaned-cdc-mortality-1999-2010-2.csv", header = TRUE, stringsAsFactors = FALSE)

# find cause of death 2010 for all states
DR_2010 <- subset(m_df, Year == 2010)

##########################################################################
# Calc a national crude.rate for each type of cause of death for each year
###########

library(tidyr, warn.conflicts = FALSE, quietly=TRUE)
library(dplyr, warn.conflicts = FALSE, quietly=TRUE)

# get distinct causes for 2010
causes <- as.data.frame(unique(DR_2010$ICD.Chapter), stringsAsFactors = FALSE)
colnames(causes) <- 'cause'

# remove pregnancy cause due to large amounts of missing data
causes <- subset(causes, cause != "Pregnancy, childbirth and the puerperium")

# start by building a dataframe of National statistics for year/cause of death
nat_df <- data.frame(summarise(group_by(m_df, Year, ICD.Chapter), 
                               Deaths = sum(as.numeric(Deaths)),
                               Population = sum(as.numeric(Population)),
                               Crude.Rate = round(Deaths/Population * 100000, 3) ) )

# now clean up instances where national population figures vary within a single year
# due to missing / incomplete data in data set for some causes of death
for(i in 1999:2010) {
  # find max population for year
  yr_cl <- subset(nat_df, Year == i)
  maxpop <- max(yr_cl$Population)
  
  # now update all rows having year == i to maxpop
  nat_df <- transform(nat_df, Population = ifelse(Year == i, maxpop, Population))
  
  # update all crude rates due to adjustments in national population figures
  nat_df$Crude.Rate <- round(nat_df$Deaths / nat_df$Population * 100000, 3)
}

##########################################################################
# Calc % improvement in national rate 
# NOTE: improvement rate can't be calculated for 1999 due to no earlier data
# so 1999 data is excluded from the resulting "nat_imp" data frame
####

nat_imp <- data.frame(Year = numeric(0), Cause = character(0), Change = numeric(0), stringsAsFactors = FALSE)

for (yr in 2000:2010) {
  for (i in 1:nrow(causes)) {
    old_rate <- subset(nat_df, ICD.Chapter == causes$cause[i] & Year == yr-1, select=c(Crude.Rate))
    colnames(old_rate) <- "Change"
    new_rate <- subset(nat_df, ICD.Chapter == causes$cause[i] & Year == yr, select=c(Crude.Rate))
    colnames(new_rate) <- "Change"
    if(nrow(old_rate) > 0 & nrow(new_rate) > 0) {
      nat_imp <- rbind(nat_imp, data.frame(Year = yr, Cause = causes$cause[i], 
                                           Change = round((new_rate - old_rate)/old_rate, 3)))
    } # end if old_rate
  } # end for i
} # end for yr

nat_imp$Change = nat_imp$Change * 100

# create a stub data frame to use of state rate of change data
state_imp <- data.frame(Year = numeric(0), Change = numeric(0), stringsAsFactors = FALSE)

##########################################################################
# now define required SHINY function for processing input + plotting
##########################################################################

function(input, output) {
  
  # fetch required national data for display
  natl_dat <- reactive({ natl_dat <- subset(nat_imp, Cause == input$cause) })
  
  # Define weighted average % change curve plot
  output$MortImpPlot <- renderPlot({
    
    # fetch required state data for display
    state_dat <- subset(m_df, ICD.Chapter == input$cause & State == input$state, 
                        select=c(Year, Crude.Rate))
    
    # Calc % improvement for state / cause compared to previous year for yrs 2000 - 2010
    for (yr in 2000:2010) {
      old_rate <- subset(state_dat, Year == yr - 1, select=c(Crude.Rate))
      colnames(old_rate) <- "Change"
          
      new_rate <- subset(state_dat, Year == yr, select=c(Crude.Rate))
      colnames(new_rate) <- "Change"
          
      if(nrow(old_rate) > 0 & nrow(new_rate) > 0) {
        state_imp <- rbind(state_imp, data.frame(Year = yr, 
                                                     Change = round((new_rate - old_rate)/old_rate, 4)))
      } # end if old_rate
      
    } # end for yr

    # mult change amt by 100 to get percentage rate of change
    state_imp$Change = state_imp$Change * 100
    
    # combine national and state metrics into a single data frame for plotting
    plot_dat <- data.frame("State_Rate" = state_imp$Change, "National_Avg" = natl_dat()$Change)
    
    # generate the sequence to be used as limits + tick marks along x axis
    x = seq(2000, 2010, by = 1)
    
    ggplot(plot_dat, aes(x = x) ) + 
      # plot a weighted average for the % change in state mortality rate
      geom_smooth(aes(y = State_Rate, col = "State_Rate"), se = FALSE, lty = 1) +

      # plot a weighted average for the % change in national mortality rate
      geom_smooth(aes(y = National_Avg, col = "National_Avg"), se = FALSE, lty = 1) +
      
      geom_hline(yintercept=0, linetype="dashed") +
      # ylim(-p_max, p_max) +
      
      theme(panel.grid.major.y = element_blank(), axis.title=element_text(size=15,face="bold"), 
        axis.text.y = element_text(size = 13, face = "bold"), 
        plot.title = element_text(size=16, face = "bold"),
        axis.text.x = element_text(size = 13, face = "bold")) +
      
      scale_x_continuous(breaks= seq(2000, 2010, by=1), expand = c(0, 0.2)) +
      ylab(label = "Weighted Moving Average % Change in Mortality Rate") +
      xlab(label = "Year") +
      scale_colour_manual(name = "Legend", values=c("State_Rate" = "blue", "National_Avg" = "red")) +
      ggtitle(input$cause)
      
  }) # end renderPlot
  
  # print msg regaring exclusion of 3 COD's due to large amounts of missing data
  output$stats <- renderPrint({
    print('NOTE: Analyses for the following causes of death are not available due to large quantities of incomplete data')
    print('   - Diseases of the ear and mastoid process,')
    print('   - Pregnancy, childbirth and the puerperium,')
    print('   - Codes for special purposes')
  })
  
}