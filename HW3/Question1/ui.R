# R UI script for shiny app 608 HW3 Question 1

# load data from Github
m_df <- read.csv("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture3/data/cleaned-cdc-mortality-1999-2010-2.csv", header = TRUE, stringsAsFactors = FALSE)

# find cause of death 2010 for all states
DR_2010 <- subset(m_df, Year == 2010)

# select distinct causes of death from data frame
causes <- unique(DR_2010$ICD.Chapter)

# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel("Crude Mortality Rates by State: 2010"),
  
  # define dropdown
  fluidRow(      
    
    # Define the sidebar with one input
    column(9, offset = 0,
           selectInput("cause", "Select Cause of Death:", choices= causes, width = '100%')
    ) # ebd fluidRow
    
  ), # end fluidPage
  
  plotOutput("causePlot", height = 700, width = 700),
  verbatimTextOutput("stats"),
  
  hr(),
  helpText("Data provided by CDC Wonder: https://wonder.cdc.gov/ucd-icd10.html")
)