### Overview

The World Health Organization (WHO) states that tuberculosis (TB) is one of the 
leading bacterial-based causes of death worldwide. It is one of the top 5 
causes of death among women aged 15-44, and in 2014 alone killed approximately 
1.5 million people throughout the world. Furthermore, the WHO states that 
"..about one-third of the world's population has latent TB, which means 
people have been infected by TB bacteria but are not (yet) ill with the disease and 
cannot transmit the disease", and that "..over 95% of TB deaths occur in low- and 
middle-income countries".  The visualizations contained within this web app allow the 
user to explore the relationship between TB infection rates and three other country-
specific "per capita" metrics for countries throughout the world as defined by the 
World Bank's __World Development Indicators__ (WDI).

TB infection rates as quantified by WDI measure the rate of TB infections per 100,000 
people for a given country for the (2000 - 2014) time period. The three additional WDI 
per capita metrics available for analysis are:

- Healthcare Expenditures per Capita (in $USD);

- Gross National Income (GNI) per Capita (in $USD);

- Average Life Expectancy at Birth (in years).

### Visualizations

This web application is comprised of three interactive panels, each of which makes use of a
different primary data visualization technique:

- __A Global Perspective__ allows the user to generate a global chloropleth, summary statistics,
and a related data plot for any of the four WDI variables for any year of the 
(2000 - 2014) time period. Hovering the cursor over a specific country will enable the display 
of additional variable-specific and TB infection rate-specific rankings relative to the 
other countries represented within the underlying data. For the healthcare expenditure, GNI, 
and life expectancy variables, the related data plot will be a scatterplot showing the relationship 
of the variable to TB infection rates for the selected year.

- __Individual Countries vs. The World__ makes use of interactive line plots to visualize the 
time series changes in each of the four WDI variables for a user-selected country relative to the 
worldwide means, medians, minimums, and maximums of each WDI variable for the (2000-2014) 
time period. These visualizations provide the user with an intuitive method of evaluating the 
relative standing of any selected country vs. the rest of the world for each WDI variable.

- __The Full Data Set__ allows the user to explore the data set used throughout this web
application in its entirety via an interactive, searchable data table.


