# load project data from Github
wb.df <- read.csv("https://raw.githubusercontent.com/jtopor/CUNY-MSDA-608/master/FinalProject/DataFiles/wb2.csv", 
                  header=TRUE, sep = ",", stringsAsFactors = FALSE)

# get metrics for barplot

vMin <- min(subset(wb.df, year == 2014 & !is.na(tb_per100K), select=c(tb_per100K)), na.rm = TRUE)
vMax <- max(subset(wb.df, year == 2014 & !is.na(tb_per100K), select=c(tb_per100K)), na.rm = TRUE)
vMed <- median(as.matrix(subset(wb.df, 2014 == yr & !is.na(tb_per100K), select=c(tb_per100K)) )  )
vAvg <- colMeans(subset(wb.df, year == 2014 & !is.na(tb_per100K), select=c(tb_per100K)) )

can <- subset(wb.df, year == 2014 & country == "Canada", select = c(tb_per100K))
saf <- subset(wb.df, year == 2014 & country == "South Africa", select = c(tb_per100K))
bol <- subset(wb.df, year == 2014 & country == "Bolivia", select = c(tb_per100K))
ger <- subset(wb.df, year == 2014 & country == "Germany", select = c(tb_per100K))
kir <- subset(wb.df, year == 2014 & country == "Kiribati", select = c(tb_per100K))
mon <- subset(wb.df, year == 2014 & country == "Mongolia", select = c(tb_per100K))

# load into dataframe
cnames <- c("Min", "Max", "Median", "Avg", "Canada", "South Africa", "Bolivia", 
            "Germany", "Kiribati", "Mongolia")

vals <- c(vMin, vMax, vMed, round(vAvg, 2), can[[1]], saf[[1]], bol[[1]], ger[[1]], 
          kir[[1]], mon[[1]])

hw6.df <- data.frame("Metric" = cnames, "Value" = vals)

# write data frame to disk
write.csv(hw6.df, "c:/SQLData/608/FP/wbhw6.csv", row.names=FALSE)