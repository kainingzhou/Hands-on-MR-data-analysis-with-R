#---Goal of this task------
# Move from Excel to R
# (1) Use summarize() and group_by() in R to imitate the feature of Pivot in Excel 
# (2) Store summary results as data frames for later analysis
# Useful resource: 1. R for Excel Users, https://rstudio-conf-2020.github.io/r-for-excel/
#                  2. pivots tables in R - value and label filters, https://www.youtube.com/watch?v=Qv7MTCjfQyU&ab_channel=Excel2R

#---Required packages--------
library(readxl)
library(dplyr)
library(xlsx)

#---Summary data and save results------

# 1. Load the sorted data to pivot.
pepper_mr_sorteddata<- read_excel("5_pepper_prepared.xlsx", 
                                 sheet = "sorted_data")

# Keep it as raw data in a new Excel workbook as the 1st worksheet
write.xlsx(as.data.frame(pepper_mr_sorteddata), file="6_pepper_mr_pivot.xlsx", col.names = TRUE,
           sheetName="sorted_data", append=FALSE)

# 2. Summary data group by variety and plant_no, despite window number.
pepper_mr_pivot_1 <- pepper_mr_sorteddata %>%
  group_by(variety, plant_no) %>%
  summarize(TRL1 = sum(`Length date(1)`, na.rm = TRUE), 
            TRL2 = sum(`Length date(2)`, na.rm = TRUE),
            RD1 = mean(`Diameter date(1)`, na.rm = TRUE),
            RD2 = mean(`Diameter date(2)`, na.rm = TRUE)
            )

# Replace all 0 values to NA
pepper_mr_pivot_1[pepper_mr_pivot_1 == 0] <- NA

# Save the result in the 2nd worksheet
write.xlsx(as.data.frame(pepper_mr_pivot_1), file="6_pepper_mr_pivot.xlsx", col.names = TRUE,
           sheetName="pivot_all", append=TRUE)

# 3. Summary data group by variety and plant_no, filter from window1 to window10
pepper_mr_pivot_2 <- pepper_mr_sorteddata %>% 
  filter(window_real <= 10) %>% 
  group_by(variety, plant_no) %>%
  summarize(TRL1 = sum(`Length date(1)`, na.rm = TRUE), 
            TRL2 = sum(`Length date(2)`, na.rm = TRUE),
            RD1 = mean(`Diameter date(1)`, na.rm = TRUE),
            RD2 = mean(`Diameter date(2)`, na.rm = TRUE)
            )

# Replace all 0 values to NA
pepper_mr_pivot_2[pepper_mr_pivot_2 == 0] <- NA

# Save the result in the 3rd worksheet
write.xlsx(as.data.frame(pepper_mr_pivot_2), file="6_pepper_mr_pivot.xlsx", col.names = TRUE,
           sheetName="pivot_window1to10", append=TRUE)

# 4. Summary data group by variety and plant_no, filter from window10 to window21
pepper_mr_pivot_3 <- pepper_mr_sorteddata %>% 
  filter(window_real <= 21 & window_real >10) %>% 
  group_by(variety, plant_no) %>%
  summarize(TRL1 = sum(`Length date(1)`, na.rm = TRUE), 
            TRL2 = sum(`Length date(2)`, na.rm = TRUE),
            RD1 = mean(`Diameter date(1)`, na.rm = TRUE),
            RD2 = mean(`Diameter date(2)`, na.rm = TRUE)
            )

# Replace all 0 values to NA
pepper_mr_pivot_3[pepper_mr_pivot_3 == 0] <- NA

# Save the result in the 4th worksheet
write.xlsx(as.data.frame(pepper_mr_pivot_3), file="6_pepper_mr_pivot.xlsx", col.names = TRUE,
           sheetName="pivot_window10to21", append=TRUE)


