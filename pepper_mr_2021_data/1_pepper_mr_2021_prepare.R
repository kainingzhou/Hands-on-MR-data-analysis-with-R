#---Goal of this task------
# Processing the raw Minirhizotron data for later analysis. Data manipulation includes:
# (1) Add columns of treatment_id and window_real
# (2) Select useful columns and store them as a new data frame

#---Required packages--------
library(readxl)
library(dplyr)
library(xlsx)

#---- Prepare the data --------

# Load the raw data generated from Rootfly analysis
pepper <- read_excel("2_pepper_mr_2021.xlsx")

# Load the data of treatment id                                                                         
pepper_tr_id <- read_excel("3_pepper_treatmentid.xlsx")

# Load the data of soil surface id   
pepper_su_id <- read_excel("4_pepper_surfaceid.xlsx")

# Merge raw data with both treatment id and soil surface id  
pepper_prepare_1 <- merge(pepper,pepper_tr_id,by=c("plant_no"),all.x=TRUE) %>% 
                    merge(pepper_su_id,by=c("plant_no"),all.x=TRUE)

# Pass on merged data to a new data frame
pepper_prepare_2 <- pepper_prepare_1

# Created a new column called "window_real", value equals to "window" minus "surface_no", then add 1
pepper_prepare_2$window_real <- pepper_prepare_2$Window - pepper_prepare_2$surface_no + 1

# Pass previous data to a new data frame
pepper_prepare_3 <- pepper_prepare_2

# Select useful columns by names, then store them in the data frame
pepper_prepare_3 <- pepper_prepare_3 %>% select(
                                                plant_no, 
                                                treatment, variety, fertilizer, 
                                                window_real,
                                                'Length date(1)', 'Diameter date(1)',
                                                'Length date(2)', 'Diameter date(2)'
                                                )

#---Write the prepared data in a new excel workbook------

## Write the raw data with id in a new workbook
write.xlsx(pepper_prepare_2, file="5_pepper_prepared.xlsx",
           sheetName="raw_data", append=FALSE)
## Add the selected columns in a new worksheet of the same workbook. Name it as "sorted data"
write.xlsx(pepper_prepare_3, file="5_pepper_prepared.xlsx", sheetName="sorted_data", 
           append=TRUE)

