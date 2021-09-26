#---Goal of this task------
# Statistical analysis in R
# Note: this analysis follows the R script: 2_pepper_mr_2021_pivot. 
# Need to run previous script in order to execute this one

#---Tutorial------

# 1. Remove outliers: 
# https://stackoverflow.com/questions/66411056/r-how-to-remove-outliers-from-dataset-by-two-different-groups

# 2. When two samples are paired and normally distributed: Paired Samples T-test
# http://www.sthda.com/english/wiki/paired-samples-t-test-in-r

# 3. # When two samples are unpaired but normally distributed: Two samples T-test in R:
# http://www.sthda.com/english/wiki/unpaired-two-samples-t-test-in-r

# 4. When two samples are paired but not normally distributed: Paired Samples Wilcoxon Test in R
# http://www.sthda.com/english/wiki/paired-samples-wilcoxon-test-in-r

# 5. When two samples are unpaired and not normally distributed: 
# http://www.sthda.com/english/wiki/unpaired-two-samples-wilcoxon-test-in-r



#---Required packages--------
library(readxl)
library(dplyr)
library(xlsx)
library(rstatix) # check outliers
library(ggplot2)
library(ggpubr)
library(export) # for exporting the plots

#---1st Session------

# 1. Visualize data using box plots
ggboxplot(pepper_mr_pivot_1, x = "variety", y = "TRL1", 
          fill = "variety", palette = c("#00AFBB", "#E7B800"),
          ylab = "TRL1", xlab = "Variety")

ggboxplot(pepper_mr_pivot_1, x = "variety", y = "RD1", 
          fill = "variety", palette = c("#00AFBB", "#E7B800"),
          ylab = "RD1", xlab = "Variety")

# 2. Outliers
# 2.1 Detect outliers
TRL1_outliers <- pepper_mr_pivot_1 %>% 
  group_by(variety) %>% 
  identify_outliers("TRL1") 

print(TRL1_outliers)

RD1_outliers <- pepper_mr_pivot_1 %>% 
  group_by(variety) %>% 
  identify_outliers("RD1") 

print(RD1_outliers)

# 2.2 Remove outliers
S1_valid <- pepper_mr_pivot_1 %>% 
  anti_join(TRL1_outliers, by = "plant_no") %>% 
  anti_join(RD1_outliers, by = "plant_no")

# 3. Visualize data without outliers
ggboxplot(S1_valid, x = "variety", y = "TRL1", 
          fill = "variety", palette = c("#00AFBB", "#E7B800"),
          ylab = "Total root length (mm)", xlab = "Variety")

ggboxplot(S1_valid, x = "variety", y = "RD1", 
          fill = "variety", palette = c("#00AFBB", "#E7B800"),
          ylab = "Root diameter (mm)", xlab = "Variety")

# 4. Preleminary test to check independent t-test assumptions
## 4.1 Test independence of two samples (whether two samples are not related)

## 4.2 Test normal distribution of two samples ( Shapiro-Wilk test)
### 4.2.1 Shapiro-Wilk normality test for Canon's and Galliano's TRL
with(S1_valid, shapiro.test(TRL1[variety == "Canon"]))
with(S1_valid, shapiro.test(TRL1[variety == "Galliano"])) 

### 4.2.2 Shapiro-Wilk normality test for Canon's and Galliano's RD
with(S1_valid, shapiro.test(RD1[variety == "Canon"]))
with(S1_valid, shapiro.test(RD1[variety == "Galliano"])) 

## 4.3 Test homogeneity in variances (F-test)
### 4.3.1 F-test test for TRL
with(S1_valid, var.test(TRL1 ~ variety))
### 4.3.2 F-test test for RD
with(S1_valid, var.test(RD1 ~ variety))

# 5. Compute unpaired two-samples t-test since assumptions for t-test are satisfied
## 5.1 two-samples t-test for TRL
TRL1_t <- t.test(TRL1 ~ variety, data = S1_valid, var.equal = TRUE)
TRL1_t

## 5.2 two-samples t-test for RD
RD1_t <- t.test(RD1 ~ variety, data = S1_valid, var.equal = TRUE)
RD1_t

# 6. Plot graphs with Significance 
## 6.1 Plot of TRL
TRL1_p <- ggbarplot(S1_valid, x = "variety", y = "TRL1",add = "mean_se",
          fill = "variety", palette = "jco")+
  labs(caption = "\n20 days after transplanting", x = "Variety", y="Total root length (mm)\n")+ 
  # add a line break (\n) before x axis, and after y axis labels to increase the spacing between axis title and text
  coord_cartesian(ylim = c(0,90))+
  theme_classic()+
  theme(legend.title = element_text(face = "bold", colour = "black", size = 12),
        legend.text = element_text(face = "bold", colour = "black", size = 12), 
        plot.caption = element_text(size = 12, color = "red"),
        axis.title.x = element_text(face = "bold", colour = "black", size = 12),   
        axis.text.x = element_text(face = "bold", colour = "black", size = 12),
        axis.title.y = element_text(face = "bold", colour = "black", size = 12),   
        axis.text.y = element_text(face = "bold", colour = "black", size = 12)) +
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "Canon", label.y = 65, hide.ns = TRUE) # Pairwise comparison against reference
## 6.2 Plot of RD
RD1_p <- ggbarplot(S1_valid, x = "variety", y = "RD1",add = "mean_se",
          fill = "variety", palette = "jco")+
  labs(caption = "\n20 days after transplanting", x = "Variety", y="Root diameter (mm)\n")+ 
  # add a line break (\n) before x axis, and after y axis labels to increase the spacing between axis title and text
  theme_classic()+
  theme(legend.title = element_text(face = "bold", colour = "black", size = 12),
        legend.text = element_text(face = "bold", colour = "black", size = 12), 
        plot.caption = element_text(size = 12, color = "red"),
        axis.title.x = element_text(face = "bold", colour = "black", size = 12),   
        axis.text.x = element_text(face = "bold", colour = "black", size = 12),
        axis.title.y = element_text(face = "bold", colour = "black", size = 12),   
        axis.text.y = element_text(face = "bold", colour = "black", size = 12)) +
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "Canon", label.y = 0.5, hide.ns = TRUE)  # Pairwise comparison against reference

#---2nd Session------

# 1. Visualize data using box plots
ggboxplot(pepper_mr_pivot_1, x = "variety", y = "TRL2", 
          fill = "variety", palette = c("#00AFBB", "#E7B800"),
          ylab = "TRL2", xlab = "Variety")

ggboxplot(pepper_mr_pivot_1, x = "variety", y = "RD2", 
          fill = "variety", palette = c("#00AFBB", "#E7B800"),
          ylab = "RD2", xlab = "Variety")

# 2. Outliers
# 2.1 Detect outliers
TRL2_outliers <- pepper_mr_pivot_1 %>% 
  group_by(variety) %>% 
  identify_outliers("TRL2") 

print(TRL2_outliers)

RD2_outliers <- pepper_mr_pivot_1 %>% 
  group_by(variety) %>% 
  identify_outliers("RD2") 

print(RD2_outliers)

# 2.2 Remove outliers
S2_valid <- pepper_mr_pivot_1 %>% 
  anti_join(TRL2_outliers, by = "plant_no") %>% 
  anti_join(RD2_outliers, by = "plant_no")

# 3. Visualize data without outliers
ggboxplot(S2_valid, x = "variety", y = "TRL2", 
          fill = "variety", palette = c("#00AFBB", "#E7B800"),
          ylab = "Total root length (mm)", xlab = "Variety")

ggboxplot(S2_valid, x = "variety", y = "RD2", 
          fill = "variety", palette = c("#00AFBB", "#E7B800"),
          ylab = "Root diameter (mm)", xlab = "Variety")

# 4. Preleminary test to check independent t-test assumptions
## 4.1 Test independence of two samples (whether two samples are not related)

## 4.2 Test normal distribution of two samples ( Shapiro-Wilk test)
### 4.2.1 Shapiro-Wilk normality test for Canon's and Galliano's TRL
with(S2_valid, shapiro.test(TRL2[variety == "Canon"])) #  p-value = 0.01479
with(S2_valid, shapiro.test(TRL2[variety == "Galliano"])) 

### 4.2.2 Shapiro-Wilk normality test for Canon's and Galliano's RD
with(S2_valid, shapiro.test(RD2[variety == "Canon"]))
with(S2_valid, shapiro.test(RD2[variety == "Galliano"])) 

## 4.3 Test homogeneity in variances (F-test)
### 4.3.1 F-test test for TRL
with(S2_valid, var.test(TRL2 ~ variety))
### 4.3.2 F-test test for RD
with(S2_valid, var.test(RD2 ~ variety))

# 5. Compute unpaired Two-Samples Wilcoxon Test in R since assumptions for t-test are violated for TRL 
## 5.1 two-samples t-test for TRL
TRL2_t <- wilcox.test(TRL2 ~ variety, data = S2_valid, var.equal = TRUE) # p-value = 0.02612
TRL2_t

## 5.2 two-samples t-test for RD
RD2_t <- t.test(RD2 ~ variety, data = S2_valid, var.equal = TRUE) #  p-value = 0.6703
RD2_t

# 6. Plot graphs with Significance 
## 6.1 Plot of TRL
TRL2_p <- ggbarplot(S2_valid, x = "variety", y = "TRL2",add = "mean_se",
          fill = "variety", palette = "jco")+
  labs(caption = "\n30 days after transplanting", x = "Variety", y="Total root length (mm)\n")+ 
  # add a line break (\n) before x axis, and after y axis labels to increase the spacing between axis title and text
  coord_cartesian(ylim = c(0,90))+
  theme_classic()+
  theme(legend.title = element_text(face = "bold", colour = "black", size = 12),
        legend.text = element_text(face = "bold", colour = "black", size = 12), 
        plot.caption = element_text(size = 12, color = "red"),
        axis.title.x = element_text(face = "bold", colour = "black", size = 12),   
        axis.text.x = element_text(face = "bold", colour = "black", size = 12),
        axis.title.y = element_text(face = "bold", colour = "black", size = 12),   
        axis.text.y = element_text(face = "bold", colour = "black", size = 12)) +
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "Canon", label.y = 90, hide.ns = TRUE)  # Pairwise comparison against reference

## 6.2 Plot of RD
RD2_p <- ggbarplot(S2_valid, x = "variety", y = "RD2",add = "mean_se",
          fill = "variety", palette = "jco")+
  labs(caption = "\n30 days after transplanting", x = "Variety", y="Root diameter (mm)\n")+ 
  # add a line break (\n) before x axis, and after y axis labels to increase the spacing between axis title and text
  theme_classic()+
  theme(legend.title = element_text(face = "bold", colour = "black", size = 12),
        legend.text = element_text(face = "bold", colour = "black", size = 12), 
        plot.caption = element_text(size = 12, color = "red"),
        axis.title.x = element_text(face = "bold", colour = "black", size = 12),   
        axis.text.x = element_text(face = "bold", colour = "black", size = 12),
        axis.title.y = element_text(face = "bold", colour = "black", size = 12),   
        axis.text.y = element_text(face = "bold", colour = "black", size = 12)) +
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "Canon", label.y = 0.5, hide.ns = TRUE)  # Pairwise comparison against reference

ggarrange(TRL2_p, RD2_p,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)
# 7. Export graph
ggarrange(TRL1_p, TRL2_p,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)
graph2ppt(file="Pepper_MR_Figures.pptx", width=10, height=6, append = FALSE)

ggarrange(RD1_p, RD2_p,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)
graph2ppt(file="Pepper_MR_Figures.pptx", width=10, height=6, append = TRUE)
