# Step 1: Create and set personal library
dir.create(path = "C:/Users/Asus/AppData/Local/R/win-library/4.4", recursive = TRUE)
.libPaths("C:/Users/Asus/AppData/Local/R/win-library/4.4")

# Step 2: Install required packages
install.packages("jsonlite")
install.packages("rlang")
install.packages("dplyr")
install.packages("tidyr")

# Step 3: Load required packages
library(jsonlite)
library(rlang)
library(dplyr)
library(tidyr)

# Step 4: Clear workspace and set options
rm(list=ls())
options(scipen=999)  # Prevent scientific notation

# Step 5: Set working directory
setwd("F:/R Wallmart project/walmart_project_in_r")

# Step 6: Read and prepare data
walmart = read.csv("Walmart_Store_sales.csv")

# Data Preparation
walmart$Store <- as.factor(walmart$Store)
walmart$Date <- as.Date(walmart$Date, format="%d-%m-%Y")
walmart$Holiday_Flag <- as.factor(walmart$Holiday_Flag)

# Print initial data info
print("Data Structure:")
str(walmart)
print("\nSummary Statistics:")
summary(walmart)

#### Analysis Questions ####

# Q1: Which store has maximum sales?
store_sales = aggregate(Weekly_Sales~Store, data=walmart, sum)
print("\nQ1: Stores ranked by total sales:")
print(arrange(store_sales, desc(Weekly_Sales)))

# Q2: Store with maximum standard deviation and coefficient of variation
store_sales$sales_mean <- aggregate(Weekly_Sales~Store, data=walmart, mean)$Weekly_Sales
store_sales$sales_sd <- aggregate(Weekly_Sales~Store, data=walmart, sd)$Weekly_Sales
store_sales$cov = store_sales$sales_sd / store_sales$sales_mean

print("\nQ2a: Stores ranked by sales standard deviation:")
print(arrange(store_sales, desc(sales_sd)))
print("\nQ2b: Stores ranked by coefficient of variation:")
print(arrange(store_sales, desc(cov)))

# Q3: Quarterly growth rate in Q3'2012
walmart_q <- walmart 
Q2_start <- as.Date("01-04-2012", "%d-%m-%Y")
Q2_end <- as.Date("30-06-2012", "%d-%m-%Y")
Q3_start <- as.Date("01-07-2012", "%d-%m-%Y")
Q3_end <- as.Date("30-09-2012", "%d-%m-%Y")

walmart_q$Quarter = ifelse(Q2_start <= walmart_q$Date & walmart_q$Date <= Q2_end, 
                          "Q2-2012", 
                          ifelse(Q3_start <= walmart_q$Date & walmart_q$Date < Q3_end,
                                 "Q3-2012", "Other"))

walmart_g <- walmart_q %>%
  group_by(Store, Quarter) %>%
  summarise(Weekly_Sales = sum(Weekly_Sales)) %>%
  ungroup() %>%
  spread(Quarter, Weekly_Sales)

walmart_g = data.frame(walmart_g)
walmart_g$growth_perct = round((walmart_g$Q3.2012 - walmart_g$Q2.2012) / walmart_g$Q2.2012 * 100, 2)

print("\nQ3: Stores ranked by Q3 2012 growth rate:")
print(arrange(walmart_g, desc(growth_perct)))

# Q4: Holiday impact analysis
SuperBowl <- as.Date(c("2010-02-12","2011-02-11","2012-02-10","2013-02-08"))
LabourDay <- as.Date(c("2010-09-10", "2011-09-09", "2012-09-07", "2013-09-06"))
Thanksgiving <- as.Date(c("2010-11-26", "2011-11-25", "2012-11-23", "2013-11-29"))
Christmas <- as.Date(c("2010-12-31", "2011-12-30", "2012-12-28", "2013-12-27"))

walmart_h <- select(walmart, Date, Weekly_Sales)
walmart_h$hflag <- ifelse(walmart_h$Date %in% SuperBowl, "SB",
                  ifelse(walmart_h$Date %in% LabourDay, "LD",
                  ifelse(walmart_h$Date %in% Thanksgiving, "TG",
                  ifelse(walmart_h$Date %in% Christmas, "CH", "None"))))

print("\nQ4: Average sales by holiday type:")
print(aggregate(Weekly_Sales~hflag, data=walmart_h, mean))

# Q5: Monthly and semester sales analysis
walmart_s_month_year = transform(walmart,
                               Year_Sale = as.numeric(format(Date, "%Y")),
                               Month_Sale = as.numeric(format(Date, "%m")))

Summarized_View = aggregate(Weekly_Sales~Month_Sale+Year_Sale, walmart_s_month_year, sum)
print("\nQ5: Monthly sales summary:")
print(arrange(Summarized_View, desc(Weekly_Sales)))

# Q6: Store 1 prediction models
walmart_store1 <- select(filter(walmart, Store==1), -1)

# Linear Model iterations
print("\nQ6: Prediction Models for Store 1")
print("\nModel 1 - All variables:")
walmart_lm = lm(Weekly_Sales ~ Holiday_Flag + Temperature + Fuel_Price + CPI + Unemployment, walmart_store1)
print(summary(walmart_lm))

print("\nModel 2 - Without Fuel_Price:")
walmart_lm1 = lm(Weekly_Sales ~ Holiday_Flag + Temperature + CPI + Unemployment, walmart_store1)
print(summary(walmart_lm1))

print("\nModel 3 - Without Unemployment:")
walmart_lm2 = lm(Weekly_Sales ~ Holiday_Flag + Temperature + CPI, walmart_store1)
print(summary(walmart_lm2))

print("\nModel 4 - Final model with significant variables:")
walmart_lm3 = lm(Weekly_Sales ~ Temperature + CPI, walmart_store1)
print(summary(walmart_lm3))

# Save results
save.image("walmart_analysis_results.RData")
print("\nAnalysis complete. Results saved to walmart_analysis_results.RData")