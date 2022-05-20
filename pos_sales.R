library(tidyverse)
library(dplyr)
library(broom)
library(stringr)
library(lubridate)
library(ggplot2)
library(scales)
products <- read.csv("products.csv", header = TRUE)
products <- as_tibble(products)


# renaming columns
colnames(products)[12] <- "Price"

# extracting only the essential columns
prod_cleaned <- as.tibble(products[,c("Date","Product", "SKU", "Quantity", "Price")]) ; prod_cleaned

# filtering out sanitizers
sanitizers <- prod_cleaned %>% filter(grepl('Sanitizer', Product))

# trimming the string part in quantity column
sanitizers$Quantity <- str_trim(str_replace(sanitizers$Quantity, "No\\(s\\)", ""),"both")

#CONVERT date column into date format
sanitizers$Date <- dmy(sanitizers$Date)
sanitizers %>% arrange(ymd(sanitizers$Date))
head(sanitizers)

# Convert quantity to numeric
sanitizers$Quantity <- as.numeric(sanitizers$Quantity)

# Q1 - Top 5 Products Sold During the Period
# Group by SKU and get qty by SKU

s_top5 <- sanitizers %>% group_by(SKU) %>% summarise(sum_qty=sum(Quantity))
s_top5 <-s_top5 %>% top_n(5)
ggplot(s_top5,
       aes(x = SKU,y= sum_qty))+
  geom_bar(stat = 'identity')
# Q2 - Bottom 5 Products Sold During the Period
s_top5 %>% top_n(-10)
arrange(s_top5, desc(sum_qty))


#filtering out the data only belonging to first 4 months
sales_md <- sanitizers %>% filter(Date > "2022-01-01" & Date <"2022-04-30")

# Adding month and day columns into the tibble
sales_md <- sales_md %>% mutate(month = month(sales_md$Date, label = TRUE))
sales_md <- sales_md %>% mutate(day = wday(sales_md$Date, label = TRUE, abbr = TRUE))

# Q3 - Revenue Per Month
# Calculate Revenue Per Month
sales_mom <- sales_md %>% group_by(month) %>% summarise(revenue=sum(Price)) ; sales_mom

ggplot(sales_mom,
       aes(x = month,y= revenue,fill = month))+
  geom_bar(stat = 'identity') + scale_y_continuous(labels = comma)
# Q4 - Which day has the most revenue?
sales_daily <- sales_md %>% group_by(day) %>% summarise(revenue=sum(Price)) ; sales_daily


ggplot(sales_daily,
       aes(x = day,y= revenue,fill = day))+
  geom_bar(stat = 'identity') + scale_y_continuous(labels = comma)
