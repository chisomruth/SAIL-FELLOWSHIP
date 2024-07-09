#load the required library and packages
library(tibble)
library(tidyr)
library(ggplot2)
library(janitor)
library(dplyr)
library(skimr)
library(readr)
library(purrr)
if(!require(pacman)) install.packages("pacman")
pacman:: p_load(
  tidyverse, #meta package
  inspectdf,
  plotly,
  janitor,
  visdat,
  esquisse
)

#load the data set into R
commerce1 <- read.csv("D:/DDrive - Document/R_Tutoria_365/nigeria_intra_Africa_trade_connectedness.csv")
View(commerce1) # view the data set in a tabular form

#explore the structure and dimensions of the dataset
dim(commerce1)
str(commerce1) # summary and glimpse shows the same view of information. use either of the two check the structure
glimpse(commerce1)
summary(commerce1) #summary(base) gives a summary information about each of the columns highlighting missing values per column
#                   and calculates the summary statistics of the numeric columns
#data shows there are 47,127 observations and 28 columns

#exploring the data set further to identify the relevant and non relevant columns.
unique(commerce1$exporting_country) #Nigeria is the only exporting country 
unique(commerce1$exporting_region)
unique(commerce1$exporting_recs)
unique(commerce1$exporting_gdp_per_capita)
unique(commerce1$exporting_gdp)
unique(commerce1$export_value)

#Having seen that the above columns provides only one data value about Nigeria, it will be removed, along with other columns that are not needed for this report
commerce_cols_cl <- commerce1 %>% 
  select(-c(exporting_country_iso,importing_country_iso, commodity_index, commodities_code,export_value,
            exporting_country,exporting_gdp,exporting_gdp_per_capita, exporting_recs,exporting_region, year))
view(commerce_cols_cl) # check irrelevant columns have been removed.

#Looking at each column and dealing with missing value. 
#Since this data is about understanding the intra trade activities of Nigeria with other countries in the continent, 
#it is better to remove rows of country where the trade value is missing and NAs in the Scale Sci.

commerce_NA_cl <- commerce_cols_cl %>% 
  filter(!is.na(trade_value) & !is.na(scaled_sci) )
skim(commerce_NA_cl) #check to see the cleaned data set
summary(commerce_NA_cl) # this shows our data is without missing value
View(commerce_NA_cl)

# rename countries with miss spelt names

unique_country_names<- unique(commerce_NA_cl$importing_country) # extract the country names to identify wrong names
unique_country_names

commerce_NA_cl <- commerce_NA_cl %>% 
  mutate(importing_country = recode(importing_country,
                          "C?te d?Ivoire" = "Côte d'Ivoire",
                          "S?o Tom? & Pr?ncipe" = "Sao Tome and Principe")
         )
commerce_NA_cl <- commerce_NA_cl %>% # 1 country did not correct,because of special character "?" , 
#using the gsub function to rename country below
  mutate(importing_country = gsub("S\\?o Tom\\? & Pr\\?ncipe", "Sao Tome and Principe", importing_country))

view(commerce_NA_cl)
unique_country_names # check that the names are now correct

#get the minimum and max trade value rowwise to see full column details about that country
commerce_NA_cl[which.min(commerce_NA_cl$trade_value),]
commerce_NA_cl[which.max(commerce_NA_cl$trade_value),]

#get the total trade value for the year
sum(trade_by_importingC$total_trade_value)

# Next is analysis of country by trade value in desc order
#Summarize the data by importing country. Use the group by function

trade_by_importingC <- commerce_NA_cl %>% 
  group_by(importing_country) %>% 
  summarise(total_trade_value = sum(trade_value)) %>% 
  arrange(desc(total_trade_value))
view(trade_by_importingC)

#get the percentage of the each country over total trade
trade_by_importingC2 <- trade_by_importingC %>% 
  mutate(percentage = round(total_trade_value/sum(total_trade_value)*100,1))
View(trade_by_importingC2)

#sum the total % share of the 10 importing countries
sum(trade_by_importingC2$percentage[1:10])

#visualize the data
plot_importingC <- ggplot(trade_by_importingC, aes(x = reorder(importing_country, total_trade_value), y = total_trade_value))+
  geom_bar(stat = "identity", fill ="steelblue") +
  coord_flip()+
  ggtitle("Total Trade Value by Importing Country")+
  xlab("Importing Country")+
  ylab("Total Trade Value")+
  theme_classic()
plot_importingC

# For clearer visual, lets look at the top 10 importing country
top_10_importing_country <- trade_by_importingC %>% 
  top_n(10, wt = total_trade_value)
view(top_10_importing_country)

#visualize the data,(this does not show the %)
#plot_top10_C <- ggplot(top_10_importing_country, aes(x = reorder(importing_country, total_trade_value), y = total_trade_value))+
 # geom_bar(stat = "identity", fill ="steelblue") +
  #coord_flip()+
  #ggtitle("Top_10 Total Trade Value by Importing Country")+
  #xlab("Importing Country")+
  #ylab("Total Trade Value")+
  #theme_classic() +
  #theme(axis.text.x = element_text(angle = 45,
   #                              hjust = 1))
#plot_top10_C needs adjustment too

# Lets add the % of trade each for each country within the top10 group
top_10_importing_country <- top_10_importing_country %>% 
  mutate(percentage = total_trade_value/sum(total_trade_value)*100)
View(top_10_importing_country)
sum(top_10_importing_country$percentage)

#visualize the data
plot_top10_C <- ggplot(top_10_importing_country, aes(x = reorder(importing_country, total_trade_value), y = total_trade_value, fill = importing_country)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 1), 
            size = 3, 
            color = "black") +
  coord_flip() +
  ggtitle("Top 10 Total Trade Value by Importing Country") +
  xlab("Importing Country") +
  ylab("Total Trade Value") +
  scale_fill_brewer(palette = "Set3") +  # Adjust color palette if needed
  theme_minimal(base_size = 10) +  # Set base font size for better readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 10, face = "bold", hjust = 1),  # Center align title
        legend.position = "right",  # Move legend to the right
        legend.title = element_blank(),  # Remove legend title
        legend.text = element_text(size = 8),  # Adjust legend text size
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))  # Add some padding around the plot

plot_top10_C

#For clearer visual, lets also get the bottom10 importing countries
bottom_10_importing_country <- trade_by_importingC %>% 
  slice_min(order_by = total_trade_value, n = 10)
view(bottom_10_importing_country)

# Lets add the % of trade each for each country within the bottom10 group
bottom_10_ICbypercent <- bottom_10_importing_country %>% 
  mutate(percentage = round(total_trade_value/sum(total_trade_value)*100,1))
view(bottom_10_ICbypercent)

#visualize the bottom_10 importing countries
plot_bottom10_C <- ggplot(bottom_10_ICbypercent, aes(x = reorder(importing_country, total_trade_value), y = total_trade_value, fill = importing_country)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 1), 
            size = 3, 
            color = "black") +
  coord_flip() +
  ggtitle("Bottom_10 Trade Value by Importing Country") +
  xlab("Importing Country") +
  ylab("Total Trade Value") +
  #scale_fill_brewer(palette = "Set3") +  # Adjust color palette if needed
  theme_minimal(base_size = 10) +  # Set base font size for better readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 10, face = "bold", hjust = 1),  # Center align title
        legend.position = "right",  # Move legend to the right
        legend.title = element_blank(),  # Remove legend title
        legend.text = element_text(size = 8),  # Adjust legend text size
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))  # Add some padding around the plot

plot_bottom10_C

#Trade value vs distance to capital
TV_by_distcap <- ggplot(commerce_NA_cl, aes(x = distcap, y = trade_value)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm")+
  ggtitle("Trade Value vs Distance to Capital") + 
  labs(x= "Distance to Capital", y ="Trade Value")+
  theme_classic()
  #There is negative correlation here. The longer the distance, the lesser the trade value except for SA
TV_by_distcap # this represent distance to the country's capital

#Trade value vs distance to country
TV_by_dist <- ggplot(commerce_NA_cl, aes(x = dist, y = trade_value)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm")+
  ggtitle("Trade Value vs Distance To Country") + 
  labs(x= "Distance to Country", y ="Trade Value")+
  theme_gray()
#There is negative correlation here. The longer the distance, the lesser the trade value except for SA
TV_by_dist # this represent distance to the country, not necessarily the capital


# Impact of common language on trade value
TV_by_ComLag <- ggplot(commerce_NA_cl, aes(x = factor(comlang_off), fill= trade_value)) + 
  geom_bar() + ggtitle("Trade Value vs Common Language") +
  xlab("Common Language (0 = No, 1 = Yes)") + ylab("Trade Value")+
  theme_light()
 TV_by_ComLag # this reveals that common language does not affect Nigeria's trading relation with other countries within 
 #the region exported to

view(top_10_importing_data)

# Impact of colonial ties on trade value
TV_by_ColMaster <- ggplot(commerce_NA_cl, aes(x = factor(comcol), y = trade_value)) +
  geom_boxplot() + ggtitle("Trade Value vs Colonial Ties") + 
  xlab("Colonial Ties (0 = No, 1 = Yes)") + ylab("Trade Value")+
  theme_bw()
TV_by_ColMaster # this reveals that colonial master ties does not impact Nigeria's 
                #trading relation with other countries that were not colonized by same master

#Economic Indicators
#Analyze the relationship between trade values and economic indicators like GDP and GDP per capita.

# Trade value vs importing GDP
TV_by_GDP <- ggplot(commerce_NA_cl, aes(x = importing_gdp, y = trade_value)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm") +
  ggtitle("Trade Value vs Importing Country's GDP") + 
  xlab("Importing GDP") + ylab("Trade Value")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 10, face = "bold", hjust = 1))
  
TV_by_GDP # this shows a positive correlation btw the trade value and the countries GDP. 
#The higher the GDP, the higher the value of export from Nigeria

# Trade value vs importing GDP per capita
TV_by_GDPerCapita <- ggplot(commerce_NA_cl, aes(x = importing_gdp_per_capita, y = trade_value)) + 
  geom_point(alpha = 0.5) + geom_smooth(method = "lm") + 
  ggtitle("Trade Value vs Importing GDP per Capita") +
  xlab("Importing GDP per Capita") + ylab("Trade Value")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 10, face = "bold", hjust = 1))
TV_by_GDPerCapita


# Trade values by importing region
trade_by_importing_region <- commerce_NA_cl %>% 
  group_by(importing_region) %>% 
  summarize(total_trade_value = sum(trade_value))
view(trade_by_importing_region)

#get the % of the share of the region 
trade_by_importing_region <- trade_by_importing_region %>% 
  mutate(percentage = round(total_trade_value/sum(total_trade_value)*100,1))
view(trade_by_importing_region)

#visualize the data
TV_by_ImportingR <- ggplot(trade_by_importing_region, aes(x = reorder(importing_region, total_trade_value), y = total_trade_value, fill = importing_region)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 1), 
            size = 3, 
            color = "black") +
  coord_flip() +
  ggtitle("Total Trade Value by Importing Region") +
  xlab("Importing Region") +
  ylab("Total Trade Value") +
  scale_fill_brewer(palette = "Set3") +  # Adjust color palette if needed
  theme_minimal(base_size = 10) +  # Set base font size for better readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 10, face = "bold", hjust = 1),  # Center align title
        legend.position = "right",  # Move legend to the right
        legend.title = element_blank(),  # Remove legend title
        legend.text = element_text(size = 8),  # Adjust legend text size
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))  # Add some padding around the plot

TV_by_ImportingR

# Analyse trade by the importing RECs
#summarize by importing recs
trade_by_importing_recs <- commerce_NA_cl %>% 
  group_by(importing_recs) %>% 
  summarize(total_trade_value = sum(trade_value))
view(trade_by_importing_recs)

#get the % share of each recs over total trade value
Importing_recsPerc <- trade_by_importing_recs %>% 
  mutate(percentage = round(total_trade_value/sum(total_trade_value)*100,1)) %>% 
arrange(desc(percentage))
view(Importing_recsPerc)

#get top recs for, slice and sum
sum(Importing_recsPerc$percentage[1:4]) # the top 4 recs makes up 99.5% of the trade. 
top_4_importing_recs <- Importing_recsPerc %>% 
  top_n(4, wt = total_trade_value)
View(top_4_importing_recs)

#visualize the data
  TV_by_ImportingRec <- ggplot(top_4_importing_recs, aes(x = reorder(importing_recs, total_trade_value), y = total_trade_value, fill = importing_recs)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(round(percentage, 1), "%")), 
              position = position_stack(vjust = 1), 
              size = 3, 
              color = "black") +
    coord_flip() +
    ggtitle("Total Trade Value by Importing Recs") +
    xlab("Importing Recs") +
    ylab("Total Trade Value") +
    scale_fill_brewer(palette = "Set3") +  # Adjust color palette if needed
    theme_minimal(base_size = 10) +  # Set base font size for better readability
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 10, face = "bold", hjust = 1),  # Center align title
          legend.position = "right",  # Move legend to the right
          legend.title = element_blank(),  # Remove legend title
          legend.text = element_text(size = 8),  # Adjust legend text size
          plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))  # Add some padding around the plot
  
TV_by_ImportingRec
