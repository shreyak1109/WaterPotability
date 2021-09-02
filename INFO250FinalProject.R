library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(tidyverse)
library(dplyr)
library(scales)
library(readr)

dw <- read_csv("water_potability.csv")
View(dw)

# Hardness and Solids
sp3<-ggplot(dw, aes(x = dw$Hardness, y = dw$Solids, color= dw$ph)) + geom_point() + 
  labs(title = "Hardness and Solids with pH", 
       subtitle = "Water Hardness and Amount of Solids in Water\n compared to pH levels.",
       x = "Hardness", y = "Solids", color = "pH")
sp3
# Gradient between n colors
sp3+scale_color_gradientn(colours = rainbow(5))


# Conductivity
ggplot(dw, aes(x=dw$Conductivity)) + 
  geom_histogram(color="black", fill = "mintcream", binwidth=28) + 
  labs(title = "Conductivity Amounts",
       subtitle = "How much Conductivity there is in the Water.",
       x = "Conductivity", y = "Amount")


# Pie chart of Potability Percentages
percent_dw  <- dw %>% 
  count(Potability) %>%
  mutate(Potability = as.factor(Potability), 
         cumulative = cumsum(n),
         midpoint = cumulative - n / 2,
         labels = paste0(round((n/ sum(n)) * 100, 1), "%", " (Cnt: ", n, ") "),
         end_angle = 2*pi*cumsum(n)/n,      # ending angle for each pie slice
         start_angle = lag(end_angle, default = 0),   # starting angle for each pie slice
         mid_angle = 0.5*(start_angle + end_angle))


ggplot(percent_dw, aes(x = "", y = n, fill = Potability)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y", start = 0) +
  labs(title = "Percent of Potability \n",
       subtitle = "Percent of Potable Water in the dataset",
       fill = "Potability") + 
  geom_text(aes( label = labels), color="black",
            fontface = "bold", position = position_stack(vjust=0.5)) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(hjust = 0.5, face="bold", size = 10)) 


# Boxplot of pH levels and Potability
ggplot(dw, aes(x=factor(Potability), y= ph, color = factor(Potability))) + 
  geom_boxplot() +
  labs(title = "pH level in Potability and non-Potability Water",
       x = "Potability", y = "pH", color = "Potability")


# Scatterplot of pH Levels and Turbidity compared to Potability

ggplot(dw, aes(x=ph, y=Turbidity, group=1, color = factor(Potability))) +
  geom_point()+
  labs(title = "pH vs. Turbidity for Potability and non Potability water",
        color = "Potability")

