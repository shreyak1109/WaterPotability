library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(tidyverse)
library(dplyr)
library(scales)
library(readr)

dw <- read_csv("water_potability.csv")
View(dw)

# Scatterplot of the Hardness and Solids with pH

# This scatter plot represents the water hardness against the number of solids in different pH levels. 
# Looking at the plot, we can see that there are many points located at the center where the value of the
# Hardness is in the range 150 to 250 and the value for Solids in the range 1000 to 3000. The color of each
# point represents its pH level. The hotter the color (red, orange) the lower the pH, the more acidic the
# water is, and vice versa, the cooler the color (blue, purple), the higher the pH level, and the more
# alkaline the water is. The green range represents pH in range 6-7. Most of our points are green meaning
# most of our data points have pH in the range 6-7.

sp3<-ggplot(dw, aes(x = dw$Hardness, y = dw$Solids, color= dw$ph)) + geom_point() + 
  labs(title = "Hardness and Solids with pH", 
       subtitle = "Water Hardness and Amount of Solids in Water\n compared to pH levels.",
       x = "Hardness", y = "Solids", color = "pH")
sp3
# Gradient between n colors
sp3+scale_color_gradientn(colours = rainbow(5))


# Histogram for the Conductivity Amounts

# This is a histogram graph representing the conductivity in water that is affected by the presence of
# inorganic dissolved solids such as chloride, nitrate, sulfated, and phosphate anions. It is a measurement
# of the water. To pass the electrical current and a higher conductivity value means more chemicals
# dissolved in the water that’s the reason it is important in the water dataset. Regular drinking water
# contains 200 to 800 µS/cm which tells us the dataset we have is safe/good for drinking water.

ggplot(dw, aes(x=dw$Conductivity)) + 
  geom_histogram(color="black", fill = "mintcream", binwidth=28) + 
  labs(title = "Conductivity Amounts",
       subtitle = "How much Conductivity there is in the Water.",
       x = "Conductivity", y = "Amount")


# Pie chart of Potability Percentages

# This pie chart graph represents the percentage of potability in the dataset divided into two categories:
# 0, which means the water is not potable, and 1, which is potable. From the chart, we can see that 61% of
# the dataset showing that the water is not potable to drink, only 39% of the dataset showing that the water
# is potable for drinking. The ‘Cnt’ represents the number of data in each category. There are 1998 data
# points belonging to the not potable group (group 0) corresponding to 61%, and there are 1278 data points
# belonging to the potable group (group 1) corresponding to 39%.

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

# This graph represents the distribution of pH for different potability of water. Potability 0 means the
# water is not potable, and 1, means the water is potable. According to the graph, there is not too much
# difference in the pH distribution for the 'potable' and the 'non-potable' groups. The median of the pH is
# the same between the two groups. The box of the 'potable' group (potability = 1) is narrower, meaning the
# pH distribution of this group is more condensed than non-potable water. Other than that, the pH scale for
# the 'potable' and 'non-potable' groups is quite similar in this dataset. It is understandable since water
# potability depends not only on the pH level but also on other factors such as hardness, solids, turbidity,
# etc.

ggplot(dw, aes(x=factor(Potability), y= ph, color = factor(Potability))) + 
  geom_boxplot() +
  labs(title = "pH level in Potability and non-Potability Water",
       x = "Potability", y = "pH", color = "Potability")


# Scatterplot of pH Levels and Turbidity compared to Potability

# This scatter plot shows the pH levels compared to turbidity for each observation. Along with that, the
# color of the point shows whether or not it is potable. The blue point represents that the water is 
# potable. The peach color point represents that the water is not potable. This graph has more peach color
# points than blue which means that most of the water tested for potability is not safe to drink. The 
# higher the turbidity is the more cloudy the water is, and based on this graph most of the data points 
# average to around 4, which would mean that the water needed to be filtered before it is safe to drink. 
# For water to be safe to drink based on pH levels, it would be around 7 and most of the data observances 
# are around 7 which means the pH levels are mostly neutralized.

ggplot(dw, aes(x=ph, y=Turbidity, group=1, color = factor(Potability))) +
  geom_point()+
  labs(title = "pH vs. Turbidity for Potability and non Potability water",
        color = "Potability")