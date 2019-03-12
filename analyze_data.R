if (!"dplyr" %in% installed.packages()){
  install.packages("dplyr")
}
library(dplyr)

############################################################################################

# Summary statistics
feeding <- compare_boxes[which(compare_boxes$state == "feeding"), ]
resting <- compare_boxes[which(compare_boxes$state == "resting"), ]

group_by(feeding, condition) %>%
  summarise(
    count = n(),
    mean = mean(duration, na.rm = TRUE),
    sd = sd(duration, na.rm = TRUE)
  )
group_by(resting, condition) %>%
  summarise(
    count = n(),
    mean = mean(duration, na.rm = TRUE),
    sd = sd(duration, na.rm = TRUE)
  )

# One-Way ANOVA Test
feed.aov <- aov(duration ~ condition, data = feeding)
rest.aov <- aov(duration ~ condition, data = resting)

summary(feed.aov)
summary(rest.aov)

############################################################################################
# Testing for Normality and Homogeneity of Variance

# Normality: Shaprio-Wilk Test






