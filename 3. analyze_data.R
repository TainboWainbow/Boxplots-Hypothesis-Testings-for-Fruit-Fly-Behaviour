if (!"dplyr" %in% installed.packages()){
  install.packages("dplyr")
}
if (!"car" %in% installed.packages()){
  install.packages("car")
}
library(car)

############################################################################################
# Summary statistics
feeding <- compare_boxes[which(compare_boxes$state == "feeding"), ]
resting <- compare_boxes[which(compare_boxes$state == "resting"), ]
feeding$condition <- as.factor(feeding$condition)
resting$condition <- as.factor(resting$condition)

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

############################################################################################
# Testing for Normality and Homogeneity of Variance

# Normality: Shaprio-Wilk Test
residuals(feed.aov) %>% shapiro.test()
residuals(rest.aov) %>% shapiro.test()

# Homogeneity: Levene's Test
leveneTest(duration ~ condition, data = feeding)
leveneTest(duration ~ condition, data = resting)

# Non-parametric Alternative for ANOVA: Kruskal-Wallis Rank Sum Test
kruskal.test(duration ~ condition, data = feeding)
kruskal.test(duration ~ condition, data = resting)

# One-Way ANOVA Test gives different values but 
# also rejects H0 for the feeding intervals data
feed.aov <- aov(duration ~ condition, data = feeding)
rest.aov <- aov(duration ~ condition, data = resting)

summary(feed.aov)
summary(rest.aov)
