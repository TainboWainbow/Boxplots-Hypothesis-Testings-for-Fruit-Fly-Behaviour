if ("ggplot2" %in% installed.packages()){
  install.packages("ggplot2")
}
if (!"car" %in% installed.packages()){
  install.packages("car")
}
library(ggplot2)
library(car)

#######################################################################################################################

# Creating data.frame for an individual set of observation
final_resting <- data.frame("duration" = final_resting, "state" = c(rep("resting", times = length(final_resting))))
final_feeding <- data.frame("duration" = final_feeding, "state" = c(rep("feeding", times = length(final_feeding))))
final <- rbind(final_resting, final_feeding)

final$"condition" <- c(rep("fed", times = nrow(final)))
final$"date" <- c(rep("07/27", times = nrow(final)))
final$"fly" <- c(rep(6, times = nrow(final)))


# Collect all final results of all sets of  observation into one data frame called 'compare_boxes'
compare_boxes <- data.frame()
compare_boxes <- rbind(compare_boxes, final)


# Once all data collected, prepare to create boxplots
compare_boxes$state <- as.factor(compare_boxes$state)
compare_boxes$fly <- as.factor(compare_boxes$fly)
compare_boxes$condition <- as.factor(compare_boxes$condition)

# Creating boxplots for all observation
b <- ggplot(compare_boxes, aes(y = duration, x = state, colour = state)) + 
  geom_boxplot(fill = "#4271AE", alpha = 0.7, notch = FALSE) +
  scale_y_continuous(name = "Duration (seconds)", limits = c(0, 1250)) +
  ggtitle("Comparing Resting and Feeding Patterns") + 
  theme_bw() +
  theme(plot.title = element_text(size = 12, family = "Tahoma", face = "bold", hjust = 0.5),
        text = element_text(size = 10, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 10)) +
  geom_jitter() + 
  facet_grid(. ~ condition) +
  scale_fill_brewer(type = "seq")
  
b


# Confirm observations with summary of data
fed <- compare_boxes[which(compare_boxes$condition == "fed"), ]
starved <- compare_boxes[which(compare_boxes$condition == "starved"), ]
fed_feeding <- fed[which(fed$state == "feeding"), ]
fed_resting <- fed[which(fed$state == "resting"), ]
stv_feeding <- starved[which(starved$state == "feeding"), ]
stv_resting <- starved[which(starved$state == "resting"), ]

summary(fed_feeding$duration)
summary(fed_resting$duration)
summary(stv_feeding$duration)
summary(stv_resting$duration)


# Comparing durations between factors - Analysis of variance
# First comparing between the conditions (starved or fed) of the flies
summary(aov(duration ~ condition, data = compare_boxes))

# Now comparing between the states (feeding or resting)
summary(aov(duration ~ state, data = compare_boxes))


# We find that there is indeed significant difference between factors
# ANOVA test assumes that the data is normally distributed and the variance across factors are homogenous
# We check these assumptions:
# 1. Normality - Normal Q-Q plot
plot(aov(duration ~ condition, data = compare_boxes), main = "By condition of flies", 2)
plot(aov(duration ~ state, data = compare_boxes), main = "By state of flies", 2)

# 2. Homogeneity of variance - Residuals vs Fitted plot
plot(aov(duration ~ condition, data = compare_boxes), main = "By condition of flies", 1)
plot(aov(duration ~ state, data = compare_boxes), main = "By state of flies", 1)

# 2. Homogeneity of variance - Levene's Test
leveneTest(duration ~ condition, data = compare_boxes)
#leveneTest(duration ~ state, data = compare_boxes)
