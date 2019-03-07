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
leveneTest(duration ~ state, data = compare_boxes)
