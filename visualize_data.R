# data.frame function throws an error if the lengths of the two vectors are not the same
# So add NA to the shorter vector if needed

final <- data.frame(x=c(final_resting), y=c(final_feeding))
colnames(final) <- c("resting","feeding")

boxplot(final)
