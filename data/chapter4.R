# access the MASS package
library(MASS)
library(GGally)
library(corrplot)


# load the data
data("Boston")

glimpse(Boston)


p <- ggpairs(Boston, mapping = aes(), lower = list(combo = wrap("facethist", bins = 10)), upper = list(continuous = wrap("cor", size=3)))
p


summary(Boston)


# calculate the correlation matrix and round it
cor_matrix<-cor(Boston) %>% round(digits = 2)
# visualize the correlation matrix
corrplot(cor_matrix, method="circle", type="upper", tl.pos="d", tl.cex=0.6)



cb <- as.data.frame(cor(Boston)) # Create a DF of the correlation matrix.
cor_matrix_high <- as.data.frame(matrix(nrow = 14, ncol = 14)) #Copy
colnames(cor_matrix_high) <- colnames(cor_matrix) #the structure of
rownames(cor_matrix_high) <- rownames(cor_matrix) #cor_matrix.
cor_threshold <- 0.7
# Loop through the correlation matrix and save only values that exceed the threshold.
for(col in names(cb)) {
  for(row in 1:length(cb[[col]])) {
    if(abs(cb[[col,row]]) > cor_threshold & abs(cb[[col,row]]) < 1) { 
      cor_matrix_high[col,as.character(rownames(cb)[row])] <- round(cb[[col,row]], digits = 2)
    }
  }
}
# Print the matrix.
cor_matrix_high



boston_scaled <- as.data.frame(scale(Boston))
summary(boston_scaled)
# Create a quantile vector of crim, and use it to create the categorical "crime".
bins <- quantile(boston_scaled$crim)
crime <- cut(boston_scaled$crim, breaks = bins, include.lowest = TRUE, label = c('low','med_low','med_high','high'))
# Replace the original unscaled variable.
boston_scaled <- dplyr::select(boston_scaled, -crim)
boston_scaled <- data.frame(boston_scaled, crime)
table(boston_scaled$crim) # Explore the categorised variable.




n <- nrow(boston_scaled) # Get number of rows in the dataset.
ind <- sample(n,  size = n * 0.8) # Choose randomly 80% of the rows.
train <- boston_scaled[ind,] # Create train set.
test <- boston_scaled[-ind,] # Create test set.
# Save the correct classes from the test data.
correct_classes <- test$crime
# Remove the crime variable from the test data.
test <- dplyr::select(test, -crime)



lda.fit <- lda(crime ~ ., data = train)
lda.fit



# Define a function for the biplot arrows.
lda.arrows <- function(x, myscale = 2, arrow_heads = 0.2, color = "deeppink", tex = 1, choices = c(1,2)){
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex, col=color, pos=3)
}
classes <- as.numeric(train$crime) # Turn the classes to numeric for plotting.
plot(lda.fit, dimen = 2, col = classes, pch = classes) # Plot.
lda.arrows(lda.fit) # Add arrows.


lda.pred <- predict(lda.fit, newdata = test) # Predict the test values.
# Cross tabulate the predictions with the correct values.
table(correct = correct_classes, predicted = lda.pred$class)

boston_scaled <- as.data.frame(scale(Boston)) # Standardise the data.
dist_eu <- dist(boston_scaled) # Create an euclidian distance matrix.
summary(dist_eu) # Summarise the matrix.


km <-kmeans(dist_eu, centers = 4) # Cluster the data.
pairs(boston_scaled, col = km$cluster) # Plot the clusters.



k_max <- 15 # Maximum number of clusters to try.
# Define a function for testing.
k_try <- function(k) {
  kmeans(dist_eu, k)$tot.withinss
}
# Calculate the total within sum of squares using the function.
twcss <- sapply(1:k_max, k_try)

# Visualize the results.
plot(1:k_max, twcss, type='b')



km <-kmeans(dist_eu, centers = 2) # Cluster the data.
pairs(boston_scaled, col = km$cluster) # Plot the clusters.


