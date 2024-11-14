library(lidR)
library(raster)
library(sf)
library(dplyr)

csv_dir <- 'G:/Users/ioly0001/Thesis/Data/R/Segmentation/Annotations'
csv_files <- list.files(path = csv_dir, pattern = "\\.csv$", full.names = TRUE)
chm_dir <- 'G:/Users/ioly0001/Thesis/Data/R/Segmentation/CHM_seg'
chm_files <- list.files(path = chm_dir, pattern = "\\.tif$", full.names = TRUE)
las_dir <- "G:/Users/ioly0001/Thesis/Data/R/Segmentation/LAS"
las_files <- list.files(path = las_dir, pattern = "\\.las$", full.names = TRUE)
met_dir <- 'C:/Users/ioly0001/Documents/LasKrycklan - noise/Metrics/'
met_files <- list.files(path = met_dir, pattern = "\\.csv$", full.names = TRUE)

i <- 1


for (las_file_path in las_files) {
  
  csv_file <- csv_files[i]
  # chm_file <- chm_files[i]
  las_file <- las_files[i]
  metrics <- file.path(met_dir, paste0(tools::file_path_sans_ext(basename(las_file)), "_m.csv"))
  
  feat <- seg(csv_file, chm_file, las_file, metrics)
  
  i <- i + 1
  
  # all_metrics[[basename(las_file)]] <- feat
  
}

# Merge all metrics in one csv file

j <- 1
merged_mets <- data.frame()

for (met_file_path in met_files) {
  
  csv <- read.csv(met_file_path, header = TRUE, sep = ',')
  merged_mets <- rbind(merged_mets, csv)
  j <- 1 + j
  
}
out_file <- 'D:/ioly_mets/merged_mets.csv'
write.csv(merged_mets, file = out_file, row.names = FALSE)

# Normalize values of height and radius
merged_mets <- read.table('D:/ioly_mets/merged_mets.csv', header = TRUE, sep = ',')

# Create normalized radius column
merged_mets$n_radius <- (merged_mets$mean_r - merged_mets$min_r) / (merged_mets$max_r - merged_mets$min_r)

# Create normalized height column
merged_mets$n_height <- (merged_mets$mean_h - merged_mets$min_h) / (merged_mets$max_h - merged_mets$min_h)

# Save the updated dataframe to a new file
write.csv(df, "path_to_your_file/merged_mets_normalized.csv", row.names = FALSE)

# View the updated dataframe
head(df)

#### Graphs ####

hist(merged_mets$mean_r)
hist(merged_mets$max_h)
hist(merged_mets$dens10)
hist(merged_mets$dens50)
hist(merged_mets$dens100)
hist(merged_mets$dens200)

SDTs <- merged_mets[merged_mets$Status == 'SDT',]
Healthy <- merged_mets[merged_mets$Status == 'Healthy',]
hist(SDTs$mean_r)
hist(Healthy$mean_r)
hist(SDTs$mean_h)
hist(Healthy$mean_h)
hist(SDTs$dens10)
hist(Healthy$dens10)
hist(SDTs$dens25)
hist(Healthy$dens25)
hist(SDTs$dens50)
hist(Healthy$dens50)
hist(SDTs$dens100)
hist(Healthy$dens100)
hist(SDTs$dens200)
hist(Healthy$dens200)

plot(SDTs$mean_h, SDTs$mean_r)
plot(Healthy$mean_h, Healthy$mean_r)

color = c('green', 'brown')
merged_mets$Status <- as.factor(merged_mets$Status)
plot(merged_mets$mean_r, merged_mets$dens100, col = color[unclass(merged_mets$Status)], pch = 16, xlab = "Mean R", ylab = "Dens 100")

plot(merged_mets$mean_r, merged_mets$mean_h, col = color)

#### Prediction Model ####

install.packages(c('klaR', 'MASS', 'ggplot2'))
library(klaR)
library(MASS)
library(ggplot2)

# Load the dataset
merged_mets <- read.table('C:/Users/ioly0001/Desktop/merged_mets.csv', header = TRUE, sep = ',')

# Ensure Status is a factor
merged_mets$Status <- as.factor(merged_mets$Status)
merged_mets$
# Define the swa function for subsampling
swa <- function(data, target_col, num_samples, seed) {
  set.seed(seed)
  min_class <- min(table(data[[target_col]]))
  sampled_data <- do.call(rbind, lapply(levels(data[[target_col]]), function(cls) {
    subset(data, data[[target_col]] == cls)[sample(min_class, num_samples), ]
  }))
  return(sampled_data)
}


# Perform LDA with the selected variables
linear <- lda(Status ~ mean_r + dens100, data = merged_mets)

# Subsample using swa function
subsampled_data_all <- swa(merged_mets, "Status", 300, 500)
subsampled_model_all <- lda(Status ~ n_height, data = subsampled_data_all)

# Make predictions using the subsample for all variables
predictions <- predict(subsampled_model_all, merged_mets)

# Create a confusion matrix
confusion_matrix_all <- table(predicted = predictions$class, actual = merged_mets$Status)
print("Confusion Matrix - All Variables:")
print(confusion_matrix_all)

# Calculate accuracy for all variables model
accuracy_all <- sum(diag(confusion_matrix_all)) / sum(confusion_matrix_all)
print(paste("Accuracy - All Variables:", accuracy_all))

# Perform LDA with selected variables (mean_r, dens100, mean_h)
linear1 <- lda(Status ~ n_radius, data = merged_mets)

# Subsample using swa function for selected variables model
subsampled_data1 <- swa(merged_mets, "Status", 300, 10)
subsampled_model1 <- lda(Status ~ n_radius, data = subsampled_data1)

# Make predictions using the subsample for selected variables
predictions1 <- predict(subsampled_model1, merged_mets)
confusion_matrix1 <- table(predicted = predictions1$class, actual = merged_mets$Status)
print("Confusion Matrix - meanR, dens100, meanH:")
print(confusion_matrix1)

# Calculate accuracy for the selected variables model
accuracy1 <- sum(diag(confusion_matrix1)) / sum(confusion_matrix1)
print(paste("Accuracy - meanR, dens100, meanH:", accuracy1))

# Function to calculate additional metrics
calculate_metrics <- function(confusion_matrix) {
  true_positive <- diag(confusion_matrix)
  false_positive <- colSums(confusion_matrix) - true_positive
  false_negative <- rowSums(confusion_matrix) - true_positive
  true_negative <- sum(confusion_matrix) - (true_positive + false_positive + false_negative)
  
  precision <- true_positive / (true_positive + false_positive)
  recall <- true_positive / (true_positive + false_negative)
  f1_score <- 2 * ((precision * recall) / (precision + recall))
  
  # Handle division by zero in precision, recall, and F1 score
  precision[is.nan(precision)] <- 0
  recall[is.nan(recall)] <- 0
  f1_score[is.nan(f1_score)] <- 0
  
  list(precision = precision, recall = recall, f1_score = f1_score)
}

# Calculate metrics for all variables model
metrics_all <- calculate_metrics(confusion_matrix_all)
print("Metrics - All Variables:")
print(metrics_all)

# Calculate metrics for selected variables model
metrics1 <- calculate_metrics(confusion_matrix1)
print("Metrics - meanR, dens100, meanH:")
print(metrics1)

# Plotting LDA results
plot_lda <- function(model, data, title) {
  lda_values <- predict(model, data)
  lda_df <- data.frame(LDA1 = lda_values$x[, 1], Status = data$Status)
  
  if (ncol(lda_values$x) > 1) {
    lda_df$LDA2 <- lda_values$x[, 2]
    p <- ggplot(lda_df, aes(x = LDA1, y = LDA2, color = Status)) +
      geom_point() +
      labs(title = title, x = "LDA1", y = "LDA2") +
      theme_minimal()
  } else {
    p <- ggplot(lda_df, aes(x = LDA1, color = Status, fill = Status)) +
      geom_histogram(position = "dodge", bins = 30) +
      labs(title = title, x = "LDA1") +
      theme_minimal()
  }
  
  print(p)
}

# Plot for all variables model
plot_lda(subsampled_model_all, merged_mets, "LDA - n_height + dens100")

# Plot for selected variables model
plot_lda(subsampled_model1, merged_mets, "LDA - n_height + n_radius")









