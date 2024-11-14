library(lidR)
library(raster)
library(sf)
library(dplyr)
library(ggplot2)
las_dir <- "G:/Users/ioly0001/Thesis/Data/LasKrycklan - noise"
las_files <- list.files(path = las_dir, pattern = "\\.las$", full.names = TRUE)

#### Extract .las files without noise ####

no_noise_dir <- "G:/Users/ioly0001/Thesis/Data/LasKrycklan - noise/"

for (las_file_path in las_files) {
  
  n <- nn(las_file_path)
  
  filelas_file_path <- paste0(no_noise_dir, basename(las_file_path))
  
  writeLAS(n, filelas_file_path)
  
  cat("Noise removed and saved to:", filelas_file_path, "\n")
  
}

#### 0.2-1m las ####

las_dir021 <- "G:/Users/ioly0001/Thesis/Data/R/.las 0.2-1m/"

for (las_file_path in las_files) {
  
  las <- ls(las_file_path)
  
  filelas_file_path <- paste0(las_dir021, basename(las_file_path))
  
  writeLAS(las, filelas_file_path)
  
  cat("LAS file filtered and saved to:", filelas_file_path, "\n")
  
}

# Write CHM from the processed .las files
# Function to create CHM from LAS file
chm <- function(las_file_path) {
  cat("Reading LAS file:", las_file_path, "\n")
  las <- readLAS(las_file_path)
  if (is.null(las)) {
    stop("Failed to read LAS file: ", las_file_path)
  }
  cat("Generating CHM for LAS file:", las_file_path, "\n")
  chm_raster <- rasterize_canopy(las, res = 0.1, algorithm = p2r())
  return(chm_raster)
}

# Directories
las_dir_seg <- 'G:/Users/ioly0001/Thesis/Data/R/Segmentation/LAS 0.2-1'
chm_dir <- 'G:/Users/ioly0001/Thesis/Data/R/Segmentation/CHM_0.2-1m/'

# Get list of LAS files
las_files <- list.files(path = las_dir_seg, pattern = "\\.las$", full.names = TRUE)

# Process each LAS file
for (las_file_path in las_files) {
  cat("Processing LAS file:", las_file_path, "\n")
  chm_ext <- chm(las_file_path)
  raster_file_path <- paste0(chm_dir, gsub('.las$', '_chm.tif', basename(las_file_path)))
  cat("Writing CHM raster to:", raster_file_path, "\n")
  writeRaster(chm_ext, raster_file_path, overwrite = TRUE)
  cat("LAS file processed and CHM saved to:", raster_file_path, "\n")
}

cat("Processing complete.\n")

#### CHM ####

raster_dir <- "G:/Users/ioly0001/Thesis/Data/R/CHM/"

for (i in 1:485) {
  las_file_path <- las_files[i]
  raster_data <- processed_las(las_file_path)
  raster_file_path <- paste0(raster_dir, gsub('.las$', '_result.tif', basename(las_file_path)))
  writeRaster(raster_data, raster_file_path, overwrite = TRUE)
  
  cat("Processed LAS file:", las_file_path, "\n")
  
}

#### Filter by Z ####

# Writing rasters

filtered_dir1 <- "G:/Users/ioly0001/Thesis/Data/R/filtered_Z/0-5/"
filtered_dir2 <- "G:/Users/ioly0001/Thesis/Data/R/filtered_Z/5-10/"
filtered_dir3 <- "G:/Users/ioly0001/Thesis/Data/R/filtered_Z/10-15/"
filtered_dir4 <- "G:/Users/ioly0001/Thesis/Data/R/filtered_Z/15-20/"
filtered_dir5 <- "G:/Users/ioly0001/Thesis/Data/R/filtered_Z/20-25/"
filtered_dir6 <- "G:/Users/ioly0001/Thesis/Data/R/filtered_Z/25-30/"

for (las_file_path in las_files) {
  
  tryCatch( {
    
    filterlas1 <- filtered_las1(las_file_path)
    filterlas2 <- filtered_las2(las_file_path)
    filterlas3 <- filtered_las3(las_file_path)
    filterlas4 <- filtered_las4(las_file_path)
    filterlas5 <- filtered_las5(las_file_path)
    filterlas6 <- filtered_las6(las_file_path)
    
    
    filrast_file_path1 <- paste0(filtered_dir1, gsub('.las$', '_05.tif', basename(las_file_path)))
    filrast_file_path2 <- paste0(filtered_dir2, gsub('.las$', '_510.tif', basename(las_file_path)))
    filrast_file_path3 <- paste0(filtered_dir3, gsub('.las$', '_1015.tif', basename(las_file_path)))
    filrast_file_path4 <- paste0(filtered_dir4, gsub('.las$', '_1520.tif', basename(las_file_path)))
    filrast_file_path5 <- paste0(filtered_dir5, gsub('.las$', '_2025.tif', basename(las_file_path)))
    filrast_file_path6 <- paste0(filtered_dir6, gsub('.las$', '_2530.tif', basename(las_file_path)))
    
    writeRaster(filterlas1, filrast_file_path1, overwrite = TRUE)
    writeRaster(filterlas2, filrast_file_path2, overwrite = TRUE)
    writeRaster(filterlas3, filrast_file_path3, overwrite = TRUE)
    writeRaster(filterlas4, filrast_file_path4, overwrite = TRUE)
    writeRaster(filterlas5, filrast_file_path5, overwrite = TRUE)
    writeRaster(filterlas6, filrast_file_path6, overwrite = TRUE)
    
    cat("Filtered LAS files:", las_file_path, "\n")
    
    
  },
  
  error=function(e) {
    message('The point cloud contains 0 point for this height interval. No raster generated')
    print(e)
  }, 
  
  warning=function(w) {
    message('A Warning Occurred')
    print(w)
    return(NA)
  }
  
  )


  
}



#### Filter by ReturnNumber ####

num_dir1 <- "D:/Thesis/Data/R/filtered_ReturnNumber/1/"
num_dir2 <- "D:/Thesis/Data/R/filtered_ReturnNumber/2/"
num_dir3 <- "D:/Thesis/Data/R/filtered_ReturnNumber/3/"
num_dir4 <- "D:/Thesis/Data/R/filtered_ReturnNumber/4/"
num_dir5 <- "D:/Thesis/Data/R/filtered_ReturnNumber/5/"


for (las_file_path in las_files) {
  
  tryCatch( {
    
    rnlas1 <- r_num1(las_file_path)
    rnlas2 <- r_num2(las_file_path)
    rnlas3 <- r_num3(las_file_path)
    rnlas4 <- r_num4(las_file_path)
    rnlas5 <- r_num5(las_file_path)
    
    rnrast_file_path1 <- paste0(num_dir1, gsub('.las$', '_01.tif', basename(las_file_path)))
    rnrast_file_path2 <- paste0(num_dir2, gsub('.las$', '_02.tif', basename(las_file_path)))
    rnrast_file_path3 <- paste0(num_dir3, gsub('.las$', '_03.tif', basename(las_file_path)))
    rnrast_file_path4 <- paste0(num_dir4, gsub('.las$', '_04.tif', basename(las_file_path)))
    rnrast_file_path5 <- paste0(num_dir5, gsub('.las$', '_05.tif', basename(las_file_path)))
    
    writeRaster(rnlas1, rnrast_file_path1, overwrite = TRUE)
    writeRaster(rnlas2, rnrast_file_path2, overwrite = TRUE)
    writeRaster(rnlas3, rnrast_file_path3, overwrite = TRUE)
    writeRaster(rnlas4, rnrast_file_path4, overwrite = TRUE)
    writeRaster(rnlas5, rnrast_file_path5, overwrite = TRUE)
    
    cat("Filtered LAS files:", las_file_path, "\n")
  
    },
    
  error=function(e) {
      message('The point cloud contains 0 Return Numbers for this RN interval. No raster generated')
      print(e)
  }, 
    
  warning=function(w) {
      message('A Warning Occurred')
      print(w)
      return(NA)
  }
  )
}

#### Tree Tops ####

# Function to process LAS file and extract information
process_las <- function(las_file_path) {
  # Read LAS file
  las_data <- readLAS(las_file_path)
  
  # Clear noise
  las_filtered <- noise(las_data)
  
  # Extract tree tops
  ttopsloc <- tree_tops_loc(las_filtered)
  
  # Add X and Y coordinates
  ttopsloc$X <- st_coordinates(ttopsloc)[, "X"]
  ttopsloc$Y <- st_coordinates(ttopsloc)[, "Y"]
  
  # Add Height and Status attributes
  ttopsloc$Height <- ttopsloc$Z
  ttopsloc$Status <- "Healthy"
  
  # Remove geometry column
  ttopsloc <- ttopsloc[, -which(names(ttopsloc) == "geometry")]
  
  # Extract filename without extension
  file_name <- tools::file_path_sans_ext(basename(las_file_path))
  
  # Define the output CSV file path
  output_csv <- paste0("G:/Users/ioly0001/Thesis/Data/R/ttops/", file_name, ".csv")
  
  # Write the processed data to a CSV file
  write.csv(ttopsloc, file = output_csv, row.names = FALSE)
  
  # Print message indicating completion
  cat("Data exported to CSV:", output_csv, "\n")
}

# Directory containing LAS files
las_dir <- "G:/Users/ioly0001/Thesis/Data/LasKrycklan"

# List of LAS files
las_files <- list.files(las_dir, pattern = "\\.las$", full.names = TRUE)

# Process each LAS file and export the results to CSV files
lapply(las_files, process_las)



#### Density Layers ####

density_dir4 <- "C:/Users/user/Desktop/Thesis/Data/R/density/0.2-1/"

for (las_file_path in las_files) {
  
  density4 <- dense4(las_file_path)
  
  filrast_file_path4 <- paste0(density_dir4, gsub('.las$', '_021.tif', basename(las_file_path)))
  
  writeRaster(density4, filrast_file_path4, overwrite = TRUE)
  
  cat("Filtered LAS files:", las_file_path, "\n")
  
}

#### Annotation Accuracy Assessment (STDs) ####

# Load the merged SDTs file (annotations)
train <- read.table('G:/Users/ioly0001/Thesis/Data/R/merged_sdts.csv', header = TRUE, sep = ';')
ldt <- read.table('G:/Users/ioly0001/Thesis/Data/R/merged_ldts.csv', header = TRUE, sep = ',')


# Load the field data (GPS)
gps <- read.table('G:/Users/ioly0001/slu2024-05016.csv', header = TRUE, sep = ';')

# Remove all the rows with NA values
gps <- gps[!is.na(gps$Easting), ]

gps$min_dist <- 0 # New column for minimum distance
gps$min_dist_L <- 0
gps$class <- 0 # New column for classes

# Set the file in alphabetical order (Aspen, Dead, Laying)
gps <- gps[order(gps$Name),]

# Set classes
gps$class[1:17] <- 'Aspen'
gps$class[18:112] <- 'SDT'
gps$class[113:196] <- 'LDT'

i <- 1

gps_sdt <- gps[gps$class=='SDT',]
gps_sdt$min_dist <- NA


for (i in 1:nrow(gps_sdt)) {
  dist_vecx <- gps_sdt$Easting[i] - train$X
  dist_vecy <- gps_sdt$Northing[i] - train$Y
  dist_xy <- sqrt(dist_vecx^2 + dist_vecy^2)
  gps_sdt$min_dist[i] <- min(dist_xy, na.rm = TRUE)
}

gps_ldt <- gps[gps$class=='LDT',]
gps_ldt$min_dist_L <- NA

for (i in 1:nrow(gps_ldt)) {
  dist_vecx <- gps_sdt$Easting[i] - ldt$X
  dist_vecy <- gps_sdt$Northing[i] - ldt$Y
  dist_xy <- sqrt(dist_vecx^2 + dist_vecy^2)
  gps_ldt$min_dist_L[i] <- min(dist_xy, na.rm = TRUE)
}
## Omission / Commision ##
# Filter the rows that contain the specific plot names in the 'layer' column
train_obs <- train %>%
  filter(layer %in% c("tt_plot9", "tt_plot10", "tt_plot36", "tt_plot37", "tt_plot38")) %>%
  filter(Status == "SDT")

ldt_obs <- ldt %>%
  filter(layer %in% c("LDTplot9", "LDTplot10", "LDTplot36", "LDTplot37", "LDTplot38"))
# Define a threshold for true positives 
threshold <- 5

# Calculate true positives, false positives, and false negatives
true_positives <- sum(gps_sdt$min_dist <= threshold, na.rm = TRUE)
false_positives <- sum(gps_sdt$min_dist > threshold, na.rm = TRUE)
false_negatives <- nrow(train_obs) - true_positives

# Calculate omission and commission
omission_error <- false_negatives / (true_positives + false_negatives)
commission_error <- false_positives / (true_positives + false_positives)

print(paste("Omission Error:", omission_error))
print(paste("Commission Error:", commission_error))

#LDTS

thresholdL <- 10
# Calculate true positives, false positives, and false negatives
true_positivesL <- sum(gps_ldt$min_dist <= thresholdL, na.rm = TRUE)
false_positivesL <- sum(gps_ldt$min_dist > thresholdL, na.rm = TRUE)
false_negativesL <- nrow(ldt_obs) - true_positivesL

# Calculate omission and commission
omission_errorL <- false_negativesL / (true_positivesL + false_negativesL)
commission_errorL <- false_positivesL / (true_positivesL + false_positivesL)

print(paste("Omission Error:", omission_errorL))
print(paste("Commission Error:", commission_errorL))

# Create a data frame for plotting
error_data <- data.frame(
  Error_Type = c("Omission Error", "Commission Error"),
  Value = c(omission_error, commission_error)
)

error_plot <- ggplot(error_data, aes(x = Error_Type, y = Value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  ylim(0, 1) +
  theme_minimal() +
  labs(title = "SDTs Error Plot",
       x = "Error Type",
       y = "Error Rate")

print(error_plot)

