# Clearing noise
noise <- function(x) {
  return(filter_poi(x, Classification != LASNOISE & x$Z >= 0))
}

#### Clearing noise ####

nn <- function(las_file_path) {
  las_file <- readLAS(las_file_path)
  n <- noise(las_file) 
  return(n)
}

#### Functions for CHM ####

# Rasterizing CHM
chm <- function(x) {
  return(rasterize_canopy(x, res = 0.25, algorithm = p2r()))
}

??rasterize_canopy

# Apply the functions to all files (For CHM)
processed_las <- function(las_file_path) {
  las_data <- readLAS(las_file_path)
  las_filtered <- noise(las_data)
  chm_data <- chm(las_filtered)
  return(chm_data)
}

#### Height Filtering ####

# 0-5m
z_05 <- function(x) {
  return(filter_poi(x, Z >= 0 & Z <= 5))
}

# 5-10m
z_510 <- function(x) {
  return(filter_poi(x, Z >= 5 & Z <= 10))
}

# 10-15m
z_1015 <- function(x) {
  return(filter_poi(x, Z >= 10 & Z <= 15))
}

# 15-20m
z_1520 <- function(x) {
  return(filter_poi(x, Z >= 15 & Z <= 20))
}

# 20-25m
z_2025 <- function(x) {
  return(filter_poi(x, Z >= 20 & Z <= 25))
}

# 25-30
z_2530 <- function(x) {
  return(filter_poi(x, Z >= 25 & Z <= 31))
}

# Apply 1st filter
filtered_las1 <- function(las_file_path) {
  las_data <- readLAS(las_file_path)
  las_filtered1 <- noise(las_data)
  filter1 <- z_05(las_filtered1)
  rast1 <- chm(filter1)
  return(rast1)
}

# Apply 2nd filter
filtered_las2 <- function(las_file_path) {
  las_data <- readLAS(las_file_path)
  las_filtered2 <- noise(las_data)
  filter2 <- z_510(las_filtered2)
  rast2 <- chm(filter2)
  return(rast2)
}

# Apply 3rd filter
filtered_las3 <- function(las_file_path) {
  las_data <- readLAS(las_file_path)
  las_filtered3 <- noise(las_data)
  filter3 <- z_1015(las_filtered3)
  rast3 <- chm(filter3)
  return(rast3)
}

# Apply 4th filter
filtered_las4 <- function(las_file_path) {
  las_data <- readLAS(las_file_path)
  las_filtered4 <- noise(las_data)
  filter4 <- z_1520(las_filtered4)
  rast4 <- chm(filter4)
  return(rast4)
}

# Apply 5th filter
filtered_las5 <- function(las_file_path) {
  las_data <- readLAS(las_file_path)
  las_filtered5 <- noise(las_data)
  filter5 <- z_2025(las_filtered5)
  rast5 <- chm(filter5)
  return(rast5)
}

# Apply 6th filter
filtered_las6 <- function(las_file_path) {
  las_data <- readLAS(las_file_path)
  las_filtered6 <- noise(las_data)
  filter6 <- z_2530(las_filtered6)
  rast6 <- chm(filter6)
  return(rast6)
}

#### Filters for Return Numbers ####

rn1 <- function(x) {
  return(filter_poi(x, ReturnNumber == 1L))
}

rn2 <- function(x) {
  return(filter_poi(x, ReturnNumber == 2L))
}

rn3 <- function(x) {
  return(filter_poi(x, ReturnNumber == 3L))
}

rn4 <- function(x) {
  return(filter_poi(x, ReturnNumber == 4L))
}

rn5 <- function(x) {
  return(filter_poi(x, ReturnNumber == 5L))
}

  
# Return Number
r_num1 <- function(las_file_path) {
  las_data <- readLAS(las_file_path)
  las_fil1 <- noise(las_data)
  num1 <- rn1(las_fil1)
  raster1 <- chm(num1)
  return(raster1)
} 

r_num2 <- function(las_file_path) {
  las_data <- readLAS(las_file_path)
  las_fil2 <- noise(las_data)
  num2 <- rn2(las_fil2)
  raster2 <- chm(num2)
  return(raster2)
} 

r_num3 <- function(las_file_path) {
  las_data <- readLAS(las_file_path)
  las_fil3 <- noise(las_data)
  num3 <- rn3(las_fil3)
  raster3 <- chm(num3)
  return(raster3)
} 

r_num4 <- function(las_file_path) {
  las_data <- readLAS(las_file_path)
  las_fil4 <- noise(las_data)
  num4 <- rn4(las_fil4)
  raster4 <- chm(num4)
  return(raster4)
} 

r_num5 <- function(las_file_path) {
  las_data <- readLAS(las_file_path)
  las_fil5 <- noise(las_data)
  num5 <- rn5(las_fil5)
  raster5 <- chm(num5)
  return(raster5)
} 




#### Functions for Tree Tops ####

# CHM
chm_t <- function(x) {
  return(rasterize_canopy(x, 0.25, pitfree(subcircle = 0.01)))
}

# Function for tree heights (No return)
f <- function(x) {
  x * 0.1 + 3
}

# Tree tops function
tree_tops <- function(x) {
  return(locate_trees(x, lmf(f)))
}

tree_tops_loc <- function(x) {
  return(locate_trees(x, lmf(ws = 5)))
}

tt_data <- function(x) {
  
  # Add X and Y coordinates
  x$X <- st_coordinates(x)[, "X"]
  x$Y <- st_coordinates(x)[, "Y"]
  
  # Add Height and Status attributes
  x$Height <- x$Z
  x$Status <- "Healthy"
  
  # Ensure x is a data frame
  if (!is.data.frame(x)) {
    stop("Input must be a data frame.")
  }
  
  # Ensure required columns are present
  if (!all(c("treeID", "Z") %in% names(x))) {
    stop("Input data frame must contain 'treeID' and 'Z' columns.")
  }
  
  # Print structure of x before modifications
  print("Before modifications:")
  print(str(x))
  
  # Print structure of x after modifications
  print("After modifications:")
  print(str(x))
  
  return(x)
}




final <- function(las_file_path) {
  las_data <- readLAS(las_file_path)
  las_filtered <- noise(las_data)
  ttopsloc <- tree_tops_loc(las_filtered)
  t_data <- tt_data(ttopsloc)
  return(t_data)
}




#### Density ####

density <- function(x) {
  return(rasterize_density(x, res = 0.1))
}

z_210 <- function(x) {
  return(filter_poi(x, Z >= 2 & Z <= 10))
}

# 5-10m
z_1015 <- function(x) {
  return(filter_poi(x, Z >= 10 & Z <= 15))
}

# 10-15m
z_1525 <- function(x) {
  return(filter_poi(x, Z >= 15 & Z <= 25))
}

dense1 <- function(las_file_path) {
  las_data <- readLAS(las_file_path)
  las_filtered1 <- noise(las_data)
  filter1 <- z_210(las_filtered1)
  rast1 <- density(filter1)
  return(rast1)
}

dense2 <- function(las_file_path) {
  las_data <- readLAS(las_file_path)
  las_filtered2 <- noise(las_data)
  filter2 <- z_1015(las_filtered2)
  rast2 <- density(filter2)
  return(rast2)
}

dense3 <- function(las_file_path) {
  las_data <- readLAS(las_file_path)
  las_filtered3 <- noise(las_data)
  filter3 <- z_1525(las_filtered3)
  rast3 <- density(filter3)
  return(rast3)
}



#### 0.2-1m .las files ####

z_021 <- function(x) {
  return(filter_poi(x, Z >= 0.2 & Z <= 1))
}

ls <- function(las_file_path) {
  las_data <- readLAS(las_file_path)
  n <- noise(las_data)
  flas <- z_021(n)
  return(flas)
}

#### Segmentation and Metrics ####

seg <- function(csv_file_path, chm_file_path, las_file_path, met_dir){
  
  # Read annotated data
  csv <- read.table(csv_file_path, header = TRUE, sep = ',') 
  csv <- subset(csv, select = c('treeID', 'Z', 'X', 'Y', 'Status'))
  csvr <- read.table(csv_file_path, header = TRUE, sep = ',') 
  csvr <- subset(csvr, select = c('treeID', 'Z', 'X', 'Y', 'Status'))
  
  las <- readLAS(las_file_path) # read las files
  chm <- chm(las)
  
  crs_las <- crs(las)
  csv <- st_as_sf(csv, coords=c("X","Y"), crs=crs_las)
  
  status <- csvr$Status
  
  alg <- dalponte2016(chm, csv) # correlate annotations on csv
  las <- segment_trees(las, alg) # segment trees
  
  id <- unique(las$treeID) # isolate treeIDs
  id <- id[id >= 0]
  id <- na.omit(id) # remove NAs
  id <- unique(id)  # get only the id numbers
  id <- sort(id)
  
  # Create feat data frame with tree metrics and matching status
  feat <- data.frame(id = id, mean_h = NA, max_r = NA, max_h = NA, mean_r = NA, 
                     min_h = NA, min_r = NA, sd_r = NA, dens10 = NA, dens25 = NA, dens50 = NA, dens100 = NA, dens200 = NA)
  
  for (i in 1:length(id)) {
    index_tree <- (las$treeID == id[i])
    index_tree <- !is.na(index_tree) & index_tree # Ensure index_tree does not have NA values
    
    if (sum(index_tree, na.rm = TRUE) == 0) {
      next
    }
    mets_all <- metrics_c(index_tree, las, id[i])
    
    # Ensure the metrics are consistent with the feat structure
    if (!is.null(mets_all) && all(names(mets_all) == names(feat))) {
      feat[i, ] <- mets_all
    }
  }
  
  # Handling extra tree IDs
  # Use some if statements in case of NA values
  id_extra <- csv$treeID[-(1:length(id))]
  csv_extra <- csvr[-(1:length(id)), ]
  
  for (i in 1:length(id_extra)) {
    index_tree_extr <- (((las$X - csv_extra$X[i])^2 + (las$Y - csv_extra$Y[i])^2) <= 1)
    index_tree_extr <- !is.na(index_tree_extr) & index_tree_extr 
    
    if (sum(index_tree_extr, na.rm = TRUE) == 0) {
      next
    }
    mets <- metrics_c(index_tree_extr, las, id_extra[i])
    
    
    if (!is.null(mets) && all(names(mets) == names(feat))) {
      feat <- rbind(feat, mets)
    }
  }
  
  # Merge feat with csvr based on treeID to get Status
  feat <- merge(feat, csvr[, c('treeID', 'Status')], by.x = 'id', by.y = 'treeID', all.x = TRUE)
  
  # Check the size of feat and csvr
  print(paste("Number of rows in feat:", nrow(feat)))
  print(paste("Number of rows in csvr:", nrow(csvr)))
  
  write.csv(feat, file = met_dir, row.names = FALSE)
  cat('Metrics for', las_file_path, 'saved to:', met_dir, '\n')
  return(feat)
}

metrics_c <- function(index_tree, las, id_1) {
  if (sum(index_tree, na.rm = TRUE) == 0) {
    return(NULL)
  }
  
  feat_1 <- data.frame(id = id_1, mean_h = NA, max_r = NA, max_h = NA, 
                       mean_r = NA, min_h = NA, min_r = NA, sd_r = NA, dens10 = NA, dens25 = NA, dens50 = NA, 
                       dens100 = NA, dens200 = NA)
  
  x_id <- las$X[index_tree]
  y_id <- las$Y[index_tree]
  z_id <- las$Z[index_tree]
  tree_frame <- data.frame(x = x_id, y = y_id, z = z_id)
  thresh_z <- tree_frame[tree_frame$z >= 1.5, ]
  
  if (nrow(thresh_z) > 0) {
    # Mean / Max / Min height
    feat_1$mean_h <- mean(thresh_z$z, na.rm = TRUE)
    feat_1$max_h <- max(thresh_z$z, na.rm = TRUE)
    feat_1$min_h <- min(thresh_z$z, na.rm = TRUE)
    
    # Mean / Max / Min radius from the segmentation point
    mean_x <- mean(thresh_z$x, na.rm = TRUE)
    mean_y <- mean(thresh_z$y, na.rm = TRUE)
    dist <- sqrt((thresh_z$x - mean_x)^2 + (thresh_z$y - mean_y)^2)
    feat_1$sd_r <- sd(dist, na.rm = TRUE)
    feat_1$max_r <- max(dist, na.rm = TRUE)
    feat_1$mean_r <- mean(dist, na.rm = TRUE)
    feat_1$min_r <- min(dist, na.rm = TRUE)
    
    # calculate density from 0.25m to 2m from the segmantation point
    feat_1$dens10 <- length(dist[dist <= 0.1]) / (0.1^2 * pi) 
    feat_1$dens25 <- length(dist[dist <= 0.25]) / (0.25^2 * pi)
    feat_1$dens50 <- length(dist[dist <= 0.5]) / (0.5^2 * pi)
    feat_1$dens100 <- length(dist[dist <= 1]) / (1^2 * pi)
    feat_1$dens200 <- length(dist[dist <= 2]) / (2^2 * pi)
  }
  
  return(feat_1)
}

