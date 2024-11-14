require(raster)
require(terra)

#Function to create a template with size tempLength+1xtempLength+1 and values=1 at the cells given by xcells and ycells
createTempLine <- function(tempLength,xcells,ycells){
  tempLine<-matrix(0,tempLength+1,tempLength+1)
  tempMid<-tempLength/2+1
  for (l in 1:length(xcells)){
    #Only set cell to 1 if the distance to the center is leq tempLength/2
    if (sqrt((xcells[l]-tempMid)^2+(ycells[l]-tempMid)^2)<=tempLength/2){
      tempLine[ycells[l],xcells[l]]<-1
    }
  }
  return(tempLine)
}

chm_dir <- 'G:/Users/ioly0001/Thesis/Data/R/Segmentation/LDT dens/'
chm_files <- list.files(path = chm_dir, pattern = "\\.tif$", full.names = TRUE)

chm <- raster("G:/Users/ioly0001/Thesis/Data/R/Segmentation/LDT dens/plot1_021.tif")
chm_bw <- chm
chm_bw[]<-(0)
chm_bw[chm>=2]<-1

tempLength<-5/0.25
tempMid<-tempLength/2+1

k<-5
onedir<-seq(0,tempLength/2)
xcells<-c(tempMid-onedir,tempMid+onedir)

tempMatch<-stack()
dens_dir <- 'G:/Users/ioly0001/Thesis/Data/R/Segmentation/LDT/'
i <- 1
class(chm_bw)
for (chm_file_path in chm_files) {
  
  chm_bw <- raster(chm_files[i])
  chm_bw[i] <- (0)
  chm_bw[chm_bw >= 2] <- 1
  print('1')
  tempLength<-5/0.25
  tempMid<-tempLength/2+1
  
  print('2')
  onedir<-seq(0,tempLength/2)
  xcells<-c(tempMid-onedir,tempMid+onedir)
  print('edo')
  print('3')
  tempMatch<-stack()
  print('edo')
  
  #Create line templates from 0 to pi/4 and apply templates to raster
  for (k in 1:(tempLength/2)){
    ycells1<-c(tempMid-round(onedir*k/(tempLength/2)),tempMid+round(onedir*k/(tempLength/2)))
    ycells2<-tempLength+1-ycells1+1
    tempLine<-createTempLine(tempLength,xcells,ycells2)
    print('windows')
    #print(tempLine)
    #plot(raster(tempLine))
    tempMatch_new <- focal(chm_bw, w = tempLine, fun = sum, na.rm = TRUE)
    tempMatch<-stack(tempMatch,tempMatch_new)
  }
  print('out')
  #Create line templates from pi/4 to pi/2 and apply templates to raster
  for (k in (tempLength/2-1):0){
    ycells1<-c(tempMid-round(onedir*k/(tempLength/2)),tempMid+round(onedir*k/(tempLength/2)))
    ycells2<-tempLength+1-ycells1+1
    tempLine<-createTempLine(tempLength,ycells2,xcells)
    #windows()
    #plot(raster(tempLine))
    tempMatch_new<- focal(chm_bw, w=tempLine, fun=sum, na.rm=TRUE)
    tempMatch<-stack(tempMatch,tempMatch_new)
  }
  print('outt')
  #Create line templates from pi/2 to 3pi/4 and apply templates to raster
  for (k in 1:(tempLength/2)){
    ycells1<-c(tempMid-round(onedir*k/(tempLength/2)),tempMid+round(onedir*k/(tempLength/2)))
    ycells2<-tempLength+1-ycells1+1
    tempLine<-createTempLine(tempLength,ycells1,xcells)
    #windows()
    #plot(raster(tempLine))
    tempMatch_new<- focal(chm_bw, w=tempLine, fun=sum, na.rm=TRUE)
    tempMatch<-stack(tempMatch,tempMatch_new)
  }
  print('outtt')
  #Create line templates from 3pi/4 to pi and apply templates to raster
  for (k in (tempLength/2-1):0){
    ycells1<-c(tempMid-round(onedir*k/(tempLength/2)),tempMid+round(onedir*k/(tempLength/2)))
    ycells2<-tempLength+1-ycells1+1
    tempLine<-createTempLine(tempLength,xcells,ycells1)
    #windows()
    #plot(raster(tempLine))
    tempMatch_new<- focal(chm_bw, w=tempLine, fun=sum, na.rm=TRUE)
    tempMatch<-stack(tempMatch,tempMatch_new)
  }
  print('out1')
  filechm_file_path <- paste0(dens_dir, basename(chm_file_path))
  writeRaster(tempMatch, filechm_file_path, overwrite = TRUE)
  cat("Processed tif file:", chm_file_path, '\n')
  i <- i + 1  
}
#Create line templates from 0 to pi/4 and apply templates to raster
for (k in 1:(tempLength/2)){
  ycells1<-c(tempMid-round(onedir*k/(tempLength/2)),tempMid+round(onedir*k/(tempLength/2)))
  ycells2<-tempLength+1-ycells1+1
  tempLine<-createTempLine(tempLength,xcells,ycells2)
  #windows()
  #plot(raster(tempLine))
  tempMatch_new<- focal(chm_bw, w=tempLine, fun=sum, na.rm=TRUE)
  tempMatch<-stack(tempMatch,tempMatch_new)
}
#Create line templates from pi/4 to pi/2 and apply templates to raster
for (k in (tempLength/2-1):0){
  ycells1<-c(tempMid-round(onedir*k/(tempLength/2)),tempMid+round(onedir*k/(tempLength/2)))
  ycells2<-tempLength+1-ycells1+1
  tempLine<-createTempLine(tempLength,ycells2,xcells)
  #windows()
  #plot(raster(tempLine))
  tempMatch_new<- focal(chm_bw, w=tempLine, fun=sum, na.rm=TRUE)
  tempMatch<-stack(tempMatch,tempMatch_new)
  
  
}
#Create line templates from pi/2 to 3pi/4 and apply templates to raster
for (k in 1:(tempLength/2)){
  ycells1<-c(tempMid-round(onedir*k/(tempLength/2)),tempMid+round(onedir*k/(tempLength/2)))
  ycells2<-tempLength+1-ycells1+1
  tempLine<-createTempLine(tempLength,ycells1,xcells)
  #windows()
  #plot(raster(tempLine))
  tempMatch_new<- focal(chm_bw, w=tempLine, fun=sum, na.rm=TRUE)
  tempMatch<-stack(tempMatch,tempMatch_new)
}
#Create line templates from 3pi/4 to pi and apply templates to raster
for (k in (tempLength/2-1):0){
  ycells1<-c(tempMid-round(onedir*k/(tempLength/2)),tempMid+round(onedir*k/(tempLength/2)))
  ycells2<-tempLength+1-ycells1+1
  tempLine<-createTempLine(tempLength,xcells,ycells1)
  #windows()
  #plot(raster(tempLine))
  tempMatch_new<- focal(chm_bw, w=tempLine, fun=sum, na.rm=TRUE)
  tempMatch<-stack(tempMatch,tempMatch_new)
}

writeRaster(tempMatch,"G:/Users/ioly0001/Thesis/Data/R/Segmentation/LDT/tempMatch.tif",overwrite=TRUE)
?stack()
