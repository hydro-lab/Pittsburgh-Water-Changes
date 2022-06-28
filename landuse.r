library(raster) # to import image
library(sp)
library(rgdal)
library(rgeos) # gCovers - test extents, not needed to crop
library(dplyr) # used first in rename - general data frame management 
library(ggplot2) # plotting tools (histogram)

# Set your working directory.  Avoid spaces in your path and OneDrive
setwd("D:\\GIS\\layers")

# ------------------------------------------------------------------------------
# Import raster image
cover <- raster("landlayers\\LCMAP_CU_2009_V12_LCPRI.tif") # Aug

# To crop the image
crp_ext <- readOGR("Chartier\\globalwatershed.shp")

sr<-"+proj=longlat +datum=WGS84"
cover.proj<- projectRaster(cover,crs = sr)





cover.crp<-crop(cover,crp_ext)






ggplot() + 
  geom_raster(data=ndwi_df, aes(x=x,y=y,fill=layer)) +
  scale_fill_gradientn(colours = topo.colors(5), limits = c(-1,0.1)) +
  theme(axis.text = element_blank(), axis.title = element_blank())

writeRaster(ndwi, 
            filename= "20220102_ndwi.tif",
            format = "GTiff", # save as a tif, save as a FLOAT if not default, not integer
            overwrite = TRUE)  # OPTIONAL - be careful. This will OVERWRITE previous files.

# ------------------------------------------------------------------------------
## TO DETERMINE WATER THRESHOLD
# Build histogram 
h = hist(ndwi, # built-in histogram function
         breaks=seq(-1,1,by=0.01), # span of possible NDWI values
         plot=FALSE) 

bins <- h$mids # positions    number
v <- h$counts # counts        integer

# Allocate arrays used in analysis
windows <- 10 # number of averaging windows
avg <- array(0, dim = c(length(bins),windows)) # moving average filter
peaks <- array(0, dim = c(length(bins),windows)) # peak locations
nop <- array(0, dim = c(windows)) # number of peaks found

# Loop around number averaging window values
for (w in 1:10){
  # filter values (v=h$counts) with the averaging window size 2*w+1; therefore, w is the radius of the window
  for (k in (w+1):(200-w)){ # assume tails are unimportant, default to zero based on preallocation
    avg[k,w] <- ((sum(v[(k-w):(k+w)]))/((2*w)+1))
  }
  # identify and number peaks
  cnt <- 0
  for (j in (w+1):(200-w)){
    if ( ((avg[j-1,w])<(avg[j,w])) & ((avg[j+1,w])<(avg[j,w])) ) {
      cnt <- (cnt+1) # if a peak, add one to count
      peaks[j,w] <- cnt # add peak count to location
      nop[w] <- cnt # count peaks
      
    }
  }
}

# Set error values for the result vectors in case neither two nor three peaks are found:
threepeak <- -1
twopeak <- -1

# Detect peaks
for (w in 1:10){
  # testing in three peaks
  # due to the order of the w variable, only the 'smoothest' result will be kept
  if ((nop[w])==3){
    # finds the second and third peak
    for (j in 1:200){
      if ((peaks[j,w])==2){
        sec <- j # stores the index of the second peak
      }
      if ((peaks[j,w])==3){
        thr <- j # stores the index of the third peak
      }
    }
    # finds minimum between second and third peak
    m <- max(v) # create variable for minimum, initially set higher than any value
    for (j in (sec):(thr)){
      if ((avg[j,w])<m){
        goal <- j
        m <- avg[j,w]
      }
    }
    threepeak <- (bins[(goal)])
  }
  # test in case exactly three peaks were not found
  if ((nop[w])==2){
    # find the position of the first and second (the only) peaks
    for (j in 1:200){
      if ((peaks[j,w])==1){
        fst <- j # stores the index of the second peak
      }
      if ((peaks[j,w])==2){
        sec <- j # stores the index of the third peak
      }
    }
    # finds minimum between first and second peak
    m <- max(v) # create variable for minimum, initially set higher than any value
    for (j in (fst):(sec)){
      if ((avg[j,w])<m){
        goal <- j
        m <- avg[j,w]
      }
    }
    twopeak <- (bins[(goal)])
  }
}

# Store values
ndwi_values <- data.frame(ndwi@data@values)
ndwi_values <- rename(ndwi_values, data=ndwi.data.values)

# histogram visualization
h <- ggplot(ndwi_values, aes(x=data)) +
  geom_histogram(breaks = (c(0:200)/100-1), color = "black", fill = "gray", na.rm = TRUE) +
  geom_vline(aes(xintercept = twopeak), color = "green") +
  geom_vline(aes(xintercept = threepeak), color = "blue") +
  xlab("NDWI") +
  ylab("Count") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(axis.text = element_text(face = "plain", size = 24)) +
  theme(axis.title = element_text(face = "plain", size = 24))
ggsave("20210102_hist.eps", h, device = "eps", dpi = 72)

# Analysis
# The two-peak analysis provides a more stable threshold value by visual 
# inspection; therefore, we will set the threshold to twopeak:
ndwi_threshold <- twopeak

# ------------------------------------------------------------------------------
## CALCULATE AREA

# If it is sufficient to compare number of pixels:
no_of_pixels <- sum(ndwi[] >= ndwi_threshold)

# With raster:area
# Need to reproject:
ndwi_WGS84 <- projectRaster(ndwi, crs = "+proj=longlat +datum=WGS84 +no_defs")
sz <- area(ndwi_WGS84) # finds areas
sm <- 0
for (i in 1:length(sz@data@values)) {
  if (is.na(ndwi_WGS84@data@values[i]) == FALSE) {
    if (ndwi_WGS84@data@values[i] >= ndwi_threshold) {
      sm <- sm + sz@data@values[i]
    }
  }
}
