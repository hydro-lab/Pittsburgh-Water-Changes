library(raster) # to import image
library(sp)
library(rgdal)
library(rgeos) # gCovers - test extents, not needed to crop
library(doParallel)
library(dplyr) # used first in rename - general data frame management 
library(stringr)
library(ggplot2) # plotting tools (histogram)

##################################################################
#                                                                #
#                        Crop from file                          #
#                                                                #
##################################################################

# Set your working directory.  Avoid spaces in your path and OneDrive
setwd("D:\\GIS\\layers")

# ------------------------------------------------------------------------------
# Import raster image
cover24 <- raster("landlayers\\LCMAP_CU_2009_V12_LCPRI.tif") 

# To crop the image
crp_ext24 <- readOGR("Chartier\\globalwatershed.shp")
#renamed "cover" for 2009 to "cover25" to help numerically organize

#cover35 <- raster("landlayer\\LCMAP_CU_2020_V12_LCPRI.tif")
#cover34 <- raster("landlayer\\LCMAP_CU_2019_V12_LCPRI.tif")
#cover33 <- raster("landlayer\\LCMAP_CU_2018_V12_LCPRI.tif")
#cover32 <- raster("landlayer\\LCMAP_CU_2017_V12_LCPRI.tif")
#cover31 <- raster("landlayer\\LCMAP_CU_2016_V12_LCPRI.tif")
#cover30 <- raster("landlayer\\LCMAP_CU_2015_V12_LCPRI.tif")
#cover29 <- raster("landlayer\\LCMAP_CU_2014_V12_LCPRI.tif")
#cover28 <- raster("landlayer\\LCMAP_CU_2013_V12_LCPRI.tif")
#cover27 <- raster(landlayer\\LCMAP_CU_2012_V12_LCPRI.tif")
#cover26 <- raster("landlayer\\LCMAP_CU_2011_V12_LCPRI.tif")

#cover25 <- raster("landlayer\\LCMAP_CU_2010_V12_LCPRI.tif")



#cover23 <- raster("landlayers\\LCMAP_CU_2008_V12_LCPRI.tif")
#crp_ext23 <- readOGR("Chartier\\globalwatershed.shp")
#cover22 <- raster("landlayers\\LCMAP_CU_2007_V12_LCPRI.tif")

#cover21 <- raster("landlayers\\LCMAP_CU_2006_V12_LCPRI.tif")
#cover20 <- raster("landlayers\\LCMAP_CU_2005_V12_LCPRI.tif")
#cover19 <- raster("landlayers\\LCMAP_CU_2004_V12_LCPRI.tif")
#cover18 <- raster("landlayers\\LCMAP_CU_2003_V12_LCPRI.tif")
#cover17 <- raster("landlayer\\LCMAP_CU_2002_V12_LCPRI.tif")
#cover16 <- raster("landlayer\\LCMAP_CU_2001_V12_LCPRI.tif")
#cover15 <- raster("landlayers\\LCMAP_CU_2000_V12_LCPRI.tif")
#cover14 <- raster("landlayers\\LCMAP_CU_1999_V12_LCPRI.tif")
#cover13 <- raster("landlayers\\LCMAP_CU_1998_V12_LCPRI.tif")
#cover12 <- raster("landlayers\\LCMAP_CU_1997_V12_LCPRI.tif")
#cover11 <- raster("landlayers\\LCMAP_CU_1996_V12_LCPRI.tif")
#cover10 <- raster("landlayers\\LCMAP_CU_1995_V12_LCPRI.tif")
#cover9 <- raster("landlayers\\LCMAP_CU_1994_V12_LCPRI.tif")
#cover8 <- raster("landlayers\\LCMAP_CU_1993_V12_LCPRI.tif")
#cover7 <- raster("landlayers\\LCMAP_CU_1992_V12_LCPRI.tif")
#cover6 <- raster("landlayers\\LCMAP_CU_1991_V12_LCPRI.tif")
#cover5 <- raster("landlayers\\LCMAP_CU_1990_V12_LCPRI.tif")
#cover4 <- raster("landlayers\\LCMAP_CU_1989_V12_LCPRI.tif")
#cover3 <- raster("landlayers\\LCMAP_CU_1988_V12_LCPRI.tif")
#cover2 <- raster("landlayers\\LCMAP_CU_1987_V12_LCPRI.tif")
#cover1 <- raster(landlayers\\LCMAP_CU_1986_V12_LCPRI.tif")
#cover0 <- raster(landlayers\\LCMAP_CU_1985_V12_LCPRI.tif")


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
crp.chartier2009 <- raster("D:\\GIS\\layers\\Chartier\\2009chartier.tif")
nrow(crp.chartier2009)
ncol(crp.chartier2009)
crp.chartier2009[1,1]
#JULY 1 CHANGES: I created a matrix first to help when defining loop later but this did not work. 
#I think my matrix is only halfway defined but I'm not entirely sure if I really need one.
m <- matrix(nrow = 5, ncol = 5)
for(i in 1:1404){
  + for(j in 1:816){
  + m[i-6,j-6] <- abs(i-j)  
  }
}
print(m)
#JULY 3 CHANGES: I tried to do the for loop without a matrix until I understand it better (or absolutely need one)

##################################################################
#                                                                #
#                   Analyze from cropped image                   #
#                                                                #
##################################################################

#crp.chartier2009 <- raster("D:\\GIS\\layers\\Chartier\\2009chartier.tif")
#crp.chartier2009 <- raster("/Volumes/T7/pa_hydro/Chartier/2009chartier.tif")

im <- list.files("/Volumes/T7/pa_hydro/Chartier", 
                 pattern = "*.tif$", 
                 full.names = TRUE, 
                 recursive = TRUE, 
                 ignore.case=TRUE, 
                 include.dirs = TRUE)
yr <- array(NA, dim = length(im))
for (i in 1:length(im)) {
     a <- str_split(im[i],"/") # splits all directory names
     b <- str_split(a[[1]][length(a[[1]])],".tif") # length(a[[1]]) goes to last element, i.e., filename, removes extension
     c <- str_split(b[[1]][1],"") # finding year
     d <- paste0(c[[1]][1], c[[1]][2], c[[1]][3], c[[1]][4])
     yr[i] <- as.numeric(d)
}
ls <- data.frame(im,yr)
rm(im,yr)

registerDoParallel(detectCores())
imperm <- foreach (q = 1:(nrow(ls)), .combine = 'rbind') %dopar% {
     ye <- as.numeric(ls$yr[q])
     img <- raster(ls$im[q])
     img <- as.matrix(img)
     cnt <- 0 # counting variable
     for(i in 1:nrow(img)){
          for(j in 1:ncol(img)){
               if (is.na(img[i,j])==FALSE) { # determines if the value is not NA
                    if (img[i,j]==1) { # sets impermeable value(s), add more here.
                         cnt <- cnt +1
                    }
               }
          }
     }
     print(c(ye,cnt)) # "prints" to output array: year, count of impermeable surface
}

imperm <- data.frame(imperm)
names(imperm) <- c('yr','area')

imperm$change <- 0
for (i in 3:nrow(imperm)) {
     imperm$change[i] <- imperm$area[i]/imperm$area[2]-1
}

ggplot(imperm) +
     geom_line(aes(x=yr,y=area)) +
     labs(x = "Year", y = "Area") + 
     xlim(1985,2015) + 
     ylim(200000,250000) + 
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1) + 
     theme(axis.text = element_text(face = "plain", size = 12))
     
ggplot(imperm) +
     geom_point(aes(x=yr,y=change)) +
     labs(x = "Year", y = "Change (since 1986)") + 
     xlim(1985,2015) + 
     ylim(0,0.1) + 
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1) + 
     theme(axis.text = element_text(face = "plain", size = 12))

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
