### CERATE ZONES FOR SENSITIVITY TESTING ###

##LOAD LIBS

library(tidyverse)
library(raster)
library(dplyr)

##LOAD CLASSIFIED RASTER 

ras = stack('for_analysis_zones.tif')

plot(ras)

e_20 = ras[[1]]
s_6 = ras[[2]]
a_4 = ras[[3]]
class = ras[[4]]

#isolate type cover classes - open and mature

class[class == 0] = NA

plot(class)

mature = class

mature[mature != 3] = NA

plot(mature)

open = class

open[open != 4] = NA

plot(open)

#Elevatsion classes, 1200m and 2000m

e_20 = mask(e_20, class)

plot(e_20)

high = e_20

high[high > 17] = NA
high[high < 14] = NA

plot(high)

low = e_20

low[low > 8] = NA
low[low < 4] = NA

plot(low)

#slope classes

s_6 = mask(s_6, class)

plot(s_6)

flat = s_6

flat[flat != 1] = NA

plot(flat)

steep = s_6

steep[steep != 4] = NA

plot(steep)

#aspect classes

a_4 = mask(a_4, class)

north = a_4
east = a_4
south = a_4
west = a_4

north[north != 1] = NA
south[south != 3] = NA
east[east != 2] = NA
west[west != 4] = NA

plot(north)
plot(south)
plot(east)
plot(west)

slopes = stack(steep, flat)

aspects = stack(north, east, south, west)

covers = stack(open, mature)

#loop for high elevation 
high_class = list()

for (i in c(1:4)) {
  
  asp = aspects[[i]]
  high_class[[i]] = list()
  
    for (j in c(1:2)) {
      
      cov = covers[[j]]
      high_class[[i]][[j]] = list()
      
        for (k in c(1:2)) {
          
          slo = slopes[[k]]
          
          r_1 = mask(high, asp)
          r_2 = mask(r_1, cov)
          out = mask(r_2, slo)
          
          #plot(out)
          
          out = sampleRandom(out,3000, asRaster = TRUE)
          
          
          high_class[[i]][[j]][[k]] = out
          
          print(cellStats(out, 'sum'))
          
        }
      high_class[[i]][[j]] = stack(high_class[[i]][[j]])
    }
  
  high_class[[i]] = stack(high_class[[i]])
  
}

high_class = stack(high_class)

plot(high_class)

#loop for high elevation 
low_class = list()

for (i in c(1:4)) {
  
  asp = aspects[[i]]
  low_class[[i]] = list()
  
  for (j in c(1:2)) {
    
    cov = covers[[j]]
    low_class[[i]][[j]] = list()
    
    for (k in c(1:2)) {
      
      slo = slopes[[k]]
      
      r_1 = mask(low, asp)
      r_2 = mask(r_1, cov)
      out = mask(r_2, slo)
      
      plot(out)
      
      
      low_class[[i]][[j]][[k]] = out
      
      print(cellStats(out, 'sum'))
      
    }
    low_class[[i]][[j]] = stack(low_class[[i]][[j]])
  }
  
  low_class[[i]] = stack(low_class[[i]])
  
}

low_class = stack(low_class)

writeRaster(low_class, 'low_elevations.tif')
writeRaster(high_class, 'high_elevations.tif')






