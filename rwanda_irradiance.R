#####
#
#   Irradiance in Rwanda
#
#####
##### Resources
#
# I made use of the excellent book of Lamigueiro for this R script, read it if you want to do some reseach with GEO data:
#   Lamigueiro, Oscar Perpiñán. Displaying Time Series, Spatial, and Space-time Data with R
#   Boca Raton: CRC, 2014.
#   (You can find his original code on GitHub: https://github.com/oscarperpinan/spacetime-vis)
#
##### Input data
#
# From here I got the solar irradiation files (for free):
# https://wui.cmsaf.eu/safira/action/viewProduktDetails?id=14389_15619_17250_17954_18118_18982
#   They show the surface incoming shortwave radiation, I only requested monthly means for the area of Rwanda in 2011, not the complete dataset.
#   The data is a combination of information from the MSG and Polar orbiting satellites. Take NetCDF4 as output.
#
# DEM files (elevation maps) I got from here (also for free):
# http://srtm.csi.cgiar.org/
#
#####

##### Load packages
if(!require(RColorBrewer)) { install.packages("RColorBrewer"); require(RColorBrewer)}
if(!require(raster)) { install.packages("raster"); require(raster)}
if(!require(rasterVis)) { install.packages("rasterVis"); require(rasterVis)}
if(!require(maps)) { install.packages("maps"); require(maps)}
if(!require(mapdata)) { install.packages("mapdata"); require(mapdata)}
if(!require(maptools)) { install.packages("maptools"); require(maptools)}
if(!require(sp)) { install.packages("sp"); require(sp)}
if(!require(latticeExtra)) { install.packages("latticeExtra"); require(latticeExtra)}
if(!require(ncdf4)) { install.packages("ncdf4"); require(ncdf4)} #


# Path to folder where solar irradiation files are stored:
tmp <- "~/R/Solar/Rwanda"
filesCMSAF <- dir(tmp, pattern="SISmm")
SISmm <- stack(paste(tmp, filesCMSAF, sep="/"))
# CM-SAF data is average monthly irradiance (W/m2). Multiply by 24
# hours and days in a month to obtain monthly irradiation in Wh/m2
daysMonth <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
SISmm <- SISmm * 24 * daysMonth
# Monthly irradiation in kWh/m2 
SISm <- SISmm / 1000 
# Annual average sum
SISav <- sum(SISm)
# Write raster data to file 
writeRaster(SISav, file="SISav", overwrite=TRUE) # Will overwrite existing file

# Load map to plot as layer of irradiance:
ext <- as.vector(extent(SISav)) # Extension coordinates if Rwanda
boundaries <- map("worldHires", #you can add: ,"Rwanda". To only have Rwanda map on plot.
                  xlim=ext[1:2], ylim=ext[3:4],
                  plot=FALSE)
boundaries <- map2SpatialLines(boundaries, proj4string=CRS(projection(SISav)))

# Load cities of Rwanda (only captital)
cities_rwanda <- world.cities[world.cities$country.etc == "Rwanda" & world.cities$capital == 1,]
# Now class data.frame, make spatial:
# (I could not find a better way to plot the data)
coordinates(cities_rwanda) <- c("long", "lat")

# Load cities of Rwanda (smaller cities, population >80,000)
small_cities_rwanda <- world.cities[world.cities$country.etc == "Rwanda" & world.cities$capital == 0 & world.cities$pop > 80000,]
# Now class data.frame, make spatial:
coordinates(small_cities_rwanda) <- c("long", "lat")


# First unzip all elevation .tif files (I did this in explorer)
# All DEM .tif files in folder:
dem_files <- dir(tmp, pattern = "\\.tif")
DEM_total <- mosaic(raster(paste(tmp, dem_files[1], sep = "/")), 
                    raster(paste(tmp, dem_files[2], sep = "/")), 
                    fun = mean)

slope <- terrain(DEM_total, "slope")
aspect <- terrain(DEM_total, "aspect")
hs <- hillShade(slope=slope, aspect=aspect, angle=20, direction=30) # You can play around here with the location of the sun to get shades

# hillShade theme GrTheme (gray colors) and semitransparency of 0.6
hsTheme <- modifyList(GrTheme(), list(regions=list(alpha=0.6)))

# Use nice color palette:
colr <- colorRampPalette(rev(brewer.pal(11, 'Spectral'))) #reverse order of spectral palette (see: http://moderndata.plot.ly/create-colorful-graphs-in-r-with-rcolorbrewer-and-plotly/)

# Plot
levelplot(SISav, 
          main = "Annual irradiation in Rwanda 2011 [kWh/m2]", #Plot title
          panel=panel.levelplot.raster, 
          margin=F,                     # suppress marginal graphics
          colorkey=list(
            space='right'               # plot legend at right side
          ),
          col.regions=colr              # colour ramp
         ) +
  levelplot(hs,
            par.settings=hsTheme,       # hillShade theme
            maxpixels=1e6) +
  layer(sp.lines(boundaries, lwd=2)) +  # Line weight
  layer(sp.points(cities_rwanda,        # Plot Kigali location (point)
                  pch = 18,             # Overview of different points you can use: http://www.endmemo.com/program/R/pchsymbols.php
                  col = "black",        # Color of point
                  cex = 1)) +           # Size of point
  layer(sp.pointLabel(cities_rwanda,    # Plot Kigali name
                      labels = cities_rwanda$name, 
                      col = "black", 
                      cex = 1)) + 
  layer(sp.points(small_cities_rwanda,  # Plot smaller cities (>80,000) (points on map)
                  pch = 20,            
                  col = "black",       
                  cex = .8)) +
  layer(sp.pointLabel(small_cities_rwanda,# Plot smaller cities (names)
                      labels = small_cities_rwanda$name, 
                      col = "black", 
                      cex = .8))