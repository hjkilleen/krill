# ===================================================================================================
# Setup
# ===================================================================================================
require(maptools) # readShapeLines ft to read shape file of shoreline
require(rgeos) # gDistance function to calc min dist to shore and readWKT (takes strings, converts to geospatial points)
require(rgdal) # needed for rgeos "spTransform" method, which transforms datum projections
require(geosphere) # for bearing() function and other useful geographic functions
require(raster)
# Projection Strings:
	# use to get spatial points for lat/lon combos
	wgs.84    <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
	# Planar CRS Projection string:
	epsg.2767 <- "+proj=lcc +lat_1=39.83333333333334 +lat_2=38.33333333333334 +lat_0=37.66666666666666 +lon_0=-122 +x_0=2000000 +y_0=500000 +ellps=GRS80 +units=m +no_defs"
# ===================================================================================================
# Data Import
# ===================================================================================================

# Coastline file: read shape file in WGS 84 projection
# coast.line <- readShapeLines("/Users/Connor/Documents/Graduate School/Dibble_Research/GIS_data/Western/Western.shp",
# 	CRS(wgs.84)) # apparently that is depracated
coast.line <- readOGR(dsn = "/Users/Connor/Documents/Graduate School/Dibble_Research/GIS_data/Western", layer = "Western")
coast.line@data$id = rownames(coast.line@data) # set id column in "data" slot, which contains attributes
coast.line.f <- fortify(coast.line, region = "id") # melt geometries table: region selects the feature attribute name to tag each row in the new data frame. 
Wseaboard.df <- left_join(coast.line.f, coast.line@data, by = "id") # join attribute table and geometry table

save(Wseaboard.df, file = '/Users/connor/Documents/Graduate School/Dibble_Research/Krill/Data/Wseaboard_df')