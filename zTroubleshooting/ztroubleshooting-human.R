### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
###   land ownership and roads data
###       (are a pain in the @$$)
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


    ### newp####
        
        
        ## IN PROGRESS ##
       unique(clip@data$GENDESCRIP)
       unique(clip@data$OWNCLASS)
       unique(clip@data$PROPTYPE)
       summary(clip@data$GRAZING_AC)
       
       nrow(clip@data)
       length(which(is.na(clip@data$GENDESCRIP))) # 4 NAs
       length(which(is.na(clip@data$OWNCLASS))) # 0 NAs! oh wait...
       length(which(clip@data$OWNCLASS == "Undetermined")) # yeah. 30289 undetermined
       length(which(clip@data$GENDESCRIP == "Unk")) # 30290, basically the same
       length(which(is.na(clip@data$PROPTYPE))) # ok this just for pvt i think
       
       plot(clip, col = clip@data$GENDESCRIP, border = NA)
       
       library(ggplot2)

        
        
        
        ## CUT FROM LOOP ##
        
         # clip cadastral to home ranges
         clip <- gIntersection(cadi, hrs.crs, byid = TRUE)
          # this does not retain attribute data, no way to tell land ownership
         
         # store
         writeOGR(clip, dsn = "../GIS/Shapefiles/Land",
           layer = outname,
           driver = "ESRI Shapefile",
           overwrite_layer = TRUE)
         # this doesn't work because clip makes a spatialpolygons
         # but you  need a spatialpolyDATAFRAME for this (so similar issue of losing data)
        
        
         
         
        ## OTHER CUTS  ##        
        test <- files.cad[1]
        test
        nchar(test)
        substr(test, nchar(test)-9, nchar(test)-8)
        
               ggplot(clip) +
         geom_polygon(fill = "GENDESCRIP")
               
        
        # crop cadastral to extent of home range area
         cadcrop <- crop(cadi, hrs.crs) # just to extent, would be better to
        
        unique(cadscrop[[1]]@data$WHATEVER)

        attributes(clip)
      
      
      # read them in and stack them
      
      # match extent (and resolution? check ?crop) to hrs
      
      # crop to hrs
      
      