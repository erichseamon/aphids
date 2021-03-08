library(dplyr)
library(reshape2)

aphids_2011 <- read.csv("data/sweepnet_2011_final_WW_rev2.csv", header=TRUE, strip.white = TRUE)
aphids_2012 <- read.csv("data/sweepnet_2012_final_WW_rev2.csv", header=TRUE, strip.white = TRUE)
aphids_2013 <- read.csv("data/sweepnet_2013_final_WW_rev2.csv", header=TRUE, strip.white = TRUE)
aphids_2014 <- read.csv("data/sweepnet_2014_final_WW_rev2.csv", header=TRUE, strip.white = TRUE)
l <- list(aphids_2011, aphids_2012, aphids_2013, aphids_2014)

library(gtools)
aphids_11_14 <- do.call(smartbind,l)


#aphids_11_14 <- read.csv("./finaldata/2011_2014aphidfinalresult.csv", header=TRUE, strip.white = TRUE)
aphids_11_14 <- aphids_11_14[,c(1:ncol(aphids_11_14)-1)]
#aphids_11_14$site_year <- paste(aphids_11_14$Year, "_", aphids_11_14$SiteID, sep="")
aphids_11_14$site_year <- aphids_11_14$SiteID
aphids_11_14 <- aphids_11_14[,c(ncol(aphids_11_14), 1:ncol(aphids_11_14)-1)]

cdl_lookup <- read.csv("cdl_lookup/cdl_lookup.csv")
cdl_lookup$Value <- paste("X", cdl_lookup$Value, sep="")
column <- colnames(aphids_11_14)
column1 <- as.data.frame(column[5:length(column)])
colnames(column1) <- c("Value")
column1$ID <- 1:nrow(column1)
column2 <- merge(cdl_lookup, column1, by="Value")
column2 <- column2[order(column2$ID), ]
column_refined <- c(column[1:4], as.character(column2$Description))
colnames(aphids_11_14) <- column_refined

#aphids_11_14 <- melt(data = aphids_11_14, id.vars = c("site_year", "ring", "Year", "SiteID"), measure.vars = c("Winter.Wheat","Lentils"                  ,"Peas"       ,             
#"Barley" ,                  "Spring.Wheat"   ,          "Pasture.Grass"        ,    "Developed.Open.Space"    ,
#"Evergreen.Forests" ,       "Dry.Beans"         ,       "Fallow.Idle.Cropland" ,    "Developed.Low.Intensity" ,
#"Grassland.Herbaceous" ,    "Shrubland"         ,       "Oats"   ,                  "Herbaceous.Wetlands"  ,   
#"Sod.Grass.Seed"       ,    "Alfafa"             ,      "Deciduous.Forest"    ,     "Pasture.Hay" ,            
#"Other.Hay.Non.Alfalfa" ,   "Developed.Med.Intensity" , "Apples"           ,        "Open.Water"  ,            
#"Developed.High.Intensity" ,"Triticale"        ,        "Canola"      ,             "Sweet.Corn"    ,          
#"Potatoes"     ,            "Asparagus"         ,       "Corn"       ,              "Sugar.Beets"   ,          
#"Watermelon"    ,           "Woodly.Wetlands"      ,    "Grapes"     ,              "Flaxseed"     ,           
#"Herbs"        ,            "Cherries"             ,    "Carrots"   ,               "Other.Crops"   ,          
#"Barren"        ,           "Sunflower"  ,              "Rapeseed"       ))         


aphids_location <- read.csv("./finaldata/sweepnet_DecDeg_modified_v4.csv", header=TRUE, strip.white = TRUE)
aphids_location <- aphids_location[,c(1:ncol(aphids_location)-1)]
aphids_location$site_year <- paste(aphids_location$Year, "_", aphids_location$SiteID, sep="")
aphids_location <- aphids_location[,c(ncol(aphids_location), 1:ncol(aphids_location)-1)]

aphid_merge <- inner_join(aphids_location, aphids_11_14, by=c("site_year"))
#aphid_merge <- subset(aphid_merge, value != "#N/A")

#aphid_merge <- na.omit(aphid_merge)
aphid_merge$variable <- as.factor(aphid_merge$variable)
aphid_merge$value <- as.numeric(as.character(aphid_merge$value))

aphid_data_wide <- aphid_merge

aphid_data_long <- gather(aphid_merge, Crop, Pct, `Spring Wheat`:`Mint`, factor_key=TRUE)

write.csv(aphid_data_wide, file = "./finaldata/aphid_data_wide_feb2021.csv", row.names=FALSE)
write.csv(aphid_data_long, file = "./finaldata/aphid_data_long_feb2021.csv", row.names=FALSE)




#aphid_merge <- subset(aphid_merge, ring < 4500)

#aphid_merge <- subset(aphid_merge, ring == 500 & Year.x == 2013 )
#aphid_merge <- subset(aphid_merge, variable == "Spring.Wheat" | variable == "Winter.Wheat")


#--end

#--analysis


plot = ggplot(aphid_merge) +
  geom_bar(aes(x=SiteID.x, y=value, fill = variable), position="dodge", stat="identity") +
  theme(axis.text.x=element_text(angle = 90, hjust=1))

print(plot)




p <- ggplot(data = aphids_merge, aes(x = Year.x, y = TotAph)) + geom_point()
q <- p + facet_wrap(~County)

cycl6 <- subset(mpg, cyl == 6)
q + geom_point(data = cycl6, color = "red")





#--model


my.model <- ezANOVA(data = aphids_merge, dv = .(DV), wid = .(Subject), within = .(Treatment, Time), between = .(Sex), type = 3, detailed = F, return_aov = T) # the last two arguments are optional







