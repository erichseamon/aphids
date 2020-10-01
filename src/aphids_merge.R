library(dplyr)
library(reshape2)

aphids_2011 <- read.csv("data/sweepnet_2011_final.csv", header=TRUE, strip.white = TRUE)
aphids_2012 <- read.csv("data/sweepnet_2012_final.csv", header=TRUE, strip.white = TRUE)
aphids_2013 <- read.csv("data/sweepnet_2013_final.csv", header=TRUE, strip.white = TRUE)
aphids_2014 <- read.csv("data/sweepnet_2014_final.csv", header=TRUE, strip.white = TRUE)
l <- list(aphids_2011, aphids_2012, aphids_2013, aphids_2014)

library(gtools)
aphids_combined <- do.call(smartbind,l)


aphids_11_14 <- read.csv("./finaldata/2011_2014aphidfinalresult.csv", header=TRUE, strip.white = TRUE)
aphids_11_14 <- aphids_11_14[,c(1:ncol(aphids_11_14)-1)]
aphids_11_14$site_year <- paste(aphids_11_14$Year, "_", aphids_11_14$SiteID, sep="")
aphids_11_14 <- aphids_11_14[,c(ncol(aphids_11_14), 1:ncol(aphids_11_14)-1)]

aphids_11_14 <- melt(data = aphids_11_14, id.vars = c("site_year", "ring", "Year", "SiteID"), measure.vars = c("Winter.Wheat","Lentils"                  ,"Peas"       ,             
"Barley" ,                  "Spring.Wheat"   ,          "Pasture.Grass"        ,    "Developed.Open.Space"    ,
"Evergreen.Forests" ,       "Dry.Beans"         ,       "Fallow.Idle.Cropland" ,    "Developed.Low.Intensity" ,
"Grassland.Herbaceous" ,    "Shrubland"         ,       "Oats"   ,                  "Herbaceous.Wetlands"  ,   
"Sod.Grass.Seed"       ,    "Alfafa"             ,      "Deciduous.Forest"    ,     "Pasture.Hay" ,            
"Other.Hay.Non.Alfalfa" ,   "Developed.Med.Intensity" , "Apples"           ,        "Open.Water"  ,            
"Developed.High.Intensity" ,"Triticale"        ,        "Canola"      ,             "Sweet.Corn"    ,          
"Potatoes"     ,            "Asparagus"         ,       "Corn"       ,              "Sugar.Beets"   ,          
"Watermelon"    ,           "Woodly.Wetlands"      ,    "Grapes"     ,              "Flaxseed"     ,           
"Herbs"        ,            "Cherries"             ,    "Carrots"   ,               "Other.Crops"   ,          
"Barren"        ,           "Sunflower"  ,              "Rapeseed"       ))         


aphids_location <- read.csv("./finaldata/sweepnet_DecDeg_modified_v4.csv", header=TRUE, strip.white = TRUE)
aphids_location <- aphids_location[,c(1:ncol(aphids_location)-1)]
aphids_location$site_year <- paste(aphids_location$Year, "_", aphids_location$SiteID, sep="")
aphids_location <- aphids_location[,c(ncol(aphids_location), 1:ncol(aphids_location)-1)]

aphid_merge <- inner_join(aphids_location, aphids_11_14, by=c("site_year"))
aphid_merge <- subset(aphid_merge, value != "#N/A")

aphid_merge <- na.omit(aphid_merge)
aphid_merge$variable <- as.factor(aphid_merge$variable)
aphid_merge$value <- as.numeric(as.character(aphid_merge$value))

write.csv(aphid_merge, file = "./finaldata/aphid_merge_2.csv", row.names=FALSE)






aphid_merge <- subset(aphid_merge, ring < 4500)

aphid_merge <- subset(aphid_merge, ring == 500 & Year.x == 2013 )
#aphid_merge <- subset(aphid_merge, variable == "Spring.Wheat" | variable == "Winter.Wheat")





plot = ggplot(aphid_merge) +
  geom_bar(aes(x=SiteID.x, y=value, fill = variable), position="dodge", stat="identity") +
  theme(axis.text.x=element_text(angle = 90, hjust=1))

print(plot)









my.model <- ezANOVA(data = aphids_merge, dv = .(DV), wid = .(Subject), within = .(Treatment, Time), between = .(Sex), type = 3, detailed = F, return_aov = T) # the last two arguments are optional






p <- ggplot(data = aphids_merge, aes(x = Year.x, y = TotAph)) + geom_point()
q <- p + facet_wrap(~County)

cycl6 <- subset(mpg, cyl == 6)
q + geom_point(data = cycl6, color = "red")


