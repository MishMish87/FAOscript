# This script is made by: Michele Rocca (FAO-ESS)
# Created: 30th July 2015
# last update: 

# This script was done for Afghanistan, but could be use for each survey
# that require a monetary conversion factor for amounts of
# in-kind transfers.

mydata <- HHinput.df


mydata$urb_rur <- as.numeric(mydata$urb_rur)
mydata$region <- as.numeric(mydata$region)

mydata$urb_rur <- ifelse(mydata$urb_rur == 1, -1,mydata$urb_rur)
mydata <- mutate(mydata,FoodSubCode = urb_rur*region)
mydata <- mydata[,c("hhid","FoodSubCode")]

Foodinput.df <- read.spss("food.sav",use.value.labels= T, to.data.frame= T)
Foodinput.df <- mutate(Foodinput.df, KG_mv = fd_mv/fd_qty)
names(Foodinput.df)[names(Foodinput.df) == "hh_no"] <- "hhid"

# For food aid the questionnaire presents data only on the khilograms received.
# in order to have the corrispondent monetary value we will use the food file 
# and we will calculate the correspondent price of 1 kg of good received.
# the price will be disaggregated at regional and urban level.

# the following variable will be use to generate the correpondent price for kilogram
# of each comodities.

FoodAid.df <- FoodCashRaw[,c("hhid","q_10_8_1","q_10_8_2","q_10_8_3","q_10_8_4",
                             "q_10_8_5")] 

FoodAid.df$q_10_8_1[is.na(FoodAid.df$q_10_8_1)] <- 0
FoodAid.df$q_10_8_2[is.na(FoodAid.df$q_10_8_2)] <- 0
FoodAid.df$q_10_8_3[is.na(FoodAid.df$q_10_8_3)] <- 0
FoodAid.df$q_10_8_4[is.na(FoodAid.df$q_10_8_4)] <- 0
FoodAid.df$q_10_8_5[is.na(FoodAid.df$q_10_8_5)] <- 0

FoodAid.df <- subset(FoodAid.df,q_10_8_1 > 0 | q_10_8_2 > 0 |  q_10_8_3 > 0 | 
                       q_10_8_4 > 0 |  q_10_8_5 > 0 )

FoodAid.df <- merge(FoodAid.df,HHinput.df, by = "hhid", all.x = T)
FoodAid.df <- merge(FoodAid.df,mydata, by = "hhid", all.x = T )

# Vegetable oil ,item code = 32

VegOil.df <- subset(Foodinput.df,item_cod == "vegetable oil", select = c("hhid","KG_mv")) 
VegOil.df <- merge(HHinput.df,VegOil.df, by = "hhid",all.y = T)
VegOil.df <- merge(mydata,VegOil.df, by = "hhid", all.y = T)

PriceVegOil.df <- ddply(VegOil.df, .(FoodSubCode),.fun =function(s) median(s$KG_mv))
names(PriceVegOil.df)[names(PriceVegOil.df) == "V1"] <- "PriceVegOil"

rm(VegOil.df)
# Beans, item code = 6

Beans.df <- subset(Foodinput.df,item_cod == "beans", select = c("hhid","KG_mv")) 
Beans.df <- merge(HHinput.df,Beans.df, by = "hhid",all.y = T)
Beans.df <- merge(mydata,Beans.df, by = "hhid", all.y = T)

PriceBeans.df <- ddply(Beans.df, .(FoodSubCode),.fun =function(s) median(s$KG_mv))
names(PriceBeans.df)[names(PriceBeans.df) == "V1"] <- "PriceBeans"
rm(Beans.df)

# Wheat flour , item code = 2

Wheat.df <- subset(Foodinput.df,item_cod == "wheat flour", select = c("hhid","KG_mv")) 
Wheat.df <- merge(HHinput.df,Wheat.df, by = "hhid",all.y = T)
Wheat.df <- merge(mydata,Wheat.df, by = "hhid", all.y = T)

PriceWheat.df <- ddply(Wheat.df, .(FoodSubCode),.fun =function(s) median(s$KG_mv))
names(PriceWheat.df)[names(PriceWheat.df) == "V1"] <- "PriceWheat"

rm(Wheat.df)

# Salt , item code = 86

Salt.df <- subset(Foodinput.df,item_cod == "salt", select = c("hhid","KG_mv")) 
Salt.df <- merge(HHinput.df,Salt.df, by = "hhid",all.y = T)
Salt.df <- merge(mydata,Salt.df, by = "hhid", all.y = T)

PriceSalt.df <- ddply(Salt.df, .(FoodSubCode),.fun =function(s) median(s$KG_mv))
names(PriceSalt.df)[names(PriceSalt.df) == "V1"] <- "PriceSalt"

rm(Salt.df)

# Sugar , item code = 78 (white sugar), item code = 79 (brown sugar)

Sugar.df <- subset(Foodinput.df,item_cod == "white sugar" |item_cod == "brown sugar",
                   select = c("hhid","KG_mv")) 
Sugar.df <- merge(HHinput.df,Sugar.df, by = "hhid",all.y = T)
Sugar.df <- merge(mydata,Sugar.df, by = "hhid", all.y = T)

PriceSugar.df <- ddply(Sugar.df, .(FoodSubCode),.fun =function(s) median(s$KG_mv))
names(PriceSugar.df)[names(PriceSugar.df) == "V1"] <- "PriceSugar"

rm(Sugar.df)

FoodAid.df <- merge(FoodAid.df,PriceBeans.df, by = "FoodSubCode",all.x = T)
FoodAid.df <- merge(FoodAid.df,PriceSalt.df, by = "FoodSubCode",all.x = T)
FoodAid.df <- merge(FoodAid.df,PriceSugar.df, by = "FoodSubCode",all.x = T)
FoodAid.df <- merge(FoodAid.df,PriceVegOil.df, by = "FoodSubCode",all.x = T)
FoodAid.df <- merge(FoodAid.df,PriceWheat.df, by = "FoodSubCode",all.x = T)

# Wheat
FoodAid.df$q_10_8_1 <- ifelse(FoodAid.df$q_10_8_1 > 0,
                              FoodAid.df$q_10_8_1*FoodAid.df$PriceWheat,FoodAid.df$q_10_8_1)
# Vegetable Oil
FoodAid.df$q_10_8_2 <- ifelse(FoodAid.df$q_10_8_2 > 0,
                              FoodAid.df$q_10_8_2*FoodAid.df$PriceVegOil,FoodAid.df$q_10_8_2)
# Beans
FoodAid.df$q_10_8_3 <- ifelse(FoodAid.df$q_10_8_3 > 0,
                              FoodAid.df$q_10_8_3*FoodAid.df$PriceBeans,FoodAid.df$q_10_8_3)
# Sugar
FoodAid.df$q_10_8_4 <- ifelse(FoodAid.df$q_10_8_4 > 0,
                              FoodAid.df$q_10_8_4*FoodAid.df$PriceSugar,FoodAid.df$q_10_8_4)
# Salt
FoodAid.df$q_10_8_5 <- ifelse(FoodAid.df$q_10_8_5 > 0,
                              FoodAid.df$q_10_8_5*FoodAid.df$PriceSalt,FoodAid.df$q_10_8_5)

FoodAid.df$FoodAid <- rowSums(FoodAid.df[,c("q_10_8_1","q_10_8_2","q_10_8_3","q_10_8_4",
                                            "q_10_8_5")])
FoodAid.df <- FoodAid.df[,c("hhid","FoodAid")]

# In the questionnaire the amounts is on monthly base, as usual i will put it on yearly base

FoodAid.df$FoodAid <- FoodAid.df$FoodAid*365
