
###################################################################################################
################################### Data provided by Arasul#######################################
##################################################################################################

### Making trends and seasonality analysis


## Select dates above 2000 changing to time series. 
rain_all <- subset(rain_all, date > as.Date("1999-01-01") )


rain_all[,-1] <- as.data.frame(lapply(rain_all[,-1], function(x) ts(x, start=c(1999, 1), frequency=12 )))

### Fixing NA in magude time series for decompostion. 
rain_all$rain_magude <- na.approx(rain_all$rain_magude)



## Plotting decompose time series one by one
plot(decompose(rain_all$rain_panjane))
plot(decompose(rain_all$rain_motaze))
plot(decompose(rain_all$rain_mapulanguene))
plot(decompose(rain_all$rain_magude))


## Decomposing all time series into tren, seasonal and random
decom <- lapply(rain_all, FUN=decompose)


## Plotting all graphs in a row. 

rain_all$year <- as.factor(year(rain_all$date))
rain_all$month <- as.factor(month(rain_all$date))
# rain_all$date <- format(rain_all$date, "%b-%Y")



ggplot(rain_all, aes(month, group=year, color=year)) + geom_line(aes(y=rain_magude)) + 
  labs(title="Rainfall Magude", y="Rainfall (mm/day)", x="Month")+ theme(axis.text.x = element_text(angle = 90))


ggplot(rain_all, aes(month, group=year, color=year)) + geom_line(aes(y=rain_panjane)) + 
  labs(title="Rainfall Panjane", y="Rainfall (mm/day)", x="Month")+ theme(axis.text.x = element_text(angle = 90))
# gplot(rain_motaze[rain_motaze$date > "jan-14",], aes(x=date, y=mm_rain))+geom_point()

ggplot(rain_all, aes(month, group=year, color=year)) + geom_line(aes(y=rain_mapulanguene)) + 
  labs(title="Rainfall Mapulanguene", y="Rainfall (mm/day)", x="Month")+ theme(axis.text.x = element_text(angle = 90))


ggplot(rain_all, aes(month, group=year, color=year)) + geom_line(aes(y=rain_motaze)) + 
  labs(title="Rainfall Motaze", y="Rainfall (mm/day)", x="Month")+ theme(axis.text.x = element_text(angle = 90))




#####################################################################################################
################# Comparison of river discharge and Rainfall ########################################
#####################################################################################################
discharge$year <- year(discharge$date)
discharge$month <- month(discharge$date)

rain_all$year <- as.numeric(as.character(rain_all$year))

dis_rain <- merge( discharge[discharge$year >1999, ], rain_all[rain_all$year >1999,], by="date")


ggplot(data=dis_rain, aes(x=date, group=1))  + geom_line(aes(y=rain_magude, color="blue"))+geom_line( aes( y=discharge, color="red"))+
  labs(title="Rainfall vs river discharge", y="", x="Month")+ theme(axis.text.x = element_text(angle = 90))



#--------------------------------------------------------------------------------------------------



###################################################################################################
##################### Data provided by Xinavane Tongaat Hullet sugar cane plantation###############
###################################################################################################

############################ Ploting rainfall by year#############################################
##################################################################################################

rain_xinavane$month <- factor(rain_xinavane$month, levels=c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))

rain_xinavane <- rain_xinavane[rain_xinavane$year > 2011, ]
rain_xinavane <- rain_xinavane[!is.na(rain_xinavane$year), ]
rain_xinavane$year <- as.factor(rain_xinavane$year)
rain_xinavane$year_code <- ifelse(rain_xinavane$year == 2015, rain_xinavane$year_code <- 1, rain_xinavane$year_code <- 0 )
rain_xinavane$year_code <- as.factor(rain_xinavane$year_code)

ggplot(rain_xinavane, aes(x=month, group=year, color=year))+geom_line(aes(y=rain, size=year_code))+
  labs(title="Mean rainfall (aprox. 15km from district main city)", y="Rainfall (mm/month)", x="Month")+
  theme( text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1))+ scale_size_manual(values=c(0.5, 2.0), guide = FALSE)





############################ Ploting rainfall trends #############################################
#################################################################################################

## Creating a data variable to sort he data out to create time series
rain_xinavane <- rain_xinavane[order(rain_xinavane$year > 1990),]
rain_xinavane$day <- rep(15, length(rain_xinavane[,1]))
rain_xinavane$date <- paste( rain_xinavane$day, rain_xinavane$month,rain_xinavane$year, sep="-")
rain_xinavane$date <- as.Date( rain_xinavane$date, "%d-%b-%Y")

## Ordering the data
rain_xinavane <- rain_xinavane[order(rain_xinavane$date),]

## Creating time series with monthly periodicity
rain_xinavane$rain <- ts(rain_xinavane$rain, start=c(1967, 1), frequency=12 )

## Plotting time series
plot(decompose(rain_xinavane$rain))




############################ Ploting temperature by year #######################################
###############################################################################################

temp$month <- factor(temp$month, levels=c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))

temp <- temp[as.numeric(as.character(temp$year)) > 2011, ]
temp$year <- as.factor(temp$year)
temp$temp_min <- as.numeric(as.character(temp$temp_min))
temp$temp_max <- as.numeric(as.character(temp$temp_max))
temp$year_code <- ifelse(temp$year == 2015, temp$year_code <- 0.50, temp$year_code <- 0 )
temp$year_code <- as.factor(temp$year_code)

ggplot(temp, aes(x=month, group=year, color=year))+geom_line(aes(y=temp_min,, size=year_code))+geom_line(aes(y=temp_max,, size=year_code))+
  labs(title="Temperature (aprox. 15km away from district main city)", y="Maximum and Minimum temperature (°C)", x="Month")+
  theme( text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1))+ scale_size_manual(values=c(0.5, 1.5), guide=FALSE)


############################ Ploting temperature trends #######################################
###############################################################################################



## Creating a data variable to sort he data out to create time series
temp$day <- rep(15, length(temp[,1]))
temp$date <- paste( temp$day, temp$month,temp$year, sep="-")
temp$date <- as.Date( temp$date, "%d-%b-%Y")

summary(temp)
## Ordering the data
temp <- temp[order(temp$date),]
temp <- temp[-length(temp$date),]
temp <- temp[157:nrow(temp),]
## Creating time series with monthly periodicity
temp$temp_min <- ts(temp$temp_min, start=c(1980, 1), frequency=12 )

## Plotting time series
plot(decompose(temp$temp_min))

temp$temp_max <- ts(temp$temp_max, start=c(1980, 1), frequency=12 )

## Plotting time series
plot(decompose(temp$temp_max))


############################ Ploting humidity by year #######################################
###############################################################################################


## Plotting humidity by year

humd$month <- factor(humd$month, levels=c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))
humd$year <- as.factor(humd$year)
humd$humd_8 <- as.numeric(as.character(humd$humd_8))
humd$humd_14 <- as.numeric(as.character(humd$humd_14))

ggplot(humd, aes(x=month, group=year, color=year))+geom_line(aes(y=humd_8))+geom_line(aes(y=humd_14))+
  labs(title="Humidity- Xinavane", y="Humidity at 8:00 and at 14:00", x="Month")+
  theme( text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1))


############################ Ploting humidity trends #######################################
###############################################################################################


## Transforming month in english to spanish
humd$month <- str_replace_all(humd$month, "dec", "dic")
humd$month <- str_replace_all(humd$month, "jan", "ene")
humd$month <- str_replace_all(humd$month, "apr", "abr")
humd$month <- str_replace_all(humd$month, "aug", "ago")

## Creating a data variable to sort he data out to create time series
humd$day <- rep(15, length(humd[,1]))
humd$date <- paste( humd$day, humd$month,humd$year, sep="-")
humd$date <- as.Date( humd$date, "%d-%b-%Y")

## Ordering the data
humd <- humd[order(humd$date),]

## Creating time series with monthly periodicity
humd$humd_8 <- ts(humd$humd_8, start=c(1980, 1), frequency=12 )

## Plotting time series
plot(decompose(humd$humd_8))

humd$humd_14 <- ts(humd$humd_14, start=c(1980, 1), frequency=12 )

## Plotting time series
plot(decompose(humd$humd_14))

#####################################################################################################
################# Comparison between Xinavane and CHIRPS ########################################
#####################################################################################################


rain_xinavane$month <- month(rain_xinavane$date)
rain_xinavane$year <- as.numeric(rain_xinavane$year)
rains <- merge( rain_xinavane, rain_chirps, by=c("year", "month"))
rains$rain <- as.numeric(rains$rain)

ggplot(data=rains, aes(x=date.x, group=1))  + geom_line(aes(y=rain, color="var1"))+geom_line( aes( y=chirps, color="var2"))+
  scale_linetype_manual( values = c("var1"=1,"var2"=2),name="")+
  scale_color_discrete(name="", labels=c("Xinavane gauge", "CHIRPS"))+
  labs(title="Rainfall comparison: CHIRPS vs Xinavane gauge", y="Rainfall (mm/month)", x="Month")+ theme(axis.text.x = element_text(angle = 90))


#####################################################################################################
################# Comparison between Xinavane and CMORPH ########################################
#####################################################################################################

### Need to change units
rain_xinavane$month <- month(rain_xinavane$date)
rain_xinavane$year <- as.numeric(rain_xinavane$year)
rains2 <- merge( rain_xinavane, rain_cmorph, by=c("year", "month"))
rains2$rain <- as.numeric(rains2$rain)

ggplot(data=rains2, aes(x=date.x, group=1))  + geom_line(aes(y=rain, color="var1"))+geom_line( aes( y=chirps, color="var2"))+
  scale_linetype_manual( values = c("var1"=1,"var2"=2),name="")+
  scale_color_discrete(name="", labels=c("Xinavane gauge", "CMORPH"))+
  labs(title="Rainfall comparison: CMORPH vs Xinavane gauge", y="Rainfall (mm/month)", x="Month")+ theme(axis.text.x = element_text(angle = 90))




