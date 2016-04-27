
### The present script reads the Climate data (rainfall) for the 4 sentinel sites in the district run
## by Arasul


###################################################################################################
################################### Data provided by Arasul#######################################
##################################################################################################
#/Users/Lucia/Google Drive/MALTEM Entomology_NEW/5. Surveillance/Climate in Magude/scripts
# Read in Xinavane rain data boobs
rain_xinavane <- read_excel("/Users/Lucia/Google Drive/MALTEM Entomology_NEW/5. Surveillance/Climate in Magude/data/Xinavane/Xinavane Long Term Met data.xls",
                            skip=3, 
                            sheet="Rainfall")
### Giving the empty first column name a name
names(rain_xinavane)[1] <- "year"

## Converto to numeric the first column
rain_xinavane$year <- as.numeric(rain_xinavane$year)

rain_xinavane <- rain_xinavane[!is.na(rain_xinavane$year),]

# Remove unecessary columns
rain_xinavane$TOTAL <- NULL
rain_xinavane <- rain_xinavane[,-which(names(rain_xinavane) == "")]

# Use the gather function from tidyr to convert our data from wide to long
xinavane <- gather(data = rain_xinavane, 
                   key = month,
                   value = rain,
                   JAN : DEC)


## Reading all data sets from source
rain_motaze <- read.csv("data/rain_motaze.csv", sep=";")[173:693, 2:3]
rain_mapulanguene <- read.csv("data/rain_mapulanguane.csv", sep=";")[66:586, 2:3]
rain_panjane <- read.csv("data/rain_panjane.csv", sep=";")[186:709, 2:3]
rain_magude <- read.csv("data/rain_magude.csv", sep=";")[, 2:3]
discharge <- read.csv("data/discharge_magude.csv", sep=";")[208:740, 2:3]


### How do you tell R the language of the dates in your files?
##Transforming to numeric format and 3 decimals
rain_motaze$mm_rain <- as.numeric(rain_motaze$mm_rain, length=2)
rain_mapulanguene$mm_rain <- as.numeric(rain_mapulanguene$mm_rain, length=2)
rain_panjane$mm_rain <- as.numeric(rain_panjane$mm_rain, length=2)
rain_magude$mm_rain <- as.numeric(rain_magude$mm_rain, length=2)
discharge$discharge <- as.numeric(discharge$discharge, length=2)


### Homogenizing date data format to lowcase, chaging date language manually and changing date format 
## for each of the sentinel sites
rain_motaze$date <- tolower(rain_motaze$date)
rain_motaze$date <- str_replace_all(rain_motaze$date, " ", "-")
rain_motaze$date<-as.Date(rain_motaze$date, "%d-%b-%y")


rain_mapulanguene$date <- tolower(rain_mapulanguene$date)
rain_mapulanguene$date <- str_replace_all(rain_mapulanguene$date, " ", "-")
# rain_mapulanguene$date <- str_replace_all(rain_mapulanguene$date, "dec", "dic")
# rain_mapulanguene$date <- str_replace_all(rain_mapulanguene$date, "jan", "ene")
# rain_mapulanguene$date <- str_replace_all(rain_mapulanguene$date, "apr", "abr")
# rain_mapulanguene$date <- str_replace_all(rain_mapulanguene$date, "aug", "ago")
rain_mapulanguene$date<-as.Date(rain_mapulanguene$date, "%d-%b-%y")


rain_panjane$date <- tolower(rain_panjane$date)
rain_panjane$date <- str_replace_all(rain_panjane$date, " ", "-")
# rain_panjane$date <- str_replace_all(rain_panjane$date, "dec", "dic")
# rain_panjane$date <- str_replace_all(rain_panjane$date, "jan", "ene")
# rain_panjane$date <- str_replace_all(rain_panjane$date, "apr", "abr")
# rain_panjane$date <- str_replace_all(rain_panjane$date, "aug", "ago")
rain_panjane$date<-as.Date(rain_panjane$date, "%d-%b-%y")



rain_magude$date <- tolower(rain_magude$date)
rain_magude$date <- str_replace_all(rain_magude$date, " ", "-")
# rain_magude$date <- str_replace_all(rain_magude$date, "dec", "dic")
# rain_magude$date <- str_replace_all(rain_magude$date, "jan", "ene")
# rain_magude$date <- str_replace_all(rain_magude$date, "apr", "abr")
# rain_magude$date <- str_replace_all(rain_magude$date, "aug", "ago")
rain_magude$date<-as.Date(rain_magude$date, "%d-%b-%y")


discharge$date <- tolower(discharge$date)
discharge$date <- str_replace_all(discharge$date, " ", "-")
discharge$date <- as.character(discharge$date)
split<-strsplit(discharge$date, ",-")

firstElement <- function(x){x[1]}
split<-sapply(split,firstElement)
discharge$date <- split

# discharge$date <- str_replace_all(discharge$date, "dec", "dic")
# discharge$date <- str_replace_all(discharge$date, "jan", "ene")
# discharge$date <- str_replace_all(discharge$date, "apr", "abr")
# discharge$date <- str_replace_all(discharge$date, "aug", "ago")
discharge$date<-as.Date(discharge$date, "%d-%b-%y")



## merge all in ine data set. 
rain_all <- rain_panjane %>%
  left_join(rain_motaze, by = c("date")) %>%
  left_join(rain_magude, by = c("date")) %>%
  left_join(rain_mapulanguene, by = c("date"))

## renaming columns
names(rain_all) <- c("date", "rain_panjane", "rain_motaze", "rain_magude", "rain_mapulanguene")



###################################################################################################
##################### Data provided by Xinavane Tongaat Hullet sugar cane plantation###############
###################################################################################################

## Reading rainfall

rain_xinavane <- read_excel("/Users/Lucia/Google Drive/MALTEM Entomology_NEW/5. Surveillance/Climate in Magude/data/Xinavane/Xinavane Long Term Met data_18032016.xls",
                            skip=3, 
                            sheet="Rainfall")[,1:13]
rain_xinavane <- as.data.frame(lapply(rain_xinavane, as.numeric))

names(rain_xinavane)[1] <- "X"

### Reshaping rainfall  data for plotting
rain_xinavane <- melt(rain_xinavane, id="X")
rain_xinavane$variable <- tolower(rain_xinavane$variable)

# Renaming rainfall
names(rain_xinavane) <- c("year", "month", "rain")

## Reading temperature
temp <- read_excel("data/Xinavane/Xinavane Long Term Met data_18032016.xls",
                   skip=3, 
                   sheet="Air Temps")
names(temp)[1] <- "year"
temp$year <- as.numeric(temp$year)
temp <- temp[!is.na(temp$year),]
temp <- as.data.frame(lapply(temp, as.factor))
## Renaming columns
names(temp)<- gsub(".1", "_min", names(temp))


### Reshaping temperature data for plotting
temp <- melt( temp, id="year")


## Renaming columns
names(temp) <- c("year", "month", "temp")
temp$month <- tolower (temp$month)

### Separting minum temperatures from maximum

temp_min <- temp[grepl("_min", temp$month),]
temp_min$month<- gsub( "_min","", temp_min$month)
names(temp_min) <- c("year", "month", "temp_min")
as.numeric(as.character(temp_max$year))

temp_max <- temp[!grepl("_min", temp$month),]
names(temp_max) <- c("year", "month", "temp_max")
as.numeric(as.character(temp_max$year))

temp <- merge (temp_min, temp_max, by=c("year", "month"))


## Reading humidity data
humd <- read.csv("data/Xinavane/humd_sum.csv", sep=";")[5:50,]

## Renaming columns
names(humd) <- gsub(".1", "_14", names(humd))

# Reshaping humidty data
humd <- melt( humd, id="X")

## Renaming and reformating columns
names(humd) <- c("year", "month", "humd")
humd$month <- tolower (humd$month)


### Separting minum temperatures from maximum

humd_14 <- humd[grepl("_14", humd$month),]
humd_14$month<- gsub( "_14","", humd_14$month)
names(humd_14) <- c("year", "month", "humd_14")

humd_8 <- humd[!grepl("_14", humd$month),]
names(humd_8) <- c("year", "month", "humd_8")

humd<- merge (humd_14, humd_8, by=c("year", "month"))




###################################################################################################
##################### Reading CHIRPS ###############
###################################################################################################

rain_chirps <- read.csv("http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.monthly/.global/.precipitation/T/%28Aug%202009%29/last/RANGE/X/31.0/34.0/RANGE/Y/-22.0/-25.0/RANGE/home/.pceccato/.pceccato_postos_magude/.the_geom/gid/1/VALUE/%5BX/Y%5D/weighted-average/T+exch/2+ncoltable.html?tabopt.N=3&tabopt.1=text&tabopt.2=text&tabopt.3=skipanyNaN&NaNmarker=&tabtype=csv&eol=LF+%28unix%29&filename=datafile.csv")
names(rain_chirps) <- c("date", "chirps")
rain_chirps$date <- str_replace_all(rain_chirps$date, " ", "-")
rain_chirps$date <- tolower(rain_chirps$date)
rain_chirps$day <- rep("1-", length(rain_chirps[,1]))
# rain_chirps$date <- str_replace_all(rain_chirps$date, "dec", "dic")
# rain_chirps$date <- str_replace_all(rain_chirps$date, "jan", "ene")
# rain_chirps$date <- str_replace_all(rain_chirps$date, "apr", "abr")
# rain_chirps$date <- str_replace_all(rain_chirps$date, "aug", "ago")
rain_chirps$date <- paste0(rain_chirps$day ,rain_chirps$date)
rain_chirps$date<-as.Date(rain_chirps$date, "%d-%b-%Y")
rain_chirps$year<-year(rain_chirps$date)
rain_chirps$month<-month(rain_chirps$date)




rain_cmorph <- read.csv("data/datafile_cmorph_IRI_MMDAY.csv")
names(rain_cmorph) <- c("date", "chirps")
rain_cmorph$date <- str_replace_all(rain_cmorph$date, " ", "-")
rain_cmorph$date <- tolower(rain_cmorph$date)
rain_cmorph$day <- rep("1-", length(rain_cmorph[,1]))
# rain_cmorph$date <- str_replace_all(rain_cmorph$date, "dec", "dic")
# rain_cmorph$date <- str_replace_all(rain_cmorph$date, "jan", "ene")
# rain_cmorph$date <- str_replace_all(rain_cmorph$date, "apr", "abr")
# rain_cmorph$date <- str_replace_all(rain_cmorph$date, "aug", "ago")
rain_cmorph$date <- paste0(rain_cmorph$day ,rain_cmorph$date)
rain_cmorph$date<-as.Date(rain_cmorph$date, "%d-%b-%Y")
rain_cmorph$year<-year(rain_cmorph$date)
rain_cmorph$month<-month(rain_cmorph$date)

