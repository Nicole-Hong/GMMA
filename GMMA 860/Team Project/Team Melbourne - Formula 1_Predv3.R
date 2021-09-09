############################## 
# 
# Team Melbourne
#
##############################

install.packages(c("tidyverse","readxl"))
install.packages(c("lubridate","hms"))
update.packages()
install.packages("eeptools")
library(eeptools)
library(tidyverse)
library(readxl)
library(sqldf)
library(lubridate)
library(hms)
library(caret)
library(dplyr)
library(ggplot2)

############### Start loading of the data from Excel ################
# Total 13 Data frames


# File path to the directory

directory_csv <- "C:\\F1\\"

#directory_csv <- "C:\\Lorraine's Program\\Queen's GMMA\\860 Acquisition of Data\\Project\\formula-1-world-championship-1950-2020\\"

#directory_csv <- "C:\\Users\\dltri\\iCloudDrive\\GMMA\\classes\\GMMA860-Acquisition and Management of Data\\assignment\\team\\F1\\formula-1-world-championship-1950-2020\\"

weather_allcircuits_df <- read.csv("C:\\F1\\weather_allcircuits.csv")
weather_missing_df <- read_csv("C:\\F1\\weather_missing.csv")

circuits_df <- tryCatch ({
  read_csv(paste0(directory_csv, "circuits.csv"), col_types = cols(.default = "c", circuitId = "d"))
}, error = function(err) {
  read_csv(file.choose())
})

constructors_df <- tryCatch ({
  read_csv(paste0(directory_csv, "constructors.csv"), col_types = cols(.default = "c", constructorId = "d"))
}, error = function(err) {
  read_csv(file.choose())
})

drivers_df <- tryCatch ({
  read_csv(paste0(directory_csv, "drivers.csv"), col_types = cols(.default = "c", driverId = "d", dob="D"))
}, error = function(err) {
  read_csv(file.choose())
})

races_df <- tryCatch ({
  read_csv(paste0(directory_csv, "races.csv"), col_types = cols(.default = "c", raceId = "d", year="i", round="d", circuitId="d", date="D", time="t"))
}, error = function(err) {
  read_csv(file.choose())
})

seasons_df <- tryCatch ({
  read_csv(paste0(directory_csv, "seasons.csv"), col_types = cols(.default = "c", year = "i"))
}, error = function(err) {
  read_csv(file.choose())
})

status_df <- tryCatch ({
  read_csv(paste0(directory_csv, "status.csv"), col_types = cols(.default = "c", statusId = "d"))
}, error = function(err) {
  read_csv(file.choose())
})

constructor_results_df <- tryCatch ({
  read_csv(paste0(directory_csv, "constructor_results.csv"), col_types = cols(.default = "d", status = "c"))
}, error = function(err) {
  read_csv(file.choose())
})

constructor_standings_df <- tryCatch ({
  read_csv(paste0(directory_csv, "constructor_standings.csv"), col_types = cols(.default = "d", positionText = "c"))
}, error = function(err) {
  read_csv(file.choose())
})

driver_standings_df <- tryCatch ({
  read_csv(paste0(directory_csv, "driver_standings.csv"), col_types = cols(.default = "d", positionText = "c"))
}, error = function(err) {
  read_csv(file.choose())
})

lap_times_df <- tryCatch ({
  read_csv(paste0(directory_csv, "lap_times.csv"), col_types = cols(.default = "d", time = "t"))
}, error = function(err) {
  read_csv(file.choose())
})

pit_stops_df <- tryCatch ({
  read_csv(paste0(directory_csv, "pit_stops.csv"), col_types = cols(.default = "d", time = "t"))
}, error = function(err) {
  read_csv(file.choose())
})

qualifying_df <- tryCatch ({
  read_csv(paste0(directory_csv, "qualifying.csv"), col_types = cols(qualifyId = "d", raceId = "d", driverId = "d", constructorId = "d", number = "d", position = "d", q1 = "c", q2 = "c", q3 = "c" ))
}, error = function(err) {
  read_csv(file.choose())
})

results_df <- tryCatch ({
  read_csv(paste0(directory_csv, "results.csv"), col_types = cols(.default = "d", time = "t",fastestLapTime = "c", positionText = "c"))
}, error = function(err) {
  read_csv(file.choose())
})

weather_allcircuits_df <- tryCatch ({
  read_csv(paste0(directory_csv, "weather_allcircuits.csv"))
}, error = function(err) {
  read_csv(file.choose())
})
############### Complete loading of the data from Excel ################



############################## remove fields #####################

### NOTE: combine duration and milliseconds fields to durationMilliseconds - Feature Engineering
pit_stops_df <- select(pit_stops_df, raceId, driverId, stop, lap, time, milliseconds)
drivers_df <- select(drivers_df, driverId, driverRef, code, forename, surname, dob, nationality, url)
circuits_df <- select(circuits_df, circuitId, circuitRef, name, location, country,lat, lng, url)
lap_times_df <- select(lap_times_df, raceId, driverId, lap, position, milliseconds)
results_df <- select(results_df, resultId ,raceId ,driverId ,constructorId ,number ,grid ,position ,positionText ,positionOrder ,points ,laps ,milliseconds ,fastestLap ,rank ,fastestLapTime ,fastestLapSpeed ,statusId)

############################## End remove fields ####################


############ Convert \N into NA ##################

drivers_df$code[drivers_df$code == "\\N"] <- NA
circuits_df[circuits_df == "\\N"] <- NA
constructors_df[constructors_df == "\\N"] <- NA
races_df$time[races_df$time == "\\N"] <- NA
seasons_df[seasons_df == "\\N"] <- NA
status_df[status_df == "\\N"] <- NA
constructor_results_df[constructor_results_df == "\\N"] <- NA
constructor_standings_df[constructor_standings_df == "\\N"] <- NA
driver_standings_df[driver_standings_df == "\\N"] <- NA
lap_times_df[lap_times_df == "\\N"] <- NA
pit_stops_df[pit_stops_df == "\\N"] <- NA
qualifying_df[qualifying_df == "\\N"] <- NA
results_df[results_df == "\\N"] <- NA


############ END Convert \N into NA ##################


############ Custom Functions ###########################
#
# make sure to run the time_to_milliseconds code

time_to_milliseconds <- function(time_char) {
  time1 <- paste("00:0", time_char, sep="")
  time_minutes <- as_hms(time1)
  time_milliseconds <- as.numeric(time_minutes, units="secs")*1000
  return(time_milliseconds)
}

############ END Custom Functions ###########################


############################## Update Data Types and convert time to milliseconds #####################

## drivers_df
str(drivers_df)
## End drivers_df

## circuits_df
str(circuits_df)
## End circuits_df

## constructors_df
str(constructors_df)
## End constructors_df

## races_df
str(races_df)
races_df$time <- as_hms(races_df$time)
str(races_df)
## End races_df

## seasons_df
str(seasons_df)
## End seasons_df

## status_df
str(status_df)
## End status_df

## constructor_results_df
str(constructor_results_df)
## End constructor_results_df

## constructor_standings_df
str(constructor_standings_df)
constructor_standings_df$positionText <- as.character(constructor_standings_df$positionText)
str(constructor_standings_df)
## End constructor_standings_df

## driver_standings_df
str(driver_standings_df)
driver_standings_df$positionText <- as.character(driver_standings_df$positionText)
str(driver_standings_df)
## End driver_standings_df

## lap_times_df
str(lap_times_df)
## End lap_times_df

## pit_stops_df
str(pit_stops_df)
## End pit_stops_df

## qualifying_df
str(qualifying_df)
qualifying_df$q1 <- as.numeric(time_to_milliseconds(qualifying_df$q1))
qualifying_df$q2 <- as.numeric(time_to_milliseconds(qualifying_df$q2))
qualifying_df$q3 <- as.numeric(time_to_milliseconds(qualifying_df$q3))
str(qualifying_df)
## End qualifying_df

## results_df
str(results_df)
results_df$position <- as.numeric(results_df$position)
results_df$milliseconds <- as.numeric(results_df$milliseconds)
results_df$fastestLap <- as.numeric(results_df$fastestLap)
results_df$rank <- as.numeric(results_df$rank)
results_df$fastestLapTime <- as.numeric(time_to_milliseconds(results_df$fastestLapTime))
results_df$fastestLapSpeed <- as.numeric(results_df$fastestLapSpeed)
str(drivers_df)
str(races_df)
## End results_df

############################## END Update Data Types and convert time to milliseconds #####################

####################### Export clean dataframes to CVS Files ##################

write.csv(circuits_df,'circuits_df.csv')
write.csv(constructors_df,'constructors_df.csv')
write.csv(drivers_df,'drivers_df.csv')
write.csv(races_df,'races_df.csv')
write.csv(seasons_df,'seasons_df.csv')
write.csv(status_df,'status_df.csv')
write.csv(constructor_results_df,'constructor_results_df.csv')
write.csv(constructor_standings_df,'constructor_standings_df.csv')
write.csv(driver_standings_df,'driver_standings_df.csv')
write.csv(lap_times_df,'lap_times_df.csv')
write.csv(pit_stops_df,'pit_stops_df.csv')
write.csv(qualifying_df,'qualifying_df.csv')
write.csv(results_df,'results_df.csv')

####################### End of Export clean dataframes to CVS Files ##################


############ Joining DATA  ##############

## Update drivers_df to include Age and Full Name
drivers_df$driverName <-paste(drivers_df$forename,"",drivers_df$surname)
drivers_df$driverAge<-age_calc(drivers_df$dob,enddate = Sys.Date(), units = "years", precise = FALSE)

head(drivers_df)

# join constructor name to raceId and group by RaceId

constructorName<-sqldf('SELECT constructors_df.name,results_df.driverId, results_df.raceId from constructors_df,results_df where constructors_df.constructorId = results_df.constructorId ORDER BY results_df.raceId')


# join circuit name to Results and group by RaceId

circuitName<-sqldf('SELECT races_df.name,results_df.raceId, results_df.driverId from races_df, results_df where results_df.raceId = races_df.raceId ORDER BY results_df.raceId')

# select Max value in pit_stop_df Stop as the total number of pit stops per raceId per driverId

pitstopCount<-aggregate(stop ~ raceId+driverId, data=pit_stops_df, max)

# New dataframe Final OMEGA!!!

## drivers_df$driverId, drivers_df$driverName, drivers_df$nationality, drivers_df$driverAge, results_df$raceId, constructorName$name, circuitName$name, results_df$grid, results_df$position, results_df$fastestLapTime, results_df$fastestLapSpeed, qualifying_df$q1, qualifying_df$q2, qualifying_df$q3, pit_stop_df$milliseconds, pitStopCount$stop, results_df$miliseconds

races_2009<- sqldf('SELECT year, date,raceId FROM races_df WHERE year>= 2009')
head(races_2009)
head(drivers_df)
driver_info<- sqldf('SELECT driverId, driverName, dob,driverAge, nationality FROM drivers_df')

post2009_result<-sqldf('SELECT results_df.raceId, date,results_df.points,driverId, grid, position, fastestLapTime, fastestLapSpeed, milliseconds FROM results_df JOIN races_2009 ON races_2009.raceId = results_df.raceId')
post2009_qualifying<-sqldf('SELECT qualifying_df.raceId, driverId, q1, q2, q3 FROM qualifying_df JOIN races_2009 ON races_2009.raceId = qualifying_df.raceId')

head(post2009_qualifying)
pitstop_info<- sqldf('SELECT pit_stops_df.raceId, pit_stops_df.driverId, milliseconds, pitstopCount.stop 
                      FROM pit_stops_df, pitstopCount 
                         WHERE pit_stops_df.raceId = pitstopCount.raceId 
                         AND pit_stops_df.driverId = pitstopCount.driverId')
post2009_pitstop<-sqldf('SELECT pitstop_info.raceId, driverId, stop, milliseconds FROM pitstop_info JOIN races_2009 ON races_2009.raceId = pitstop_info.raceId')

F1_races1<-sqldf('SELECT post2009_result.raceId, post2009_result.driverId, post2009_result.points,grid, date, position, fastestLapTime, fastestLapSpeed, post2009_result.milliseconds, q1, q2, q3 
            FROM post2009_result JOIN post2009_qualifying
            ON post2009_result.raceId = post2009_qualifying.raceId
             AND post2009_result.driverId = post2009_qualifying.driverId')


###DUPLICATE milliseconds name;  ADD F1_faces1.points
F1_races<-sqldf('SELECT F1_races1.raceId, F1_races1.date,F1_races1.driverId, F1_races1.points,grid, position, fastestLapTime, fastestLapSpeed, F1_races1.milliseconds as race_milliseconds, q1, q2, q3, stop, post2009_pitstop.milliseconds 
                  FROM F1_races1 JOIN post2009_pitstop
                  ON F1_races1.driverId = post2009_pitstop.driverId
                  AND F1_races1.raceId = post2009_pitstop.raceId')


head(F1_races)
names(F1_races)[14]<-"pitstopTime"
names(F1_races)[9]<-"milliseconds"
head(F1_races)
summary(F1_races)

## add driver infomration to F1_races

F1_driver<-sqldf('SELECT raceId, date, driver_info.driverId, dob,driverName,driverAge, nationality, grid, points,position, fastestLapTime, fastestLapSpeed, milliseconds, q1, q2, q3, stop,pitstopTime 
             FROM F1_races JOIN driver_info ON F1_races.driverId = driver_info.driverId')

F1_driver$driverAge_raceday<-age_calc(F1_driver$dob,enddate = F1_driver$date, units = "years", precise = FALSE)

head(F1_driver)

F1_total<-sqldf('SELECT F1_driver.raceId, constructorName.driverId, driverName, driverAge_raceday, nationality, name, grid, points,position, fastestLapTime, fastestLapSpeed, milliseconds, q1, q2, q3, stop, pitstopTime 
             FROM F1_driver JOIN constructorName ON F1_driver.driverId = constructorName.driverId
             AND F1_driver.raceID = constructorName.raceId')
head(F1_total)

################################################

############ Feature Engineering  ##############

# Add interaction features to F1_total: average and total pitstop time per driver race, ratio of pitstop to race time



F1_total_FE <- sqldf('SELECT raceId, driverId, driverName, driverAge_raceday, points, nationality, name, grid, position, fastestLapTime, fastestLapSpeed, milliseconds, q1, q2, q3, stop, SUM(pitstopTime) AS Pitstop_Total_Time, AVG(pitstopTime) AS AvgPitStopTime FROM F1_total GROUP BY driverid, raceid ORDER BY raceid')
F1_total_FE$ratioPittoTotal <- F1_total_FE$Pitstop_Total_Time/F1_total_FE$milliseconds

# change col names for clarity, avoid duplicated names with different meanings in col headers

head(F1_total_FE)

names(races_df)[5] <- "circuit"
names(circuits_df)[3] <- "circuit"
names(F1_total_FE)[6] <- "constructor"

# Set up data frame with circuit id, circuit name, race date and start time all as prep to import weather data
# pulls together data from races_df and circuits_df

race_circuit <- merge(races_df, circuits_df, by=c("circuitId","circuitId"))
race_circuit_2009 <- filter(race_circuit,year>2008)
race_circuit_2009$url.x <- race_circuit_2009$circuitRef <- race_circuit_2009$circuit.y <- race_circuit_2009$url.y <- race_circuit_2009$round <- NULL

#Flag Monaco (shortest circuit) on race_circuit_2009 and join table to F1_total_FE
race_circuit_2009$Monaco <- ifelse(race_circuit_2009$circuit.x=="Monaco Grand Prix",1,0)
F1 <- merge(F1_total_FE, race_circuit_2009, by=c("raceId","raceId"))

driver_stdgs_subset <- sqldf('SELECT driverStandingsId, raceId, driverId, wins AS driver_wins FROM driver_standings_df')
#constructor_stdgs_subset <- sqldf('SELECT constructorStandingsId, raceId, constructorId, wins FROM constructor_standings_df')  


F1_driver_wins <- merge(F1,driver_stdgs_subset,by=c("raceId","driverId"),all.x=TRUE, all.y=FALSE)
#F1_constructor_wins <- merge(F1_driver_wins,constructor_stdgs_subset,by=c("raceId","constructorId"),all.x=TRUE, all.y=FALSE)
  
# Add total number of races per driver from 2009 from post2009 result
driver_race_count <- sqldf('SELECT driverId, COUNT(raceId) AS race_count FROM post2009_result GROUP BY driverId')


# number of wins per driver in 2019
driver_wins <- sqldf('SELECT driverId, MAX(wins) AS driver_wins FROM driver_standings_df LEFT JOIN races_df ON driver_standings_df.raceId=races_df.raceId WHERE races_df.year =2019 GROUP BY driverId')
head(driver_wins)

# number of constructor wins in 2019
constructor_wins <- sqldf('SELECT constructorId, MAX(wins) AS constructor_wins FROM constructor_standings_df LEFT JOIN races_df ON constructor_standings_df.raceId=races_df.raceId WHERE races_df.year = 2019 GROUP BY constructorId')


##weather_station_df <- tryCatch ({
##  read_excel("C:\\Users\\dltri\\iCloudDrive\\GMMA\\classes\\GMMA860-Acquisition and Management of Data\\assignment\\team\\F1\\formula-1-world-championship-1950-2020\\weather\\WeatherStations.xlsx")
##}, error = function(err) {
##  read_excel(file.choose())
##})

###read weather_allcircuits.csv to import the external weather data file 
### there is a seperate detailed R script covering steps to import weather data using riem package, 
## output is in weather_allcircuits.csv


###F1_master is final file for prediction includes driver race and westher data. Use mutiple imputation for missing data
F1_master <- merge(F1_driver_wins, weather_allcircuits_df, by.x=c("date","location"), by.y=c("date","city"), all.x=TRUE, all.y=FALSE)
#head(weatherplusF1)
#md.pattern(weatherplusF1)

weather_race_circuit_2009 <- merge(race_circuit_2009, weather_allcircuits_df, by.x=c("date","location"), by.y=c("date","city"), all.x=TRUE, all.y=FALSE)
head(weather_race_circuit_2009)
#md.pattern(weather_race_circuit_2009)
head(F1_master)
str(F1_master)

##### Predictive Modeling #####
#get minimum of Qualifying rounds
F1_master$min_q <-with(F1_master, pmin(q1, q2, q3, na.rm = TRUE))
#names(F1_master)[25] <- "time"
head(F1_master)
##step 1

#write.csv(F1_master, "C:\\Lorraine's Program\\Queen's GMMA\\860 Acquisition of Data\\Project\\formula-1-world-championship-1950-2020\\F1_Master.csv")

F1_master2<-sqldf('select  date, location, raceId, driverId,driver_wins,
                          sum(driver_wins) over (partition by driverId order by date ) as rolling_wins,
                          sum(points) over (partition by driverId order by date ) as rolling_points,
                          count(raceId) over (partition by driverId order by date ) as rolling_race_completed,
                          (sum(points) over (partition by driverId order by date ))/(count(raceId) over (partition by driverId order by date )) as avg_points_per_race,
                          (sum(driver_wins) over (partition by driverId order by date ))/(count(raceId) over (partition by driverId order by date )) as avg_wins_per_race
                   from F1_master 
                  ')
head(F1_master2)

##join F1_master and F1_master2
##   ----------  omiited Time and Circuit.x   columns  :(  due to format error
F1_master3<-sqldf('select a.date,
                          a.location,
                          a.circuitId,
                          
                          a.raceId,
                          a.driverId,
                          a.driverName,
                          a.driverAge_raceday,
                          a.points, 
                          a.constructor,     
                          a.name, 
                          a.grid, 
                          a.position, 
                          a.fastestLapTime,
                          a.fastestLapSpeed, 
                          a.milliseconds,
                          a.q1,    
                          a.q2,    
                          a.q3, 
                          a.min_q,
                          a.stop, 
                          a.Pitstop_Total_Time, 
                          a.AvgPitStopTime, 
                          a.ratioPittoTotal, 
                          a.circuitId, 
                          a.year,
                          a.country,
                          a.lat,
                          a.lng,
                          a.Monaco,
                          a.driverStandingsId,
                          a.driver_wins,
                          a.station,
                          a.AVG_tmpf,
                          a.AVG_relh, 
                            b.rolling_points,
                            b.rolling_wins,
                            b.rolling_race_completed,
                            b.avg_points_per_race,
                            b.avg_wins_per_race
                    from F1_master a left join F1_master2 b 
                     on a.driverId = b.driverId and a.raceId=b.raceId')

head(F1_master3)
weather_missing_df <- missing_weather_data
weather_missing <- weather_missing_df %>%
  mutate(AVG_tmph_c = (AVG_tmph * 9/5) + 32) %>%
  mutate(date_new = mdy(Date))
weather_missing <- subset(weather_missing, select = -c(Date, AVG_tmph))
names(weather_missing)[3] <- "AVG_tmpf"
names(weather_missing)[1] <- "location"
names(weather_missing)[4] <- "date"


head(weather_allcircuits_df)
head(weather_missing)

summary(weather_allcircuits_df)

write.csv(F1_master3,'F1_master3.csv')

#weather_allcircuits_df$date <- ymd(weather_allcircuits_df$date)
#weather_allcircuits_df$city <- as.character(weather_allcircuits_df$city)
#weather_all <- sqldf('SELECT 
#                          a.city,
#                          a.date,
#                          a.MAX_tmpf,
#                          a.MIN_tmpf,
#                          a.AVG_tmpf,
#                          
#                          b.Location,
#                          b.Relh,
#                          b.AVG_tmph_f,
#                          b.date_new
#                          
#                          from F1_master3 a left join weather_missing b
#                          on a.city = b.Location and a.date = b.date_new
#
#                        ')
#weather_all


#head(weather_missing)


 ##### linear regression #####
#CASE WHEN AVG_tmpf is NULL THEN AVG_tmph_c ELSE AVG_tmpf END AS tempf
#weather_allcircuits_df.date = date_new AND 

#remove NA for millisecond race time
F1_Master_NA <-subset(F1_master3, !is.na(milliseconds))

F1_Master_NA <- F1_Master_NA %>%
  left_join(weather_missing, by = c("location","date")) %>%
  mutate(AVG_tmpf = ifelse(is.na(AVG_tmpf.x), AVG_tmpf.y, AVG_tmpf.x)) %>%
  select(-c(AVG_tmpf.x, AVG_tmpf.y))

summary(F1_Master_NA)
## ###  --  Transforming milliseconds  -- ###
#transforming variables: Log ( milliseconds ) and sqrt (milliseconds) to take care of outliers. make them more normal
F1_Master_NA$sqrt_milliseconds <-sqrt(F1_Master_NA$milliseconds)
F1_Master_NA$lg_milliseconds <-log10(F1_Master_NA$milliseconds)

summary(F1_Master_NA)
head(F1_Master_NA)


#look at distribution
boxplot(F1_Master_NA$milliseconds)
hist(F1_Master_NA$milliseconds)
boxplot(F1_Master_NA$sqrt_milliseconds)
boxplot(F1_Master_NA$lg_milliseconds)

summary(F1_Master_NA$milliseconds)
summary(F1_Master_NA$sqrt_milliseconds)
summary(F1_Master_NA$lg_milliseconds)

hist(F1_Master_NA$sqrt_milliseconds)
hist(F1_Master_NA$lg_milliseconds)
str(F1_Master_NA$sqrt_milliseconds)

#there is only a few above 15.8 + ;  log above 6.8
plot(F1_Master_NA$sqrt_milliseconds)
plot(F1_Master_NA$lg_milliseconds)

#  ----------  removed outliers  > 6.85 -----------    ###
F1_Master_NA_out<-sqldf('select * from F1_Master_NA where lg_milliseconds<=6.85')
#boxplot(F1_Master_NA_out$lg_milliseconds)
F1_Master_NA<-F1_Master_NA_out

##  ---  Final Master List used for Data visualization  EXPORT --------##
write.csv(F1_Master_NA, "C:\\Lorraine's Program\\Queen's GMMA\\860 Acquisition of Data\\Project\\formula-1-world-championship-1950-2020\\F1_Master_for_visual.csv")


#  test and train data #
sample <- sample.int(n = nrow(F1_Master_NA), size = floor(.7*nrow(F1_Master_NA)), replace = F)
train <- F1_Master_NA[sample, ]
test  <- F1_Master_NA[-sample, ]
str(train)

########   first regression ########
reg_train <- lm(avg_points_per_race~
                  milliseconds+ 
                  sqrt_milliseconds+
                  lg_milliseconds+
                  grid + 
                  circuitId+
                  driverAge_raceday + 
                  fastestLapTime+
                  fastestLapSpeed+
                  min_q+
                  stop+
                  Pitstop_Total_Time+
                  AvgPitStopTime+
                  driver_wins+
                  rolling_wins+
                  rolling_points+
                  rolling_race_completed+
                  AVG_tmpf+
                  AVG_relh+
                  avg_wins_per_race,
                 train)



summary(reg_train)
plot(reg_train)

#######  1st revision  of regression  1 ####
########   first regression ########
reg_train1 <- lm(avg_points_per_race~
                   
                  grid + 
                   driverAge_raceday + 
                 
                  rolling_points+
                  rolling_race_completed,
                   
                train)
summary(reg_train1)
plot(reg_train1)



########  2nd regression      ##########

          
reg_train2 <- lm(lg_milliseconds~
                  avg_points_per_race+ 
                  grid + 
                  driverAge_raceday + 
                  fastestLapTime+
                  fastestLapSpeed+
                  min_q+
                  stop+
                   circuitId+
                  Pitstop_Total_Time+
                  AvgPitStopTime+
                  driver_wins+
                   rolling_wins+
                  rolling_points+
                  rolling_race_completed+
                  avg_wins_per_race,
                train)

summary(reg_train2)
plot(reg_train2)


#####    2nd  REvision of  regression 2. Final R-sqr = 0.64 ;   ???  AFter removing outlier, R-sqr = 0.86 ?????######  

reg_train2v2 <- lm(lg_milliseconds~
                   grid + 
                  driverAge_raceday + 
                   fastestLapTime+
                   fastestLapSpeed+
                   min_q+
                   stop+
                   Pitstop_Total_Time+
                   rolling_race_completed+
                   AVG_relh
                      ,
                 train)

summary(reg_train2v2)
plot(reg_train2v2)

## now decide what to do with weather data, before running mod on TEST ###

#pred <- predict(reg_train2v2, test)



####  --- compare TEst and Train  ### 

data.frame( (R2 = R2(pred, test$F1_Master_NA)),
            RMSE = RMSE(pred, test$F1_Master_NA),
            MAE = MAE(pred, test$F1_Master_NA))

data.frame(  MAE = MAE(pred, test$F1_Master_NA))



#write.csv(F1_Master_NA, "C:\\Lorraine's Program\\Queen's GMMA\\860 Acquisition of Data\\Project\\formula-1-world-championship-1950-2020\\F1_Master_predict.csv")
#-------------------------------------------------------------
############

#get_weather <- function(location, race_date) {
#  filter_df <- filter(weather_allcircuits_df, date == as.Date(race_date), city == location)
#  return(filter_df)
#}


#get_weather_station <- function(location, race_date) {
  
#  filter_df <- filter(weather_allcircuits_df, city == location, date == as.Date(race_date))

#  if(nrow(filter_df) == 0) {
#    return(NA)
#  }

#  if(length(filter_df) == 0) {
#    return(NA)
#  }
  
#  if(is_empty(filter_df) == TRUE) {
#    return(NA)
#  }

#  return(filter_df$station)
#}


#F1$weatherStation <- as.character(NA)
#F1$MAX_tmpf <- as.numeric(NA)
#F1$MIN_tmpf <- as.numeric(NA)
#F1$AVG_tmpf <- as.numeric(NA)
#F1$MAX_p01i <- as.numeric(NA)
#F1$MAX_feel <- as.numeric(NA)

#print("**** Populating weather data for F1 - Please wait... *************************************")
#for(row in 1:nrow(F1)) {
  
#  date_row <- F1[row, "date"]
#  location_row <- F1[row, "location"]
#  weather_data = get_weather(location_row, date_row)
  
#  if(nrow(weather_data) == 0) {
#    #F1[row, "weatherStation"] <- NA
#    F1[row, "MAX_tmpf"] <- NA
#    F1[row, "MIN_tmpf"] <- NA
#    F1[row, "AVG_tmpf"] <- NA
#    F1[row, "MAX_p01i"] <- NA
#    F1[row, "MAX_feel"] <- NA
#  } else {
#   #F1[row, "weatherStation"] <- weather_data$station
#   F1[row, "MAX_tmpf"] <- weather_data$MAX_tmpf
#   F1[row, "MIN_tmpf"] <- weather_data$MIN_tmpf
#   F1[row, "AVG_tmpf"] <- weather_data$AVG_tmpf
#   F1[row, "MAX_p01i"] <- weather_data$MAX_p01i
#   F1[row, "MAX_feel"] <- weather_data$MAX_feel
#  }
#}
#print("**** FINISHED - Populating weather data for F1  *************************************")


#######################################################






