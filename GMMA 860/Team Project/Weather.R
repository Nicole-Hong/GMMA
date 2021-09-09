test_reg <- lm(milliseconds ~ .,F1)
summary(test_reg)
install.packages("riem")
install.packages("rnoaa")
library(riem)
library(rnoaa)
library(lubridate)

# list of weather networks across globe by country
weather_networks<- riem_networks()

# list of weather stations in a given network
###canada_stations <- riem_stations(c("CA_AB_ASOS","CA_BC_ASOS"))

australia_stations <- riem_stations("AU__ASOS")

austria_stations <- riem_stations("AT__ASOS")

azerbaijan_stations <- riem_stations("AZ__ASOS")

bahrain_stations <- riem_stations("BH__ASOS")

belgium_stations <- riem_stations("BE__ASOS")

brazil_stations <- riem_stations("BR__ASOS")

canada_ON_stations <- riem_stations("CA_ON_ASOS")
canada_QC_stations <- riem_stations("CA_QC_ASOS")

china_stations <- riem_stations("CN__ASOS")

france_stations <- riem_stations("FR__ASOS")

germany_stations <- riem_stations("DE__ASOS")

hungary_stations <- riem_stations("HU__ASOS")

india_stations <- riem_stations("IN__ASOS")

italy_stations <- riem_stations("IT__ASOS")

japan_stations <- riem_stations("JP__ASOS")
  
korea_stations <- riem_stations("KR__ASOS")
  
malaysia_stations <- riem_stations("MY__ASOS")

mexico_stations <- riem_stations("MX__ASOS")

#monaco_stations <- riem_stations("MC__ASOS")
#note issue with monaco data iincorrectly showing morocco info

netherlands_stations <- riem_stations("NL__ASOS")

russia_stations <- riem_stations("RU__ASOS")

singapore_stations <- riem_stations("SG__ASOS")

spain_stations <- riem_stations("ES__ASOS")

turkey_stations <- riem_stations("TR__ASOS")

uae_stations <- riem_stations("AE__ASOS")

uk_stations <- riem_stations("GB__ASOS")

us_austin_stations <- riem_stations("TX_ASOS")

vietnam_stations <- riem_stations("VN__ASOS")


# weather data for a specified weather station by specified date range
### pull weather data by city/country and weather station
### add city, format weather date, select desired weather variables by city
### feature engineer weather data to obtain daily max / min / avg temp, prec, humidty

australia_YMEN <- riem_measures("YMEN","2009-01-01", "2019-12-31")
australia_YMEN$city <- "Melbourne"
australia_YMEN$date <- as.Date(australia_YMEN$valid)
australia_YMEN_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM australia_YMEN GROUP BY date')


austria_LOWG <- riem_measures("LOWG","2009-01-01", "2019-12-31")
austria_LOWG$city <- "Spielburg"
austria_LOWG$date <- as.Date(austria_LOWG$valid)
austria_LOWG_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM austria_LOWG GROUP BY date')

azerbaijan_UBBB <- riem_measures("UBBB","2009-01-01", "2019-12-31")
azerbaijan_UBBB$city <- "Baku"
azerbaijan_UBBB$date <- as.Date(azerbaijan_UBBB$valid)
azerbaijan_UBBB_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM azerbaijan_UBBB GROUP BY date')


bahrain_OBBI <- riem_measures("OBBI","2009-01-01", "2019-12-31")
bahrain_OBBI$city <- "Sakhir"
bahrain_OBBI$date <- as.Date(bahrain_OBBI$valid)
bahrain_OBBI_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM bahrain_OBBI GROUP BY date')

belgium_EBBR <- riem_measures("EBBR","2009-01-01", "2019-12-31")
belgium_EBBR$city <- "Spa"
belgium_EBBR$date <- as.Date(belgium_EBBR$valid)
belgium_EBBR_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM belgium_EBBR GROUP BY date')

brazil_SBSP <- riem_measures("SBSP","2009-01-01", "2019-12-31")
brazil_SBSP$city <- "São Paulo"
brazil_SBSP$date <- as.Date(brazil_SBSP$valid)
brazil_SBSP_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM brazil_SBSP GROUP BY date')
  
canada_CYUL <- riem_measures("CYUL","2009-01-01", "2019-12-31")
canada_CYUL$city <- "Montreal"
canada_CYUL$date <- as.Date(canada_CYUL$valid)
canada_CYUL_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM canada_CYUL GROUP BY date')

china_ZSSS <- riem_measures("ZSSS","2009-01-01", "2019-12-31")
china_ZSSS$city <- "Shanghai"
china_ZSSS$date <- as.Date(china_ZSSS$valid)
china_ZSSS_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM china_ZSSS GROUP BY date')

france_LFMC	 <- riem_measures("LFMC","2009-01-01", "2019-12-31")
france_LFMC$city <- "Le Castellet"
france_LFMC$date <- as.Date(france_LFMC$valid)
france_LFMC_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM france_LFMC GROUP BY date')

germany_EDFM	 <- riem_measures("EDFM","2009-01-01", "2019-12-31")
germany_EDFM$city <- "Hockenheim"
germany_EDFM$date <- as.Date(germany_EDFM$valid)
germany_EDFM_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM germany_EDFM GROUP BY date')

germany_EDDK	 <- riem_measures("EDDK","2009-01-01","2019-12-31")
germany_EDDK$city <- "Nürburg"
germany_EDDK$date <- as.Date(germany_EDDK$valid)
germany_EDDK_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM germany_EDDK GROUP BY date')

hungary_LHBP	 <- riem_measures("LHBP","2009-01-01","2019-12-31")
hungary_LHBP$city <- "Budapest"
hungary_LHBP$date <- as.Date(hungary_LHBP$valid)
hungary_LHBP_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM hungary_LHBP GROUP BY date')

india_VIDD	 <- riem_measures("VIDD","2009-01-01","2019-12-31")
india_VIDD$city <- "Uttar Pradesh"
india_VIDD$date <- as.Date(india_VIDD$valid)
india_VIDD_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM india_VIDD GROUP BY date')

italy_LIML	 <- riem_measures("LIML","2009-01-01","2019-12-31")
italy_LIML$city <- "Monza"
italy_LIML$date <- as.Date(italy_LIML$valid)
italy_LIML_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM italy_LIML GROUP BY date')

japan_RJGG	 <- riem_measures("RJGG","2009-01-01","2019-12-31")
japan_RJGG$city <- "Suzuka"
japan_RJGG$date <- as.Date(japan_RJGG$valid)
japan_RJGG_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM japan_RJGG GROUP BY date')

korea_RKJB	 <- riem_measures("RKJB","2009-01-01","2019-12-31")
korea_RKJB$city <- "Yeongam County"
korea_RKJB$date <- as.Date(korea_RKJB$valid)
korea_RKJB_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM korea_RKJB GROUP BY date')

malaysia_WMKK	 <- riem_measures("WMKK","2009-01-01","2019-12-31")
malaysia_WMKK$city <- "Kuala Lumpur"
malaysia_WMKK$date <- as.Date(malaysia_WMKK$valid)
malaysia_WMKK_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM malaysia_WMKK GROUP BY date')

mexico_MMMX	 <- riem_measures("MMMX","2009-01-01","2019-12-31")
mexico_MMMX$city <- "Mexico City"
mexico_MMMX$date <- as.Date(mexico_MMMX$valid)
mexico_MMMX_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM mexico_MMMX GROUP BY date')

# monaco data missing: monaco_	 <- riem_measures(" ","2019-05-26")
#$date <- as.Date(bahrain_OBBI$valid)
#_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
#                             MAX(p01i), MAX(feel) FROM bahrain_OBBI GROUP BY date')

netherlands_EHAM	 <- riem_measures("EHAM","2009-01-01","2019-12-31")
netherlands_EHAM$city <- "Zandvoort"
netherlands_EHAM$date <- as.Date(netherlands_EHAM$valid)
netherlands_EHAM_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM netherlands_EHAM GROUP BY date')

russia_URSS	 <- riem_measures("URSS","2009-01-01","2019-12-31")
russia_URSS$city <- "Sochi"
russia_URSS$date <- as.Date(russia_URSS$valid)
russia_URSS_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM russia_URSS GROUP BY date')

singapore_WSAP <- riem_measures("WSAP","2009-01-01","2019-12-31")
singapore_WSAP$city <- "Marina Bay"
singapore_WSAP$date <- as.Date(singapore_WSAP$valid)
singapore_WSAP_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM singapore_WSAP GROUP BY date')

spain_LEBL	 <- riem_measures("LEBL","2019-05-12")
spain_LEBL$city <- "Montmeló"
spain_LEBL$date <- as.Date(spain_LEBL$valid)
spain_LEBL_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM spain_LEBL GROUP BY date')

spain_LEVC	 <- riem_measures("LEVC","2009-01-01","2019-12-31")
spain_LEVC$city <- "Valencia"
spain_LEVC$date <- as.Date(spain_LEVC$valid)
spain_LEVC_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM spain_LEVC GROUP BY date')

turkey_LTFJ	 <- riem_measures("LTFJ","2009-01-01","2019-12-31")
turkey_LTFJ$city <- "Istanbul"
turkey_LTFJ$date <- as.Date(turkey_LTFJ$valid)
turkey_LTFJ_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM turkey_LTFJ GROUP BY date')

uae_OMAA	 <- riem_measures("OMAA","2009-01-01","2019-12-31")
uae_OMAA$city <- "Abu Dhabi"
uae_OMAA$date <- as.Date(uae_OMAA$valid)
uae_OMAA_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM uae_OMAA GROUP BY date')
	
uK_EGBB	 <- riem_measures("EGBB","2019-07-14")
uK_EGBB$city <- "Silverstone"
uK_EGBB$date <- as.Date(uK_EGBB$valid)
uK_EGBB_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM uK_EGBB GROUP BY date')

us_AUS	 <- riem_measures("AUS","2009-01-01","2019-12-31")
us_AUS$city <- "Austin"
us_AUS$date <- as.Date(us_AUS$valid)
us_AUS_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM us_AUS GROUP BY date')

vietnam_VVNB	 <- riem_measures("VVNB","2009-01-01","2019-12-31")
vietnam_VVNB$city <- "Hanoi"
vietnam_VVNB$date <- as.Date(vietnam_VVNB$valid)
vietnam_VVNB_weather <- sqldf('SELECT city, station, date, MAX(tmpf), MIN(tmpf), AVG(tmpf), AVG(relh), 
                             MAX(p01i), MAX(feel) FROM vietnam_VVNB GROUP BY date')

##
##rowbind city weather data frames into one data frame for all circuit cities

weather_allcircuits <- rbind(australia_YMEN_weather,austria_LOWG_weather,azerbaijan_UBBB_weather, bahrain_OBBI_weather,
                             belgium_EBBR_weather,brazil_SBSP_weather, canada_CYUL_weather, china_ZSSS_weather,
                             france_LFMC_weather, germany_EDFM_weather,germany_EDDK_weather,hungary_LHBP_weather,
                             india_VIDD_weather, italy_LIML_weather,japan_RJGG_weather, korea_RKJB_weather,
                             malaysia_WMKK_weather, mexico_MMMX_weather, netherlands_EHAM_weather, russia_URSS_weather,
                             singapore_WSAP_weather,spain_LEBL_weather,turkey_LTFJ_weather, uae_OMAA_weather,
                             uK_EGBB_weather,us_AUS_weather, vietnam_VVNB_weather)

## export final external weather data file into 1 csv file
write.csv(weather_allcircuits,'weather_allcircuits.csv')
