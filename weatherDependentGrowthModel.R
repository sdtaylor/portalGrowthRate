library(plyr)
dataFolder='~/data/portal/'
rodents=read.csv(paste(dataFolder, 'RodentsAsOfSep2015.csv', sep=''), na.strings=c("","NA"), colClasses=c('tag'='character'))
sppCodes=read.csv(paste(dataFolder, 'PortalMammals_species.csv', sep=''))


#####################################################
#Functions to retrieve weather data
precipRaw=read.csv(paste(dataFolder,'Hourly_PPT_mm_1989_present_fixed.csv',sep=''))

#Days in the capture data are always from the morning of trapping. So here I need to sum precip from the prior
#night. Mean sunset/sunrise times throughout the year are roughly 6pm-6am

uniqueDays=unique(precipRaw[,c('Year','Month','Day')])
#Explicetly order by year, month, and day
uniqueDays=uniqueDays[with(uniqueDays, order(Year,Month,Day)),]
#Extract hourly readings for the evening hours 6pm-6am
nightlyPrecip=subset(precipRaw, (Hour>=1800 & Hour <=2400) | (Hour>=100 & Hour<=600))
nightlyPrecip=nightlyPrecip[,c('Year','Month','Day','Hour','Precipitation')]
#Label hours evening/morning. 
nightlyPrecip$TimeOfDay='morning'
nightlyPrecip$TimeOfDay[nightlyPrecip$Hour>1200]='evening'

getNightlyPrecip(year,month,day){
  #From the day given, get the previous days actual date from the unique list. This will account for nights across
  #different months, or years.
  todayIndex=which(uniqueDays$Year==year & uniqueDays$Month==month & uniqueDays$Day==day)
  yesterdayIndex=todayIndex-1
  yesterdayYear=uniqueDays$Year[yesterdayIndex]
  yesterdayMonth=uniqueDays$Month[yesterdayIndex]
  yesterdayDay=uniqueDays$Day[yesterdayIndex]
    
  morningPrecip=subset(nightlyPrecip, Year==year & Month==month & Day==day & TimeOfDay=='morning')
  morningPrecip=sum(morningPrecip$Precipitation)
  
  eveningPrecip=subset(nightlyPrecip, Year==yesterdayYear & Month==yesterdayMonth & day==yesterdayDay & TimeOfDay=='evening')
  eveningPrecip=sum(eveningPrecip$Precipitation)
}