library(plyr)
dataFolder='~/data/portal/'
rodents=read.csv(paste(dataFolder, 'RodentsAsOfSep2015.csv', sep=''), na.strings=c("","NA"), colClasses=c('tag'='character'))
sppCodes=read.csv(paste(dataFolder, 'PortalMammals_species.csv', sep=''))


###################################################################################################
#Functions to retrieve weather data
##################################################################################################
precipRaw=read.csv(paste(dataFolder,'Hourly_PPT_mm_1989_present_fixed.csv',sep=''))


#Setup a weather dataframe to work with nightly temp and precip data. 
#Days in the capture data are always from the morning of trapping. So here I need to sum precip from the prior
#night, or get the temp info across two different days. Mean sunset/sunrise times throughout the year are roughly 6pm-6am

uniqueDays=unique(precipRaw[,c('Year','Month','Day')])
#Explicetly order by year, month, and day
uniqueDays=uniqueDays[with(uniqueDays, order(Year,Month,Day)),]
#Extract hourly readings for the evening hours 6pm-6am
nightlyPrecip=subset(precipRaw, (Hour>=1800 & Hour <=2400) | (Hour>=100 & Hour<=600))
#Label hours evening/morning. 
nightlyPrecip$TimeOfDay='morning'
nightlyPrecip$TimeOfDay[nightlyPrecip$Hour>1200]='evening'

####################################################
#Get the total precip from a previous night of trapping

getNightlyPrecip=function(year,month,day){
  #From the day given, get the previous days actual date from the unique list. This will account for nights across
  #different months, or years.
  todayIndex=which(uniqueDays$Year==year & uniqueDays$Month==month & uniqueDays$Day==day)
  yesterdayIndex=todayIndex-1
  yesterdayYear=uniqueDays$Year[yesterdayIndex]
  yesterdayMonth=uniqueDays$Month[yesterdayIndex]
  yesterdayDay=uniqueDays$Day[yesterdayIndex]
    
  morningPrecip=subset(nightlyPrecip, Year==year & Month==month & Day==day & TimeOfDay=='morning')
  morningPrecip=sum(morningPrecip$Precipitation)
  
  eveningPrecip=subset(nightlyPrecip, Year==yesterdayYear & Month==yesterdayMonth & Day==yesterdayDay & TimeOfDay=='evening')
  eveningPrecip=sum(eveningPrecip$Precipitation)
  
  return(morningPrecip+eveningPrecip)
}

#####################################################
#Get the low temp from a previous night of trapping

#Gives nightly low
getNightlyTemp=function(year, month, day){
  todayIndex=which(uniqueDays$Year==year & uniqueDays$Month==month & uniqueDays$Day==day)
  yesterdayIndex=todayIndex-1
  yesterdayYear=uniqueDays$Year[yesterdayIndex]
  yesterdayMonth=uniqueDays$Month[yesterdayIndex]
  yesterdayDay=uniqueDays$Day[yesterdayIndex]
  
  morningTemp=subset(nightlyPrecip, Year==year & Month==month & Day==day & TimeOfDay=='morning')
  morningTemp=min(morningTemp$TempAir)
  
  eveningTemp=subset(nightlyPrecip, Year==yesterdayYear & Month==yesterdayMonth & Day==yesterdayDay & TimeOfDay=='evening')
  eveningTemp=min(eveningTemp$TempAir)
  
  return(min(morningTemp, eveningTemp))
}

####################################################
#Get the total precipiation from the prior n months (most likely 6)
#A rolling n months that doesn't use 6 months seasons like most portal papers. Hopefully it's still fine. 
montlyPrecipTotals=ddply(precipRaw, c('Year','Month'), summarize, precip=sum(Precipitation))

#Setup an ordered list of months to figure out lags
uniqueMonths=unique(precipRaw[,c('Year','Month')])
#Explicetly order by year, and month
uniqueMonths=uniqueMonths[with(uniqueMonths, order(Year,Month)),]

getPrior6MonthPrecip=function(year, month, lag){
  thisMonthIndex=which(uniqueMonths$Year==year & uniqueMonths$Month==month)
  #Say the row index for our inputed month is 150. With a lag of 6 months, this gives a list of indexes 149,148,147,146,145,144
  priorMonthsIndex=(thisMonthIndex-lag) : (thisMonthIndex-1)
  #Now use those indexes to draw the actual year and months 
  priorYears=uniqueMonths$Year[priorMonthsIndex]
  priorMonths=uniqueMonths$Month[priorMonthsIndex]
  
  #Using the actual year and month values, now grab the total precip numbers
  
  totalPrecip=subset(precipRaw, Year %in% priorYears & Month %in% priorMonths)
  totalPrecip=sum(totalPrecip$Precipitation)
  
  return(totalPrecip)
}



