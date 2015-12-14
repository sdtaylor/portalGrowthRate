library(dplyr)
library(doParallel)
library(marked)
library(lubridate)

#If running this on hipergator, use "Rscript weatherDependentGrowthModel.R hipergator" 
args=commandArgs(trailingOnly = TRUE)
if(length(args)>0){
  dataFolder='/scratch/lfs/shawntaylor/data/portal/'
  numProcs=32
} else {
  dataFolder='~/data/portal/'
  numProcs=2
}




rodents=read.csv(paste(dataFolder, 'RodentsAsOfSep2015.csv', sep=''), na.strings=c("","NA"), colClasses=c('tag'='character'))
sppCodes=read.csv(paste(dataFolder, 'PortalMammals_species.csv', sep=''))

#1st try. only estimate after 1994 when pit tags were in heavy use. 
rodents=rodents[rodents$yr>=1995,]
rodents=rodents[rodents$yr<=2010,]

#Get trapping dates for *all* periods/plots before I cull things
trappingDates=select(rodents, period, yr, mo, dy, plot) %>% distinct()
trappingDates$period=abs(trappingDates$period)

#Some periods have missing plots that need to be filled in. There aren't many so I just 
#went and figure them out by hand
missingValues=data.frame(period=c(252,263,272,272,273,284,306),
                         plot=c(24,7,23,24,3,24,16),
                         yr=c(1999,2000,2001,2001,2001,2002,2003),
                         mo=c(2,3,1,1,3,1,11),
                         dy=c(20,4,21,21,4,12,22))
trappingDates=rbind(trappingDates, missingValues)
rm(missingValues)

rodents=rodents[rodents$period>0,]
rodents=rodents[!is.na(rodents$plot),]
rodents=rodents[!is.na(rodents$species),]

#For resources I use the total precip of the prior n months. n is:
precipMonthLag=6


###################################################################################################
#Functions to retrieve weather data
##################################################################################################
#Several functions to ultimately build 2 final lookup tables to quicly build mark data frames.
#One for the nightly precip & low temperature for trapping probability
#One for the prior 6 months of precip for survivorship
precipRaw=read.csv(paste(dataFolder,'Hourly_PPT_mm_1989_present_fixed.csv',sep=''))


#Setup a weather dataframe to work with nightly temp and precip data. 
#Days in the capture data are always from the morning of trapping. So here I need to sum precip from the prior
#night, or get the temp info across two different days. Mean sunset/sunrise times throughout the year are roughly 6pm-6am

#Get all days in the range of the study, so that you can easily lookup any previous day
uniqueDays = unlist(strsplit(as.character(seq.Date(as.Date('1977/1/1'), as.Date('2018/1/1'), 'days')), '-'))
uniqueDays = data.frame(matrix(uniqueDays, ncol=3, byrow=TRUE))
colnames(uniqueDays)=c('Year','Month','Day')
#These days started out as Date strings, so need to convert them to integers
uniqueDays[] = lapply(uniqueDays, as.character) 
uniqueDays[] = lapply(uniqueDays, as.integer)

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
  #From the day given, get the previous days actual date from the unique list. This will account for nights spanning
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
  #Data is missing from some days. If thats the case then get the average nightly low from
  #the prior 2 and the next 2 years.
  if(nrow(morningTemp)<6){
    morningTemp = filter(nightlyPrecip, (Year>=year-2 & year<=year+2) & Month==month & Day==day & TimeOfDay=='morning') %>%
      group_by(Hour) %>%
      summarize(TempAir=mean(TempAir))
  }
  morningTemp=min(morningTemp$TempAir)
  
  eveningTemp=subset(nightlyPrecip, Year==yesterdayYear & Month==yesterdayMonth & Day==yesterdayDay & TimeOfDay=='evening')
  if(nrow(eveningTemp)<6){
    eveningTemp = filter(nightlyPrecip, (Year>=yesterdayYear-2 & year<=yesterdayYear+2) & Month==yesterdayMonth & Day==yesterdayDay & TimeOfDay=='evening') %>%
      group_by(Hour) %>%
      summarize(TempAir=mean(TempAir))
  }
  eveningTemp=min(eveningTemp$TempAir)
  
  return(min(morningTemp, eveningTemp))
}

####################################################
#Get the total precipiation from the prior n months (most likely 6)
#A rolling n months that doesn't use 6 months seasons like most portal papers. Hopefully it's still fine. 
monthlyPrecipTotals = precipRaw %>%
                        group_by(Year, Month) %>%
                        summarize(precip=sum(Precipitation))

#Setup an ordered list of months to figure out lags
uniqueMonths = uniqueDays %>%
                select(Year,Month) %>%
                distinct()
#Explicetly order by year, and month
uniqueMonths=uniqueMonths[with(uniqueMonths, order(Year,Month)),]

getPrior6MonthPrecip=function(year, month){
  thisMonthIndex=which(uniqueMonths$Year==year & uniqueMonths$Month==month)
  #Say the row index for our inputed month is 150. With a lag of 6 months, this gives a list of indexes 149,148,147,146,145,144
  priorMonthsIndex=(thisMonthIndex-precipMonthLag) : (thisMonthIndex-1)
  #Now use those indexes to draw the actual year and months 
  priorYears=uniqueMonths$Year[priorMonthsIndex]
  priorMonths=uniqueMonths$Month[priorMonthsIndex]
  
  #Using the actual year and month values, now grab the total precip numbers
  
  totalPrecip=subset(precipRaw, Year %in% priorYears & Month %in% priorMonths)
  totalPrecip=sum(totalPrecip$Precipitation)
  
  return(totalPrecip)
}

########################################################################
#Setup a lookup table for nightly precip and low temps for 
#every period/plot/night combo. Takes a few minutes,
#so store it for future use. 

nightlyLookupTableFile=paste(dataFolder, 'nightlyLookup.csv',sep='')
if(file.exists(nightlyLookupTableFile)){
  nightlyLookupTable=read.csv(nightlyLookupTableFile)
} else{
#trappingDates pulled from rodents file in the beginning
  
  #Some plots are trapped > 1 night per period. It's very rare but causes issues with this analysis.
  #Here I account for that by finding those instances, and assigning just a single date for that plot/period.
  #It could probably be written better, but whatever, it works. 
  doubleDates= trappingDates %>% group_by(period, plot) %>% summarise(count=n()) %>% filter(count>1)
  for(thisRow in 1:nrow(doubleDates)){
    thisPeriod=doubleDates$period[thisRow]
    thisPlot=doubleDates$plot[thisRow]
    dateToUse=rodents %>% filter(period==thisPeriod, plot==thisPlot) %>% group_by(yr, mo, dy) %>% summarise(count=n()) 
    dateToUse=dateToUse[which.max(dateToUse$count),]
    trappingDates$yr[trappingDates$period==thisPeriod & trappingDates$plot==thisPlot]=dateToUse$yr
    trappingDates$mo[trappingDates$period==thisPeriod & trappingDates$plot==thisPlot]=dateToUse$mo
    trappingDates$dy[trappingDates$period==thisPeriod & trappingDates$plot==thisPlot]=dateToUse$dy
  }
  trappingDates=trappingDates %>% distinct()
  
  #Get nightly temp/precip values for each plot/period 
  nightlyLookupTable = trappingDates %>%
  rowwise() %>%
  #For each date/plot, get the nightly low temp and total precip, which affect probability of trapping
  mutate(precip=getNightlyPrecip(yr,mo,dy), lowTemp=getNightlyTemp(yr,mo,dy))
  #Save file for future use
  write.csv(nightlyLookupTable, nightlyLookupTableFile, row.names=FALSE)
}
rm(nightlyLookupTableFile)

#Lookup table for resources (precip) in the last 6 months
#resourceLookupTable = trappingDates %>%
#  select(period,yr,mo) %>%
#  distinct() %>%
#  group_by(period) %>%
#  filter(row_number()==1) %>%
#  ungroup() %>%
#  rowwise() %>%
#  mutate(totalPrecip=getPrior6MonthPrecip(yr, mo))

#Resources as ndvi with a time lag. 8 months from Morgan ESA 2015. 
ndviTimeLag=8
resourceLookupTable=read.csv(paste(dataFolder,'monthly_NDVI.csv',sep='')) %>%
  mutate(dateWithLag=as.Date(paste(Date,'-01',sep=''),format='%Y-%m-%d')+months(ndviTimeLag)) %>%
  mutate(yr=as.numeric(format(dateWithLag, '%Y')), mo=as.numeric(format(dateWithLag, '%m'))) %>%
  left_join(   select(trappingDates, period,yr,mo) %>% distinct(), by=c('yr','mo')  ) %>%
  select(NDVI,yr,mo,period) %>%
  rename(totalPrecip=NDVI)

habitatLookupTable= read.csv(paste(dataFolder,'shrubCoverModeled.csv',sep='')) %>%
  mutate(Plot= as.numeric(substr(Plot,5,6))) %>%
  rename(plot=Plot, yr=Year) %>%
  left_join( select(trappingDates, period,yr,plot) %>% distinct(), by=c('plot','yr')) 

#Resources as total MTE on that month
#resourceLookupTable = rodents %>%
#  mutate(wgt=wgt**0.75) %>%
#  group_by(period, yr, mo) %>%
#  summarize(mte=sum(wgt, na.rm=TRUE)) %>%
#  ungroup() %>%
#  mutate(totalPrecip=mte) #calling mte totalPrecip here so I don't have to change it everywhere else. 

rm(monthlyPrecipTotals, nightlyPrecip, precipRaw, uniqueDays, uniqueMonths, getNightlyTemp, getNightlyPrecip, getPrior6MonthPrecip, doubleDates,
   thisPeriod, thisPlot, thisRow, dateToUse)
##########################################################################################################
#Functions to setup the data frame capture history + exogounous variables
#########################################################################################################

###########################################################
#Take a portal dataframe and get a "mark" formatted capture history for each individual. 
#does not do any processing for invalid periods, multiple spp, etc.
processCH=function(df){
  periods=sort(unique(df$period))
  periods=min(periods):max(periods)
  tagList=sort(unique(df$tag))
  tagList=tagList[!is.na(tagList)]
  tagList=tagList[tagList!='0']
  tagList=tagList[tagList!='000000']
  tagCHList=c()
  
  for(tag in tagList){
    tag=as.character(tag)
    #Get a list of periods this tag was seen in. 
    thisTagPeriods=df$period[df$tag==tag]
    thisTagPeriods=thisTagPeriods[!is.na(thisTagPeriods)] #The list is full of NA's! What?! why do I have to do this? screw you R
    #convert list of seen/not seen periods to '00001001000111000.....' etc. and add it to the list of all tags
    tagCHList=c(tagCHList, as.character(paste((periods %in% thisTagPeriods)*1, collapse='')))
    
    #Pack into a dataframe and ensure capture histories are set as characters
    dfCH=data.frame(ch=tagCHList)
    dfCH[] = lapply(dfCH, as.character)
  }
  return(dfCH)
  
}

#############################################################
#Mode of a vector of numbers, used for various things
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
#####################
#Reverse a single string. Used to compute gamma below
reverseString=function(x){return(paste(rev(strsplit(x,NULL)[[1]]),collapse=""))}

###########################################################
#Get various weather variables for a particular period/plot
#Don't think I actually use this
getPlotWeatherInfo=function(period, plot){
  dayOfTrapping=unique(rodents[rodents$period==300 & rodents$plot==5 , c('yr','dy','mo','period','plot')])
  if(nrow(dayOfTrapping)>1){stop('trapping happend over >1 days!')}
  
  #dot = day of trapping
  dotYear=dayOfTrapping$yr
  dotMonth=dayOfTrapping$mo
  dotDay=dayOfTrapping$dy
  
  nightlyTemp=getNightlyTemp(dotYear, dotMonth, dotDay)
  nightlyPrecip=getNightlyPrecip(dotYear, dotMonth, dotDay)
  prior6Precip=getPrior6MonthPrecip(dotYear, dotMonth, precipMonthLag)
  
  return(list(nightlyTemp=nightlyTemp, nightlyPrecip=nightlyPrecip, prior6MonthPrecip=prior6MonthPrecip))
}

#########################################################
#Get abundances of a spp over certain periods for use as mark covariates

getRivalInfo=function(sppToUse, periods, plotToUse){
  filler=expand.grid(periods,plotToUse) #fills in missing plots where no rodent was caught
  colnames(filler)=c('period','plot')
  x=rodents %>%
    full_join(filler, by=c('plot','period')) %>%
    group_by(plot, period) %>%
    summarize(N = sum(species!=sppToUse)) %>%
    ungroup() %>%
    replace(is.na(.), 0) %>%
    filter(plot==plotToUse, period %in% periods)
  return(x$N)

}

#####################
#Reverse a single string. Used to compute gamma below
reverseString=function(x){return(paste(rev(strsplit(x,NULL)[[1]]),collapse=""))}

##########################################################
#Combine capture history and nightly weather data of an arbitrary rodent subset
#This function combines everything above to make a data frame that RMark, or marked will work with. it looks like this.
#ch, nightlyPrecip1, nightlyPrecip2, nightlyPrecip3, ...... nightlyTemp1, nightlyTemp2, nightlyTemp3,......
#0010101110....., 0, 0, .3, ....... 14, 12, 14
createMarkDF=function(rodentDF, rivalSpp=NA, reverse=FALSE){
  ch=processCH(rodentDF)
  if(reverse){
    newCH=c()
    for(i in ch$ch){newCH=c(newCH,reverseString(i))}
    ch$ch=newCH
  } 
  
  tagInfo=rodentDF %>% select(tag, period, plot)

  thisPlot=unique(rodentDF$plot)
  
  periods=sort(unique(tagInfo$period))
  periods=min(periods):max(periods)
  tagList=unique(tagInfo$tag)
  tagList=tagList[!is.na(tagList)]
  tagList=tagList[tagList!='0']
  tagList=tagList[tagList!='000000']
  tagList=sort(tagList)

  #Initialize covaraite dataframe to be paired with the capture history
  weatherDF=data.frame(matrix(data=NA, nrow=length(tagList), ncol=length(periods)*4))
  precipColNames=paste('nightlyPrecip',1:length(periods), sep='')
  tempColNames=paste('nightlyTemp',1:length(periods), sep='')
  resourceColNames=paste('resources',1:length(periods), sep='')
  habitatColNames=paste('habitat',1:length(periods),sep='')
  
  colnames(weatherDF)=c(precipColNames, tempColNames, resourceColNames,habitatColNames)

  #Put in resource and habaitat availability. 
  resources=resourceLookupTable %>% filter(period %in% periods) %>% arrange(period)
  habitat=habitatLookupTable %>% filter(period %in% periods, plot == thisPlot) %>% arrange(period)

  if(reverse){ 
    resources= resources %>% arrange(-period)
    habitat = habitat %>% arrange(-period)
  }
  
  #If making a dataframe with a rival spp abundance, set that up.
  if(!is.na(rivalSpp)){
    #Get rival spp abundances
    if(length(thisPlot)>1){stop('only one plot please')}
    rivalAbund=getRivalInfo(thisSpp, periods, thisPlot)
    
    #Re-initialize the dataframe with the extra columns
    rivalColNames=paste('rivalAbund',1:length(periods), sep='')
    
    weatherDF=data.frame(matrix(data=NA, nrow=length(tagList), ncol=length(periods)*5))
    colnames(weatherDF)=c(precipColNames, tempColNames, resourceColNames,rivalColNames,habitatColNames)
    
  }
  
  for(thisTagIndex in 1:length(tagList)){
    thisTag=tagList[thisTagIndex]
    #Because some mice roam around the plots, there is an issue with inputting weather info where you can't know which plot
    #a mice *wasn't* caught in. It's definitly not the norm though, so just pick the plot it's in the most to model weather.
    thisTagPlot=tagInfo %>% filter(tag==thisTag)
    thisTagPlot=mode(thisTagPlot$plot)
    thisTagPeriodInfo=expand.grid(periods, thisTagPlot)
    colnames(thisTagPeriodInfo)=c('period','plot')
    
    thisTagPeriodInfo=merge(thisTagPeriodInfo, nightlyLookupTable, by=c('period','plot'), all.x=TRUE, all.y=FALSE)
    thisTagPeriodInfo= thisTagPeriodInfo %>% arrange(period)
    
    if(reverse){ thisTagPeriodInfo= thisTagPeriodInfo %>% arrange(-period) }
    
    weatherDF[thisTagIndex,precipColNames]=thisTagPeriodInfo$precip
    weatherDF[thisTagIndex,tempColNames]=thisTagPeriodInfo$lowTemp
    weatherDF[thisTagIndex,resourceColNames]=resources$totalPrecip
    weatherDF[thisTagIndex,habitatColNames]=habitat$cover
    
    
    if(!is.na(rivalSpp)){
      weatherDF[thisTagIndex,rivalColNames]=rivalAbund
    }
    
  }
  

  #Merge capture history and nightly weather data
  ch=cbind(ch, weatherDF)
  return(ch)
}

##################################################################################
#The mark/recapture model!
runModel=function(df, rival=FALSE){
  #Don't understand the need for all this, I just copied it from the marked helpfile.
  x.proc=process.data(df, accumulate=FALSE)
  
  design.p=list(time.varying=c('nightlyPrecip','nightlyTemp','rivalAbund'))
  design.phi=list(time.varying=c('resources','rivalAbund','habitat'))
  
  design.parameters=list(p=design.p, Phi=design.phi)
  ddl=make.design.data(x.proc, parameters=design.parameters)
  
  if(rival){
    p.formula=list(formula=~nightlyPrecip+nightlyTemp+rivalAbund)
    phi.formula=list(formula=~resources+habitat+rivalAbund)
  } else {
    p.formula=list(formula=~nightlyPrecip+nightlyTemp)
    phi.formula=list(formula=~resources+habitat)   
  }
  
  model=crm(x.proc, ddl, hessian=FALSE, model.parameters = list(p=p.formula, Phi=phi.formula), accumulate = FALSE, model='cjs')
  model
  return(model$results$AIC)
}



#########################################################################################################
#Setup the data
###########################################################################################################
#Setup parallel processing
cl=makeCluster(numProcs)
registerDoParallel(cl)

#only need control and k-rat exclosure plots
controlPlots=c(2,4,8,11,12,14,17,22) #controls

#Only model particular spp
speciesToUse=c('PP','DM','OT','DO','PB')
rivalSpp=c('PP','DM','OT','DO','PB')
rivalSpp=c('All')

#Parallet processing needs a single loop to work with, and thus a single data frame. 
#Thing of this frame like a nested for loop. 
iterationFrame=expand.grid(controlPlots, speciesToUse, rivalSpp)
colnames(iterationFrame)=c('plot','species','rival')

# the dopar line is for paralell processing. the do line will do single threaded
#finalDF=foreach(i=1:nrow(iterationFrame), .combine=rbind, .packages=c('marked','dplyr')) %dopar% {
finalDF=foreach(i=1:20, .combine=rbind, .packages=c('marked','dplyr')) %do% {
    
  thisSpp=as.character(iterationFrame$species[i])
  thisPlot=as.integer(iterationFrame$plot[i])
  rival=as.character(iterationFrame$rival[i])
  
  print(c(thisSpp,rival,thisPlot))
  
  #When the rival is the current species being modeled, that will serve as the placeholder
  #to model the species with no competitor interaction
  if(thisSpp==rival){
    rival='None'
    aic=try( rodents %>%
             filter(species==thisSpp, plot==thisPlot) %>%
             createMarkDF(rivalSpp=thisSpp, reverse=TRUE) %>%
             runModel(rival=FALSE)  
    )
  } else {
    aic=try( rodents %>%
             filter(species==thisSpp, plot==thisPlot) %>%
             createMarkDF(rivalSpp=rival, reverse=TRUE) %>%
             runModel(rival=TRUE)
    )
  }

  if(class(aic)=='try-error'){ aic='error'}
  return(c(thisSpp,rival,thisPlot,aic))

}

colnames(finalDF)=c('species','rival','plot','aic')
finalDF=as.data.frame(finalDF)


write.csv(finalDF, '~/competitionAICscores1995-2010.csv', row.names = FALSE)

#ggplot(finalDF, aes(x=totalPrecip, y=growth, colour=species, shape=plotType, group=interaction(species, plotType)))+geom_point()+geom_line()
#ggplot(filter(finalDF, species=='PP'), aes(x=totalPrecip, y=growth, colour=plotType, group=plotType))+geom_point()
#ggplot(filter(finalDF, species=='PP'), aes(x=period, y=growth, colour=plotType, group=plotType))+geom_point()

