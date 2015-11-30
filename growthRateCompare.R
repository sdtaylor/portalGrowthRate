#More of a scratch pad at the moment
library(dplyr)
library(tidyr)

dataFolder='~/data/portal/'


rodents=read.csv(paste(dataFolder, 'RodentsAsOfSep2015.csv', sep=''), na.strings=c("","NA"), colClasses=c('tag'='character'))
sppCodes=read.csv(paste(dataFolder, 'PortalMammals_species.csv', sep=''))

growths=read.csv(paste(dataFolder, 'finalGrowthDF.csv'))

growths=growths %>%
  group_by(species, period, plotType) %>%
  summarize(growth=mean(growth))


#1st try. only estimate after 1994 when pit tags were in heavy use. 
rodents=rodents[rodents$yr>=1995,]
rodents=rodents[rodents$yr<=2010,]

rodents=rodents[rodents$period>0,]
rodents=rodents[!is.na(rodents$plot),]
rodents=rodents[!is.na(rodents$species),]

delta_MTE = rodents %>%
  mutate(mRate = wgt*0.75) %>%
  group_by(period, yr) %>%
  summarize(totalMTE=sum(mRate, na.rm=TRUE))
#Growth with a smoothing spline
delta_MTE$slopeMTE=c(diff(predict(smooth.spline(delta_MTE$totalMTE))$y), 0)
#Without a smooth spline
#delta_MTE$slopeMTE=c(diff(delta_MTE$totalMTE), 0)

delta_MTE$smoothMTE=predict(smooth.spline(delta_MTE$totalMTE))$y

growths=merge(growths, delta_MTE, by='period')


with( filter(growths, species=='PP', slopeMTE>500 | slopeMTE<(-500)),
  plot(growth~slopeMTE) )
  
#ggplot(filter(growths, species=='PB'), aes(x=period, y=growth, colour=plotType, group=plotType))+geom_point()+geom_line()

x=filter(growths, species=='PP', period>=296 & period<=303)
lm1=lm(growth~slopeMTE*plotType, data=filter(growths, species=='PP'))
step1=step(lm1, direction='both')
summary(step1)

x=growths %>%
  filter(species=='PP')#, slopeMTE<=(-500) | slopeMTE>=500) 

#x=spread(growths, plotType, growth) #%>%
#  mutate(plotDiff=control-kratPlot)

#with(filter(x, species=='PP', slopeMTE<=(-500) | slopeMTE>=500), plot(plotDiff~slopeMTE))
#abline(0,0)

ggplot(x, aes(x=slopeMTE, y=growth, colour=plotType, group=plotType))+geom_point()

with(x, plot(growth~slopeMTE))
