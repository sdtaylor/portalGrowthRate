library(dplyr)
library(tidyr)
setwd('~/projects/statsBioProject')

aicResults=read.csv('competitionAICscores1995-2010.csv') %>%
  filter(plot!=22)

aicResults$aic=as.numeric(as.character(aicResults$aic))

x=aicResults %>%
  group_by(species, plot) %>%
  top_n(1, aic)


x=aicResults %>%
  #filter(plot==2) %>%
  spread(rival, aic)

write.csv(x, 'speciesCompare.csv', row.names = FALSE)
