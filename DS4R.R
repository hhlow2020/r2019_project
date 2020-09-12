#install.packages("tidyverse")
#install.packages("psych")
install.packages("readr")

library("psych")
library(tidyverse)
library(readr)
library(dplyr)

#wow read.csv is an OUTDATED way of reading csv. use read_csv
#project becomes project.name, no..of.units --your HEADING change already when u use read.csv
#read_csv is faster than read.csv
start.time <- Sys.time()
#realis19_1<-read.csv("./data/REALIS2019.csv")
end.time <- Sys.time()
time.taken <- end.time - start.time

#tidyr -->pivoting, rectangling nesting, 
popdata<-read_csv("./data/PopData2019_fat.csv")
#damn easy to do group by. 
popdata_long<-popdata %>% pivot_longer(c(3:21), names_to = "Age Group",values_to = "Population") 
#sick the regex
who_longer<-who %>% pivot_longer(cols = new_sp_m014:newrel_f65, names_to = c("diagnosis", "gender","age"), 
                                 names_pattern ="new_?(.*)_(.)(.*)",
                                 values_to = "count")

#myown_analysis

# 1. trend for pricess in each district. every year up by how many %?
# 2. cheapest CCR to own
  
realis19<-read_csv("./Desktop/r2019_project/data/REALIS2019.csv") #in a tableau dataframe which is GREAT when using rmarkdown
realis19<-realis19 %>%mutate(`area(sqft)`=`Area (sqm)`*10.7639104)
#don't do this! just a lazy way of cleaning data
realis19[complete.cases(realis19),]


realis19['datetime']<- realis19
condo<-realis19 %>%filter(`Property Type`=='Condominium') 
plot(condo$`Postal District`,condo$`Unit Price ($ psf)`)
value_condo<-condo%>%filter(condo$`Unit Price ($ psf)`<1100, condo$`Postal District` %in% c(1,2,6,9,10,11), `area(sqft)`<1200)

realis19 %>% count(`Property Type`)
realis19%>% group_by(`Property Type`)%>%summarise(`Total sold`=count(Address)) %>% ungroup()

#plot(realis19$`Planning Area`,realis19$`Transacted Price ($)`)
library(ggplot2)
#fails
ggplot(realis19,aes(`Postal District`,`area(sqft)`,fill='Transacted Price ($)'))+geom_tile()

temp<-realis19 %>% filter(`Postal District` %in% c(9))
htemp <-temp
htemp$`Sale Date` <- lubridate::dmy(htemp$`Sale Date`)
htemp<-dplyr::arrange(htemp,`Sale Date`)
ggplot(htemp, aes(x=`Sale Date`, y = `Transacted Price ($)`))+geom_bar(stat='identity')
#so sick it knows the next month is jul when i don't even have anything there
# i need more slicing and filtering before I can know what to look at. 