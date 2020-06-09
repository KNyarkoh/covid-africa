
library(ISLR)
dta = iris
head(dta)



#01a. load libriaries
library(RODBC)
library(tidyverse)
library(ggplot2)
library(tree)

#01b. load or connect data
odbcDataSources()
owid = odbcConnect("wwcovid2")
sqlTables(owid)

dtacovid = sqlFetch(owid ,"fff")

# dtacovid = na.omit(dtacovid)

#02a. chk data str, summary and possibly range, dim, length etc
str(dtacovid)
summary(dtacovid)

#02a. mk changes to data frames where necessary
#02aa. mk Africa dummy sub regions
d_eafrica = as.numeric(ifelse(dtacovid$subregion == "East Africa" ,1 ,0))
d_mafrica = as.numeric(ifelse(dtacovid$subregion == "Middle Africa" ,1 ,0))
d_nafrica = as.numeric(ifelse(dtacovid$subregion == "Northern Africa" ,1 ,0))
d_safrica = as.numeric(ifelse(dtacovid$subregion == "Southern Africa" ,1 ,0))
d_wafrica = as.numeric(ifelse(dtacovid$subregion == "Western Africa" ,1 ,0))

#add other dummies for other variables including other regions when needed

#02b. mk new data frame
dtacovid = data.frame(dtacovid,  
                      d_eafrica ,d_mafrica ,d_nafrica ,d_safrica ,d_wafrica)

#02c. mk subset of regions
africa = dtacovid[dtacovid$region == "Africa" ,]
asia = dtacovid[dtacovid$region == "Asia" ,]
europe = dtacovid[dtacovid$region == "Europe" ,]
america = dtacovid[dtacovid$region == "North America" ,]
latin = dtacovid[dtacovid$region == "Latin America" ,]
oceania = dtacovid[dtacovid$region == "Oceania" ,]


#03. chk barplot, boxplot, frequency, and histogram when necessary
ggplot(africa ,aes(x=africa$subregion ,y=africa$total_deaths ,color=africa$subregion)) +geom_boxplot() +facet_wrap(~africa$region ,scale="free_y") +scale_y_log10()
ggplot(africa ,aes(x=africa$date ,y=africa$total_deaths ,shape=africa$subregion)) +geom_point() #+scale_y_log10()


ggplot(africa ,aes(x=africa$date ,y=africa$total_cases ,color=africa$subregion)) +geom_point() +scale_y_log10()
ggplot(africa ,aes(x=africa$region ,y=africa$new_deaths)) +geom_boxplot() +facet_wrap(~africa$location ,scale="free_y") +scale_y_log10()

boxplot(dtacovid$new_cases ~ dtacovid$subregion ,las=1)
plot(dtacovid$new_cases ~ dtacovid$date )
plot(dtacovid$total_cases ~ dtacovid$date )

# add more viz

#04. chk for correlation 
#pairs(cor(dtacovid[ ,-c(1,2,3,16,17,18,30,31,32)]))
pairs(cor(dtacovid[ ,c(4:7,12:13)]))


#05. split data into training, and testing
train = sample(1:nrow(africa) ,nrow(africa)/2)
test = -train

train_d = africa[train ,]
test_d = africa[test ,]

#06. run training model 

#07. run predicted model

#08. chk for classification model

#(lm for highly corrected variables)
