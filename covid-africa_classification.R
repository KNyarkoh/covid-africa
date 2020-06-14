
#01a. load libriaries
library(RODBC)
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
h_africa = ifelse((dtacovid$total_deaths)/1000000 >= mean((dtacovid$total_deaths[dtacovid$region == "Africa"]))/1000000 ,"high","low")

d_eafrica = as.numeric(ifelse(dtacovid$subregion == "Eastern Africa" ,1 ,0))
d_mafrica = as.numeric(ifelse(dtacovid$subregion == "Middle Africa" ,1 ,0))
d_nafrica = as.numeric(ifelse(dtacovid$subregion == "Northern Africa" ,1 ,0))
d_safrica = as.numeric(ifelse(dtacovid$subregion == "Southern Africa" ,1 ,0))
d_wafrica = as.numeric(ifelse(dtacovid$subregion == "Western Africa" ,1 ,0))
#add other dummies for other variables including other regions when needed


#02b. mk new data frame
dtacovid = data.frame(dtacovid, h_africa, 
                      d_eafrica ,d_mafrica ,d_nafrica ,d_safrica ,d_wafrica)

#02c. mk subset of regions
africa = subset(dtacovid ,region == "Africa")


#chk subset summary
summary(africa)
colnames(africa)


#03. data exploration using boxplot and scatter plot for continous variables
#03. data exploration using barplot, frequency, and histogram for categorical variables
#transformed


#04. chk for correlation 
#pairs(cor(dtacovid[ ,-c(1,2,3,16,17,18,30,31,32)]))
#pairs(cor(dtacovid[ ,c(4:7,12:13)]))


#05. split data into training, and testing
#africa = africa[c(19,20,21,23,24,25,26,27,28,29,30,31,32,33,36,40,41,42,44,45)]
set.seed(06092020)
train = sample(1:nrow(africa) ,nrow(africa)/2)
test = -train

train_d = africa[train ,]
test_d = africa[test ,]
high_test = h_africa[test]

#06. run training model
model_tree = tree(h_africa ~ subregion
                  +tests_units 
                  +total_cases
                  +stringency_index ,
                  train_d)

plot(model_tree)
text(model_tree, pretty=0)

#07. run predicted model
model_pred = predict(model_tree ,test_d ,type="class")


#08. chk for classification model
mean(model_pred != high_test)

table(model_pred)


#09. cross validate for pruning
set.seed(2)
model_prun = cv.tree(model_tree , FUN = prune.misclass)

names(model_prun)

plot(model_prun$size, model_prun$dev, type = "b")

#09a. pruned model
pruned_model = prune.misclass(model_tree ,best = 6)

plot(pruned_model)
text(pruned_model ,pretty=0)

pruned_pred = predict(pruned_model ,test_d ,type = "class")

mean(pruned_pred != high_test ,na.rm = F)

#(lm for highly corrected variables)
