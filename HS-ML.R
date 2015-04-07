setwd("~/GitHub/NYCDSA/Personal Projects/Hearthstone/")
require(ggplot2)
require(RMySQL)
require(lubridate)
require(e1071)
require(leaps)
require(caret)
require(doMC)
require(dplyr)

# Multicore setup
registerDoMC(cores = 3)

# Setup access to the SQL localhost on MAMP
db <- src_mysql(dbname = 'AMDB', host = 'localhost', user="root", password="root",unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")
drv <- dbDriver("MySQL")
con <- dbConnect(drv, host = 'localhost', user="root", password="root", dbname = 'AMDB',unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")
dbListTables(con)

# Read the arena card SQL table
cardPool<-dbGetQuery(con, "SELECT cardId, cardName, cardSet, cardRarity, 
                     cardType, cardClass, cardCost, cardText FROM arenaCards")

# Relabel the card types from 1,2,3 to Minion, Spell or Weapon
cardPool$cardType<-factor(cardPool$cardType,levels=c(1,2,3),
                          labels=c("Minion","Spell","Weapon"))

# Relabel the card rarities (two lines because of "common" factor cleanup)
cardPool$cardRarity<-as.factor(cardPool$cardRarity)
levels(cardPool$cardRarity)<-c("Common","Common","Rare","Epic","Legendary")

# Relabel card class
cardPool$cardClass<-factor(cardPool$cardClass,levels=c(0:9),c("Neutral","Druid","Hunter","Mage","Paladin","Priest","Rogue","Shaman","Warlock","Warrior"))

# Filter out cards unavailable during Arena Drafts (promotional or quest reward cards)
cardPool<-filter(cardPool,!(cardSet %in% c(10,11)))

# Save a binary of the cardPool of arena cards
# save(cardPool,file="cardList")

# Code in Card Attributes (for later feature selection)
hasTaunt<-unlist(lapply(cardPool$cardText,function(x) grepl("Taunt",x,ignore.case=T) & !grepl("destroy",x,ignore.case=T)))
hasDraw<-unlist(lapply(cardPool$cardText,function(x) grepl("Draw",x,ignore.case=T)))
hasDestroy<-unlist(lapply(cardPool$cardText,function(x) (grepl("Destroy",x,ignore.case=T) & grepl("minion",x,ignore.case=T))))
hasAOEdmg<-unlist(lapply(cardPool$cardText,function(x) grepl("damage",x,ignore.case=T) & grepl("ALL",x,ignore.case=F)))
hasSilence<-unlist(lapply(cardPool$cardText,function(x) grepl("silence",x,ignore.case=T)))
hasCharge<-unlist(lapply(cardPool$cardText,function(x) grepl("charge",x,ignore.case=T)))
hasHeal<-unlist(lapply(cardPool$cardText,function(x) grepl("restore",x,ignore.case=T)))
hasDeathrattle<-unlist(lapply(cardPool$cardText,function(x) grepl("Deathrattle:",x,ignore.case=T)))
hasEnrage<-unlist(lapply(cardPool$cardText,function(x) grepl("Enrage",x,ignore.case=T)))
hasDamage<-unlist(lapply(cardPool$cardText,function(x) grepl("Deal",x,ignore.case=T)))
hasBattlecry<-unlist(lapply(cardPool$cardText,function(x) grepl("Battlecry:",x,ignore.case=T)))
hasFreeze<-unlist(lapply(cardPool$cardText,function(x) grepl("Freeze",x,ignore.case=T)))
hasShield<-unlist(lapply(cardPool$cardText,function(x) grepl("Divine Shield",x,ignore.case=T) & !grepl("lose",x,ignore.case=T)))
hasBuff<-unlist(lapply(cardPool$cardText,function(x) grepl("\\+[0-9]",x,ignore.case=T)))
isBlank<-unlist(lapply(cardPool$cardText,function(x) grepl("undefined",x,ignore.case=T)))

# bind these columns to the original stock cards data frame
card.attr<-data.frame(hasTaunt,
                      hasDraw,
                      hasDestroy,
                      hasAOEdmg,
                      hasSilence,
                      hasCharge,
                      hasHeal,
                      hasDeathrattle,
                      hasEnrage,
                      hasDamage,
                      hasBattlecry,
                      hasFreeze,
                      hasShield,
                      hasBuff,
                      isBlank)

########## Load and clean the arenaRecords
unixEpoch<-ymd("1970-01-01")
# Load in arena eras to automatically select dates of interest
arenaEras<-dbGetQuery(con,"SELECT * FROM statEras")
# Convert Arena Eras to UTC date/times
arenaEras$eraStart<-unixEpoch+seconds(arenaEras$eraStart)
arenaEras$eraEnd<-unixEpoch+seconds(arenaEras$eraEnd)

arenaRecords<-dbGetQuery(con, "SELECT arenaId, arenaPlayerId, arenaClassId, arenaOfficialWins \"officialWins\",
                              arenaOfficialLosses \"officialLosses\", arenaWins \"wins\", arenaLosses \"losses\",
                              arenaRetireEarly \"retire\", arenaStartDate
                              FROM arenaArena") %>%
  filter(!is.na(wins),!is.na(losses),retire==0)

# A list of classes and their corresponding labels was extracted from the SQL database and applied to the record info
classes<-dbGetQuery(con, "SELECT * FROM arenaClass")
arenaRecords$arenaClassId<-factor(arenaRecords$arenaClassId,c(1:9),classes[,2])
# Convert Arena Records to UTC date/times
arenaRecords$arenaStartDate<-unixEpoch+seconds(arenaRecords$arenaStartDate)

# Only take arena entries starting at official release
release.official<-arenaEras[13,4]
release.naxx<-arenaEras[20,4]-days(9) # arena release 9 days before official naxx release
endofdata<-max(arenaRecords$arenaStartDate)

arenaRecords<-filter(arenaRecords,arenaStartDate>=release.official)


########## Select full card records #############
arenaDraftCards<-dbGetQuery(con, "SELECT draftId, cardId, arenaId, rowId, isSelected FROM arenaDraftCards")

arenaDraftPool<-dbGetQuery(con, "SELECT rowId, arenaId, pickNum FROM arenaDraftRow")

# First establish a way to query the full card record by era
# (defaulting to post-release and pre-Naxxramas expansion)
## class = name of class or classes of interest
## eraStart and eraEnd date/time in the format of "2014-03-11 17:25:34 UTC"
fullCardRecord=function(eraStart=release.official,eraEnd=release.naxx,selectsOnly=T){
  # arenaId was incomplete for arenaDraftPool, so 'rowId' was used instead
  cardRecord<-left_join(arenaDraftPool,arenaDraftCards,by="rowId")%>%
    select(-ends_with(".y")) %>%
    rename(arenaId=arenaId.x) %>%
    left_join(arenaRecords,by="arenaId") %>%
    filter(arenaStartDate>=eraStart, arenaStartDate<eraEnd) %>%
    filter(arenaId %in% arenaId[which(pickNum==30)]) %>% # Only consider complete decks
    select(-pickNum, -rowId,-officialWins,-officialLosses) # remove unnecessary columns
  
  if(selectsOnly){cardRecord<-filter(cardRecord,isSelected==1)}
  
  # Rename levels
  levels(cardRecord$arenaClassId)<-c("Druid","Hunter","Mage","Paladin","Priest","Rogue","Shaman","Warlock","Warrior")
  
  return(cardRecord)
}

# Store data for all-time in R object
allTime<-fullCardRecord(eraEnd=endofdata,selectsOnly=F)

# Identify IDs with too many duplicates
dupes<-allTime %>%
  filter(isSelected==1) %>%
  group_by(arenaId,cardId) %>%
  summarise(cardCount=n())

# 6 or more copies of a card considered unreasonable or questionable for comparison
dupeIDs<-unique(dupes[dupes$cardCount>=6,1]$arenaId)
length(dupeIDs)

allTime<-filter(allTime, !(arenaId %in% dupeIDs))

# store useful era records in R objects
vanilla<-fullCardRecord(selectsOnly=F) %>%
  filter(!(arenaId %in% dupeIDs))
naxx<-fullCardRecord(eraStart=release.naxx,selectsOnly=F) %>%
  filter(!(arenaId %in% dupeIDs))

# Function to rank the popularity of cards for high performance decks in a given class (original release)
mostPickedCards=function(whichClass,winrate=c(0:12)){
  cardPoolSpec<-select(filter(vanilla,arenaClassId==whichClass),cardId,arenaClassId,isSelected,wins)
  
  popularity<-cardPoolSpec %>%
    filter(wins %in% winrate,                 # only the win rate of interest
           arenaClassId %in% whichClass) %>%  # only the hero class of interest 
    group_by(arenaClassId,cardId) %>%
    summarise(
      timesPicked=sum(isSelected==1),
      timesSeen=length(cardId),
      fractionPicked=sum(isSelected==1)/length(cardId)
    ) %>%
    ungroup 
  
  return(data.frame(popularity,rank=rank(desc(popularity$fractionPicked),ties.method="first")))
}

# Find Card Swing (difference between having x of that card and 0 of that card)
copyPerformance=function(whichClass,recordDB){
  recordsOfInterest<-filter(recordDB,isSelected==1,arenaClassId==whichClass)
  
  meanWins<-mean(recordsOfInterest$wins)
  
  # Loop through each card to get a 0-4 copy tally of results
  checkCards=NULL
  for(i in unique(recordsOfInterest$cardId)) {
    eachSpread<-recordsOfInterest %>%
      group_by(wins,arenaId) %>%
      summarise(copies=sum(cardId==i)) %>%
      group_by(copies) %>%
      summarise(deckCount=length(copies),
                winDiff=mean(wins)-meanWins) %>% # standard error
      mutate(cardId=i) %>%
      filter(deckCount>=50) %>%
      select(-deckCount)
    checkCards<-rbind(checkCards,eachSpread)
  }
  
  return(checkCards)
}

mage.cardCopies<-copyPerformance("Mage",vanilla)

# How do decks perform relative to the mean *without* each card
mage.zeroSwing<-filter(mage.cardCopies,copies==0) %>%
  rename(baseSwing=winDiff) %>%
  select(-copies)

mageSwing<-filter(vanilla,arenaClassId=="Mage",isSelected==1) %>%
  group_by(arenaId,cardId) %>%
  summarise(copies=n()) %>%
  left_join(mage.zeroSwing,by=c("cardId")) %>%
  left_join(mage.cardCopies,by=c("cardId","copies")) %>%
  rename(copySwing=winDiff) %>%     # the swing for tha tmany copies
  mutate(cardSwing=ifelse(!is.na(copySwing),copySwing-baseSwing,0)) %>% # the swing generated by that card
  group_by(arenaId) %>%
  summarise(deckSwing=sum(cardSwing))
  

##### Create feature set
# For Mage, summarize cards by average of card rank*card attributes
magesPewPew<-filter(vanilla,isSelected==1,arenaClassId=="Mage")
cardPoolRich<-left_join(data.frame(cardPool,card.attr),select(mostPickedCards("Mage",winrate=c(0:12)),cardId,rank),by="cardId")
cardPoolRich$rank<-ifelse(is.na(cardPoolRich$rank), max(cardPoolRich$rank,na.rm=T)+1,cardPoolRich$rank)
medWins<-median(magesPewPew$wins)
mageSet<-magesPewPew %>%
  left_join(cardPoolRich,by="cardId") %>%
  group_by(arenaId) %>%
  summarise(
    #    player=first(arenaPlayerId),
    winCount=first(wins),
    cost.mean=mean(cardCost),
    cost.median=median(cardCost),
    skew=skewness(cardCost),
    rarity=mean(as.numeric(cardRarity)),
    taunt=sum(hasTaunt*rank),
    draw=sum(hasDraw*rank),
    destroy=sum(hasDestroy*rank),
    aoe=sum(hasAOEdmg*rank),
    silence=sum(hasSilence*rank),
    charge=sum(hasCharge*rank),
    heal=sum(hasHeal*rank),
    rattle=sum(hasDeathrattle*rank),
    enrage=sum(hasEnrage*rank),
    damage=sum(hasDamage*rank),
    buff=sum(hasBuff*rank),
    battlecry=sum(hasBattlecry*rank),
    blank=sum(isBlank*rank),
    dmgSpell=sum((hasDamage & cardType=="Spell")*rank),
    minions=sum((cardType=="Minion")*rank),
    spells=sum((cardType=="Spell")*rank),
    classCard=sum((cardClass!="Neutral")*rank),
    avgRank=mean(rank),
    top15=sum(rank<=15)
  ) %>%
  left_join(mageSwing,by=c("arenaId")) %>%
  select(-arenaId)

## Exploratory: correlation
### Correlation
require(corrplot)
corrplot(cor(mageSet),method="ellipse",order="hclust",type="upper")


######## Can PCA be Used? ########
require(psych)
df.scale <- scale(mageSet, center=T, scale=T)
df.pca<- principal(r=df.scale,
                   nfactors=10,
                   covar=FALSE)

KMO(cor(as.matrix(df.scale)))
### Overall MSA = 0.58, PCA may not help.


##################################
### Machine Learning/Modeling ####
##################################

## Set up training and test sets
df<- mageSet %>%
  mutate(perf=as.factor(ifelse(winCount>mean(winCount),"Good","Bad"))) %>%
  select(-winCount)

labelName<-"perf" # name of observation
predictors<-names(df)[!(names(df) %in% labelName)] # predictors
set.seed(1337)
labelNum<-which(names(df)==labelName) 
inTrain<-createDataPartition(y=df$perf,p=0.75,list=F) 
training<-df[inTrain,]
test<-df[-inTrain,]

# Check near-zero variance
nearZeroVar(training[,predictors],saveMetrics=T)


trControl <- trainControl(method="repeatedcv", number=10, repeats=3,
                          classProbs=T,summaryFunction=twoClassSummary)


##### Select Features with RFE
# define the control using a recursive variable selection function
rfeControl <- rfeControl(functions=rfFuncs, method="cv", number=10)
set.seed(1337)
mage.rfe <- rfe(training[,predictors], training$perf, sizes=c(1:20), rfeControl=rfeControl)
print(mage.rfe)
topPredictors<-predictors(mage.rfe) # store the subset of most important features
plot(mage.rfe, type=c("g", "o")) # Plot performance vs. number of variables
save(mage.rfe,file="mageRFE")

######### Try RF #########
set.seed(1337)
mage.rf<-train(perf~.,
               data=training[,c(topPredictors,labelName)],
               method='rf',
               trControl=trControl,
               preProc = c("center","scale"))

pred.rf<-predict(mage.rf,test[,c(topPredictors,labelName)])
perf.rf<-confusionMatrix(pred.rf,test$perf)
# varImp(mage.rf)
# plot(varImp(mage.rf, scale=T)) # plot the variable importance


######### GBM #########
set.seed(1337)
mage.gbm<-train(perf~.,
                data=training[,c(topPredictors,labelName)],
                method='gbm',
                trControl=trControl,
                metric="ROC",
                preProc = c("center","scale"))

pred.gbm<-predict(mage.gbm,test[,c(topPredictors,labelName)])
perf.gbm<-confusionMatrix(pred.gbm,test$perf)
prob.gbm<-predict(mage.gbm,test[,c(topPredictors,labelName)],type="prob")
plot.roc(roc(response=test$perf,prob.gbm2c$Good))

pred.rocr.gbm<-prediction(prob.gbm$Good,labels=test$perf)
perf.rocr.gbm<-performance(pred.rocr.gbm,"tpr","fpr")


##### No Middle #####
df<- mageSet[mageSet$winCount>7 | mageSet$winCount<3,] %>%
  mutate(perf=as.factor(ifelse(winCount>7,"Good","Bad"))) %>%
  select(-winCount)

labelName<-"perf" # name of observation
predictors<-names(df)[!(names(df) %in% labelName)] # predictors
set.seed(1337)
labelNum<-which(names(df)==labelName) 
inTrain<-createDataPartition(y=df$perf,p=0.75,list=F) 
training<-df[inTrain,]
test<-df[-inTrain,]

# Check near-zero variance
nearZeroVar(training[,predictors],saveMetrics=T)


trControl <- trainControl(method="repeatedcv", number=10, repeats=3,
                          classProbs=T,summaryFunction=twoClassSummary)

set.seed(1337)
mage.gbmx<-train(perf~.,
                  data=training[,c(topPredictors,labelName)],
                  method='gbm',
                  trControl=trControl,
                  metric="ROC",
                  preProc = c("center","scale"))

pred.gbmx<-predict(mage.gbmx,test[,c(topPredictors,labelName)])
perf.gbmx<-confusionMatrix(pred.gbmx,test$perf)
prob.gbmx<-predict(mage.gbmx,test[,c(topPredictors,labelName)],type="prob")
plot.roc(roc(response=test$perf,prob.gbmx$Good))

pred.rocr.x<-prediction(prob.gbmx$Good,labels=test$perf)
perf.rocr.x<-performance(pred.rocr.x,"tpr","fpr")

plot(perf.rocr.x,lwd=2,col=506,main="GBM Performance for Extremes and Total Data Set")
plot(perf.rocr.gbm,add=TRUE,lty=2,col="black",lwd=2)


pred.gbmx.full<-predict(mage.gbmx,test[,c(topPredictors,labelName)])
perf.gbmx.full<-confusionMatrix(pred.gbmx.full,test$perf)
prob.gbmx.full<-predict(mage.gbmx,test[,c(topPredictors,labelName)],type="prob")
plot.roc(roc(response=test$perf,prob.gbmx.full$Good))





##### Stuff that didn't really work #####
##### GLM #####
df<- mageSet

labelName<-"winCount" # name of observation
predictors<-names(df)[!(names(df) %in% labelName)] # predictors
set.seed(1337)
labelNum<-which(names(df)==labelName) 
inTrain<-createDataPartition(y=df$winCount,p=0.75,list=F) 
training<-df[inTrain,]
test<-df[-inTrain,]

# Check near-zero variance
nearZeroVar(training[,predictors],saveMetrics=T)


trControl <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(1337)
mage.glm<-train(winCount~.,
                data=training,
                method='bayesglm',
                trControl=trControl,
                preProc = c("center","scale"))

pred.glm<-predict(mage.glm,test[,predictors])
perf.glm<-confusionMatrix(pred.glm,test$winCount)
plot.roc(roc(response=test$winCount,pred.glm))