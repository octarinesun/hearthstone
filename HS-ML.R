setwd("~/GitHub/NYCDSA/Personal Projects/Hearthstone/")
require(ggplot2)
require(dplyr)
require(RMySQL)
require(lubridate)
require(e1071)
require(leaps)
require(caret)

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
                hasBuff)

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

# store useful era records in R objects
allTime<-fullCardRecord(eraEnd=endofdata,selectsOnly=F)
vanilla<-fullCardRecord(selectsOnly=F)
naxx<-fullCardRecord(eraStart=release.naxx,selectsOnly=F)

save(completeRecords, file="completeRecords")

# Function to rank the popularity of cards for high performance decks in a given class
mostPickedCards=function(whichClass,winrate=c(0:12)){
  cardPoolSpec<-select(filter(allTime,arenaClassId==whichClass),cardId,arenaClassId,isSelected,wins)
  
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

# mostPickedCards("Mage",winrate=12)

##### Create feature set
# For Mage, summarize cards by average of card rank*card attributes
magesPewPew<-filter(allTime,arenaClassId=="Mage",arenaStartDate<=endofdata)
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
  select(-arenaId)

## Exploratory: correlation & Heatmaps
### Correlation
corrplot(cor(mageSet),method="ellipse",order="hclust",type="upper")

### Heatmap
set.seed(1337)
mageSet.pp <- scale(mageSet, center=T, scale=T)
hmStrat<-createDataPartition(y=as.factor(mageSet$winCount),p=0.8,list=F) #strat. partition
heatmap(x=mageSet.pp[-hmStrat,-1]) # heatmap of all predictors

### Exploratory: Linear Model Feature Selection ###
features.regsubs<-regsubsets(x=winCount ~. , data=mageSet[,-1],nvmax=20)
df <- data.frame(
  est = c(summary(features.regsubs)$cp,summary(features.regsubs)$bic,summary(features.regsubs)$adjr2),
  x = rep(1:20, 3),
  type = rep(c("cp", "BIC","Adj_r2"), each = 20)
)

qplot(x, est, data = df, geom = "line" , color=type) +
  theme_bw() + facet_grid(type ~ ., scales = "free_y")

coef(features.regsubs,7)

features.step <- select(mageSet,-arenaId) 
null=lm(winCount~1, data=features.step)
full=lm(winCount~., data=features.step)
mage.step <- step(null, scope=list(upper=full),data=features.step, direction="both")



##################################
######## Time for some ML ########
##################################
# Check near-zero variance
nearZeroVar(training[,predictors],saveMetrics=T)

## Set up training and test sets
df<- mageSet %>%
  mutate(topHalf=as.factor(ifelse(winCount>mean(winCount),"Superior","Inferior"))) %>%
  select(-winCount)
labelName<-"topHalf" # name of observation
predictors<-names(df)[!(names(df) %in% labelName)] # predictors
set.seed(1337)
labelNum<-which(names(df)==labelName) 
inTrain<-createDataPartition(y=df$topHalf,p=0.75,list=F) # create stratified partition, based on observation
training<-df[inTrain,]
test<-df[-inTrain,]

trControl <- trainControl(method="repeatedcv", number=10, repeats=3, classProbs=T)

mage.gbm<-train(topHalf~.,
                data=training,
                method='gbm',
                trControl=trControl,
                preProc = c("center","scale"))

predictions<-predict(mage.gbm,test[,predictors])
perf.gbm<-confusionMatrix(predictions,test$topHalf)


######### Can PCA be Used? #########
require(psych)
df.scale <- scale(mageSet, center=T, scale=T)
df.pca<- principal(r=df.scale,
                   nfactors=10,
                   covar=FALSE)

head(df.pca$scores)
# transformed coordinates along each PCA

# Plot pairs for df.pca
pairs(df.pca$scores[-inTrain,],col=df$topHalf[-inTrain])

KMO(cor(as.matrix(df.scale)))
### Overall MSA > 0.7, PCA should be okay

######### Proceed to GBM with PCA #########
mage.gbmpca<-train(topHalf~.,
                   data=training,
                   method='gbm',
                   trControl=trControl,
                   preProc = "pca")

predictions<-predict(mage.gbmpca,test[,predictors])
perf.gbmpca<-confusionMatrix(predictions,test$topHalf)

######### Proceed to RF #########
mage.rf<-train(topHalf~.,
               data=training,
               method='rf',
               trControl=trControl,
               preProc = c("center","scale"))

predictions<-predict(mage.rf,test[,predictors])
perf.rf<-confusionMatrix(predictions,test$topHalf)
