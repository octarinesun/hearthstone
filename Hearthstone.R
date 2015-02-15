setwd("~/GitHub/NYCDSA/Personal Projects/Hearthstone")

# require(RJSONIO)
# jsondb<-"AllSets.json"
# 
# AllSets<-fromJSON(jsondb)
# unlist(AllSets)

require(sqldf)
#require(plyr)
require(dplyr)
require(RMySQL)
require(ggplot2)

# Establish a connection to my local mamp server: note for some reason the socket is required.
db <- src_mysql(dbname = 'AMDB', host = 'localhost', user="root", password="root",unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")
db


drv <- dbDriver("MySQL")
con <- dbConnect(drv, host = 'localhost', user="root", password="root", dbname = 'AMDB',unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")

dbListTables(con)

stockCards.full<-dbGetQuery(con, "SELECT * FROM arenaCards")
stockCards.abbr<-select(stockCards.full,cardId,cardName:cardClass,cardCost:cardText)

# Practice:
# find the histogram of card costs

# hist(stockCards.abbr$cardCost)

# Breakdown card types (minion,spell,weapon)
stockCards.abbr$cardType<-factor(stockCards.abbr$cardType,levels=c(1,2,3),labels=c("Minion","Spell","Weapon"))
summary(stockCards.abbr$cardType)

#### Code in Card Attributes
## Add a Taunt Column
hasTaunt<-lapply(stockCards.abbr$cardText,function(x) 
  grepl("Taunt",x,ignore.case=T) & !grepl("destroy",x,ignore.case=T))
hasTaunt<-unlist(hasTaunt)

## Add a Draw Column (0,1)
### Find the median of draw cards to split the draw values by early or late game draw
hasDraw<-lapply(stockCards.abbr$cardText,function(x) 
  grepl("Draw",x,ignore.case=T))
hasDraw<-unlist(hasDraw)

## Add Destroy column
hasDestroy<-lapply(stockCards.abbr$cardText,function(x) 
  (grepl("Destroy",x,ignore.case=T) & grepl("minion",x,ignore.case=T)))
hasDestroy<-unlist(hasDestroy)

## Affects all enemies
hasAOEdmg<-lapply(stockCards.abbr$cardText,function(x) 
  grepl("damage",x,ignore.case=T) & grepl("ALL",x,ignore.case=T))
hasAOEdmg<-unlist(hasAOEdmg)

## Silence enemies
hasSilence<-lapply(stockCards.abbr$cardText,function(x) 
  grepl("silence",x,ignore.case=T))
hasSilence<-unlist(hasSilence)

stockCards.abbr<-cbind(stockCards.abbr,hasTaunt,hasDraw,hasDestroy,hasAOEdmg,hasSilence)

# Need official arena wins for games that did not retire early and contain data for deck selection

# First get the complete decks
arenaDraftPool.full<-dbGetQuery(con, "SELECT * FROM arenaDraftRow")
# completeDecks.arenaIDs<-with(arenaDraftPool.full, unique(arenaId[pickNum==30]))

# There are this many complete decks in the data set
# numCompleteDecks<-length(completeDecks.arenaIDs)

# There are this many total decks that were started at all
# numStartedDecks<-length(arenaDraftPool.full[arenaDraftPool.full$pickNum==1,1])

# Proportion
# numCompleteDecks/numStartedDecks

# Okay, so what about records and win-rates per arenaId (db = arenaArena)
arenaRecords.full<-dbGetQuery(con, "SELECT * FROM arenaArena")
arenaRecords.abbr<-select(arenaRecords.full,arenaId:arenaClassId,wins=arenaOfficialWins,losses=arenaOfficialLosses,retire=arenaRetireEarly) %>%
  filter(!is.na(wins),!is.na(losses))

# Exploratory: what is the spread of arena wins
head(arenaRecords.full)
hist(arenaRecords.abbr$wins)
rm(arenaRecords.full)

# what percentage of early retires
# sum(arenaRecords.abbr$retire)/nrow(arenaRecords.abbr)
# [1] 0.006569347

# Should early retires be removed? Essentially, they have given up, most likely because of poor performance
# completeRecords<-intersect(completeDecks,arenaRecords.abbr$arenaId[arenaRecords.abbr$retire==0])

# nrow(arenaRecords.abbr[arenaRecords.abbr$arenaId %in% completeDecks.arenaIDs,])
# head(arenaRecords.abbr[arenaRecords.abbr$arenaId %in% completeDecks.arenaIDs,])


arenaDraftCards.full<-dbGetQuery(con, "SELECT * FROM arenaDraftCards")
# selectedDraftCards<-select(arenaDraftCards.full[which(arenaDraftCards.full$isSelected==1),],arenaId,cardId,pickNum)
# 
# length(intersect(arenaRecords.abbr$arenaId,completeDecks.arenaIDs))


# fullCardRecord<-merge(arenaDraftPool.full,arenaDraftCards.full,by="rowId")
# save(fullCardRecord,file="fullCardRecord")
# fullCardRecord<-load("fullCardRecord")

fullCardRecord.selects<-left_join(arenaDraftPool.full,arenaDraftCards.full,by="rowId")%>%
  left_join(stockCards.abbr,by="cardId") %>%
  select(-ends_with(".y"),-playerNote) %>%
  rename(arenaId=arenaId.x, pickNum=pickNum.x) %>%
  filter(isSelected==1) %>%
  left_join(arenaRecords.abbr,by="arenaId")

# Remove extraneous columns
# fullCardRecord<-select(fullCardRecord,-arenaId.y,-pickNum.y,-cardRarity.y,-playerNote)
# names(fullCardRecord)[3]<-"arenaId"
# fullCardRecord<-left_join(fullCardRecord,arenaRecords.abbr,by="arenaId")
# fullCardRecord<-fullCardRecord[!is.na(fullCardRecord$wins),]

########### Only deal with selected cards
# fullCardRecord.selects<-fullCardRecord[fullCardRecord$isSelected==1,]
# rm(fullCardRecord)
## for now, omit the records
# fullCardRecord.selects<-left_join(fullCardRecord.selects,arenaRecords.abbr,by="arenaId")


# deprecated and replaced with dplyr
# winDependencies.byID<-ddply(fullCardRecord.selects,"arenaId",summarise,
#                             deckCost.median=median(cardCost),
#                             deckCost.mean=mean(cardCost),
#                             deckRarity=mean(cardRarity.x),
#                             tauntCount=sum(hasTaunt),
#                             drawCount=sum(hasDraw),
#                             destroyCount=sum(hasDestroy),
#                             aoeCount=sum(hasAOEdmg),
#                             silenceCount=sum(hasSilence),
#                             minionCount=sum(cardType=="Minion"),
#                             spellCount=sum(cardType=="Spell")
# )


winDependencies.byID<-fullCardRecord.selects %>%
  group_by(arenaId) %>%
  summarise(
    deckCost.median=median(cardCost),
    deckCost.mean=mean(cardCost),
    deckRarity=mean(cardRarity.x),
    tauntCount=sum(hasTaunt),
    drawCount=sum(hasDraw),
    destroyCount=sum(hasDestroy),
    aoeCount=sum(hasAOEdmg),
    silenceCount=sum(hasSilence),
    minionCount=sum(cardType=="Minion"),
    spellCount=sum(cardType=="Spell"),
    classCount=sum(cardClass!=0),
    uncategorized=sum(hasDraw==FALSE & hasDestroy==FALSE & hasAOEdmg==FALSE & hasSilence==FALSE)
  ) %>%
  left_join(arenaRecords.abbr,by="arenaId")

table<-select(winDependencies.byID,wins,tauntCount)
t2<-table(table$wins,table$tauntCount)
heatmap(t2,Rowv=NA,Colv=NA,col=heat.colors(256),scale="column")

ggplot(winDependencies.byID,aes(x=aoeCount,y=wins))+geom_tile(aes(fill=wins))
  
qplot(as.factor(wins),x=drawCount,y=wins,position="jitter",data=winDependencies.byID,alpha=0.0001,xlim=c(0,10))
qplot(as.factor(wins),aoeCount,data=winDependencies.byID[!is.na(winDependencies.byID$wins),],geom="boxplot")+coord_flip()
qplot(y=wins,x=aoeCount,color=nrow(wins),data=winDependencies.byID[!is.na(winDependencies.byID$wins),],
      position="jitter",alpha=1,xlim=c(0,8))

winDependencies.byClass=function(classID=c(1:9)){
winDependencies.byID %>%
  filter(arenaClassId %in% classID) %>%
  group_by(wins) %>%
  summarise(
    deckCost=mean(deckCost.mean),
    meanTaunt=mean(tauntCount),
    meanDraw=mean(drawCount),
    meanDestroy=mean(destroyCount),
    meanAOE=mean(aoeCount),
    meanSilence=mean(silenceCount),
    meanClass=mean(classCount),
    meanSpell=mean(spellCount),
    meanMinion=mean(minionCount),
    meanUncat=mean(uncategorized)
  ) %>%
  filter(!is.na(wins))
}
ggplot(melt(winDependencies.byClass(3)[,1:10],id.vars="wins"),aes(value,wins))+geom_point()+facet_wrap(~variable,ncol=3,scales="free")


## warlock
winDependencies.warlock<-winDependencies.byID %>%
  filter(arenaClassId==8) %>%
  group_by(wins) %>%
  summarise(
    deckCost=mean(deckCost.mean),
    meanTaunt=mean(tauntCount),
    meanDraw=mean(drawCount),
    meanDestroy=mean(destroyCount),
    meanAOE=mean(aoeCount),
    meanSilence=mean(silenceCount),
    meanClass=mean(classCount),
    meanSpell=mean(spellCount),
    meanMinion=mean(minionCount),
    meanUncat=mean(uncategorized)
  ) %>%
  filter(!is.na(wins))

winDependencies.mage<-winDependencies.byID %>%
  filter(arenaClassId==3) %>%
  group_by(wins) %>%
  summarise(
    deckCost=mean(deckCost.mean),
    meanTaunt=mean(tauntCount),
    meanDraw=mean(drawCount),
    meanDestroy=mean(destroyCount),
    meanAOE=mean(aoeCount),
    meanSilence=mean(silenceCount),
    meanClass=mean(classCount),
    meanSpell=mean(spellCount),
    meanMinion=mean(minionCount),
    meanUncat=mean(uncategorized)
  ) %>%
  filter(!is.na(wins))

ggplot()+geom_line(data=winDependencies.byClass(3),aes(meanClass,wins))+geom_line(data=winDependencies.warlock,aes(meanClass,wins))

### visualization of card attributes vs win and count in deck
test<-melt(winDependencies.byID,id.vars="wins") %>%
  filter(variable=="drawCount") %>%
  group_by(value) %>%
  summarise()

qplot(data=test,x=value, y=wins,position="jitter", alpha=0.1)

############ Rank Cards by how often they are picked
cardPool.abbr<-select(fullCardRecord,cardName,cardId,cardSet,cardType,cardClass,isSelected,wins)

mostPicked<-cardPool.abbr %>%
  group_by(cardId) %>%
  summarise(
    name=unique(cardName),
    type=unique(cardType),
    class=unique(cardClass),
    timesPicked=sum(isSelected==1),
    timesSeen=length(cardId),
    percentPicked=timesPicked/timesSeen
  )

mostPicked<-arrange(mostPicked[mostPicked$type=="Minion",],desc(percentPicked))

#### ----- look at the top 10 winners and losers
cardPool.winners<-cardPool.abbr[cardPool.abbr$wins>median(arenaRecords.abbr$wins,na.rm=T),]

mostPicked.winners<-cardPool.winners %>%
  group_by(cardId) %>%
  summarise(
    name=unique(cardName),
    type=unique(cardType),
    class=unique(cardClass),
    timesPicked=sum(isSelected==1),
    timesSeen=length(cardId),
    percentPicked=timesPicked/timesSeen
  )

mostPicked.winners.sort<-arrange(mostPicked.winners,desc(percentPicked))[1:10,]

cardPool.losers<-cardPool.abbr[cardPool.abbr$wins<=median(arenaRecords.abbr$wins,na.rm=T),]
mostPicked.losers<-cardPool.losers %>%
  group_by(cardId) %>%
  summarise(
    name=unique(cardName),
    type=unique(cardType),
    class=unique(cardClass),
    timesPicked=sum(isSelected==1),
    timesSeen=length(cardId),
    percentPicked=timesPicked/timesSeen
  )
mostPicked.losers.sort<-arrange(mostPicked.losers[mostPicked.losers$type=="Weapon",],desc(percentPicked))[1:10,]


##[[ Count how many cards in a deck were from the top 10 choices for that class ]]


##################[ Doing a bit of spring cleaning ]######################

#Up next: associate Draw cards per deck with arenaIds and plot success
## associate mean card rarity with win rate


# TO DO: need to subset completeIDs by those that didn't retire early


#approx 95% of all arenaIds where a card was picked completely log that deck
length(completePicks)/length(allPicks)

arenaDraftCards.full<-dbGetQuery(con, "SELECT * FROM arenaDraftCards")
selectedDraftCards<-select(arenaDraftCards.full[which(arenaDraftCards.full$isSelected==1),],arenaId,cardId,pickNum)

# Going to select only arenaDraftCards entries where isSelected==1 and then associate those with an arenaId from the "completeRecords" dataframe
completeDeckList<-filter(selectedDraftCards,arenaId %in% completeRecords)

deckDetails.complete<-merge(completeDeckList,stockCards.abbr,sort=F)
wholeSet.complete<-merge(deckDetails.complete,arenaRecords.abbr,sort=F)
wholeSet.complete<-select(wholeSet.complete,-retire)

deckDetails.inc<-merge(selectedDraftCards,stockCards.abbr,sort=F)
wholeSet.inc<-merge(deckDetails.inc,arenaRecords.abbr,sort=F)
wholeSet.inc<-select(wholeSet.inc,-retire)

winDependencies<-ddply(wholeSet.inc,"wins",summarise,
                       deckCost.median=median(cardCost),
                       deckCost.mean=mean(cardCost),
                       deckRarity=mean(cardRarity),
                       classCards=length(cardClass!=0),
                       draw=sum(grepl(pattern="Draw",cardText))
)
ggplot()+geom_point(data=winDependencies,aes(wins,deckRarity))
ggplot()+geom_point(data=winDependencies,aes(wins,deckCost.mean))

winDependencies<-ddply(wholeSet.inc,"arenaId",summarise,
                       deckCost.median=median(cardCost),
                       deckCost.mean=mean(cardCost),
                       deckRarity=mean(cardRarity),
                       classCards=sum(cardClass!=0),
                       draw=sum(grepl(pattern="Draw",cardText)),
                       wins=median(wins)
)

ggplot()+geom_point(data=winDependencies,aes(wins,classCards))

# Can I find out what id=1468 had for a deck?
testRows<-arenaDraftRow.full[which(arenaDraftRow.full$arenaId==1468),]
testCards<-arenaDraftCards.full[which(arenaDraftCards.full$arenaId==1468),]
testCards<-testCards[testCards$isSelected==1,]


#Up next: associate Draw cards per deck with arenaIds and plot success
## associate mean card rarity with win rate

# Fun ideas: can I learn how well a set of cards could have done versus how well it actually did based on what someone selected
testDeck.1468<-merge(testCards,stockCards.abbr,by=intersect(names(testCards),names(stockCards.abbr)),sort=F)
mean(testDeck.1468$cardRarity)
sum(testDeck.1468$cardClass!=0)

library(gbm)
hs.mod<-gbm(!is.na(wins.x) ~ as.factor(cardId),
            distribution="multinomial",
            data=fullCardRecord.selects,
            n.trees=3000,
            shrinkage=0.001,
            cv.folds=5,
            verbose=F,
            n.cores=2)

# some weird gbm stuff????
y<-c(1:5,1:5)
x<-runif(10,9,20)
a<-as.data.frame(cbind(y,x))
z<-as.data.frame(cbind(y=c(1:5),q=c(T,F,T,F,F)))
