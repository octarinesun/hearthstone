setwd("~/GitHub/NYCDSA/Personal Projects/Hearthstone")

# require(RJSONIO)
# jsondb<-"AllSets.json"
# 
# AllSets<-fromJSON(jsondb)
# unlist(AllSets)

require(dplyr)
require(RMySQL)
require(ggplot2)
require(corrplot)

# Establish a connection to my local mamp server: note for some reason the socket is required.
db <- src_mysql(dbname = 'AMDB', host = 'localhost', user="root", password="root",unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")
db


drv <- dbDriver("MySQL")
con <- dbConnect(drv, host = 'localhost', user="root", password="root", dbname = 'AMDB',unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")

dbListTables(con)

classes<-dbGetQuery(con, "SELECT * FROM arenaClass")

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
hasTaunt<-unlist(lapply(stockCards.abbr$cardText,function(x) grepl("Taunt",x,ignore.case=T) & !grepl("destroy",x,ignore.case=T)))

## Add a Draw Column (0,1)
hasDraw<-unlist(lapply(stockCards.abbr$cardText,function(x) grepl("Draw",x,ignore.case=T)))

## Add Destroy column
hasDestroy<-unlist(lapply(stockCards.abbr$cardText,function(x) (grepl("Destroy",x,ignore.case=T) & grepl("minion",x,ignore.case=T))))

## Damage all enemies
hasAOEdmg<-unlist(lapply(stockCards.abbr$cardText,function(x) grepl("damage",x,ignore.case=T) & grepl("ALL",x,ignore.case=T)))

## Silence enemies
hasSilence<-unlist(lapply(stockCards.abbr$cardText,function(x) grepl("silence",x,ignore.case=T)))

## Charge
hasCharge<-unlist(lapply(stockCards.abbr$cardText,function(x) grepl("charge",x,ignore.case=T)))

## Restore HP
hasHeal<-unlist(lapply(stockCards.abbr$cardText,function(x) grepl("restore",x,ignore.case=T)))

## Deathrattle
hasDeathrattle<-unlist(lapply(stockCards.abbr$cardText,function(x) grepl("Deathrattle:",x,ignore.case=T)))

## Enrage
hasEnrage<-unlist(lapply(stockCards.abbr$cardText,function(x) grepl("Enrage",x,ignore.case=T)))

stockCards.abbr<-cbind(stockCards.abbr,
                       hasTaunt,
                       hasDraw,
                       hasDestroy,
                       hasAOEdmg,
                       hasSilence,
                       hasCharge,
                       hasHeal,
                       hasDeathrattle,
                       hasEnrage)

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
arenaRecords.abbr<-select(arenaRecords.full,arenaId:arenaClassId,wins=arenaOfficialWins,losses=arenaOfficialLosses,retire=arenaRetireEarly,arenaStartDate) %>%
  filter(!is.na(wins),!is.na(losses))

arenaRecords.abbr$arenaClassId<-factor(arenaRecords.abbr$arenaClassId,c(1:9),classes[,2])

# payments<-select(arenaRecords.full,arenaPaymentType)

arenaEras<-dbGetQuery(con,"SELECT * FROM statEras")

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
    healCount=sum(hasHeal),
    chargeCount=sum(hasCharge),
    deathrattleCount=sum(hasDeathrattle),
    enrageCount=sum(hasEnrage),
    minionCount=sum(cardType=="Minion"),
    spellCount=sum(cardType=="Spell"),
    classCount=sum(cardClass!=0),
    uncategorized=sum(!(hasDraw) & 
                        !(hasDestroy) & 
                        !(hasAOEdmg) & 
                        !(hasSilence) &
                        !(hasHeal) &
                        !(hasCharge) &
                        !(hasDeathrattle) &
                        !(hasEnrage)
    ),
    naxxCount=sum(cardSet==12)
  ) %>%
  left_join(arenaRecords.abbr,by="arenaId")

# filter the win dependencies by all date post official release
winDependencies.byID.vanilla<-filter(winDependencies.byID,arenaStartDate>=arenaEras[13,4],arenaStartDate<arenaEras[19,4])

table<-select(winDependencies.byID,wins,tauntCount)
t2<-table(table$wins,table$tauntCount)
heatmap(t2,Rowv=NA,Colv=NA,col=heat.colors(256),scale="column")

qplot(as.factor(wins),x=aoeCount,y=wins,position="jitter",data=winDependencies.byID,alpha=0.0001,xlim=c(0,10))+facet_wrap(~arenaClassId,ncol=3)
qplot(as.factor(wins),aoeCount,data=winDependencies.byID[!is.na(winDependencies.byID$wins),],geom="boxplot")+coord_flip()
qplot(y=wins,x=aoeCount,color=nrow(wins),data=winDependencies.byID[!is.na(winDependencies.byID$wins),],
      position="jitter",alpha=1,xlim=c(0,8))

winDependencies.byClass=function(eraStart=arenaEras[13,4],eraEnd=arenaEras[20,4]){
  winDependencies.byID %>%
    filter(arenaStartDate>eraStart,arenaStartDate<eraEnd) %>%
    #filter(arenaClassId %in% classID) %>%
    group_by(arenaClassId,wins) %>%
    summarise(
      deckMean=mean(deckCost.mean),
      deckMedian=mean(deckCost.median),
      meanTaunt=mean(tauntCount),
      meanDraw=mean(drawCount),
      meanDestroy=mean(destroyCount),
      meanAOE=mean(aoeCount),
      meanSilence=mean(silenceCount),
      meanHeal=mean(healCount),
      meanCharge=mean(chargeCount),
      meanDeathrattle=mean(deathrattleCount),
      meanEnrage=mean(enrageCount),
      meanClass=mean(classCount),
      meanSpell=mean(spellCount),
      meanMinion=mean(minionCount),
      meanUncat=mean(uncategorized),
      meanNaxx=mean(naxxCount)
    ) %>%
    filter(!is.na(wins))
}

vanillaHS<-winDependencies.byClass(arenaEras[13,4],arenaEras[20,4])
numberOfVanillaGames<-nrow(filter(winDependencies.byID,arenaStartDate>arenaEras[13,4],arenaStartDate<arenaEras[20,4]))
naxxHS<-winDependencies.byClass(arenaEras[20,4],1E10)
numberOfNaxxGames<-nrow(filter(winDependencies.byID,arenaStartDate>arenaEras[20,4]))

# AOE counts
qplot(as.factor(wins),x=meanAOE,y=wins,data=allClasses)+facet_wrap(~arenaClassId,ncol=3,scale="free")

# Draw Counts
qplot(as.factor(wins),x=meanDraw,y=wins,data=allClasses)+facet_wrap(~arenaClassId,ncol=3,scale="free")

# Healing
qplot(as.factor(wins),x=meanHeal,y=wins,data=allClasses)+facet_wrap(~arenaClassId,ncol=3,scale="free")

# Deck Cost
qplot(as.factor(wins),x=deckMean,y=wins,data=allClasses)+facet_wrap(~arenaClassId,ncol=3,scale="free")

# Deck Median
qplot(as.factor(wins),x=deckMedian,y=wins,data=allClasses)+facet_wrap(~arenaClassId,ncol=3,scale="free")

# Class Cards Vanilla
qplot(x=meanClass,y=wins,data=vanillaHS)+geom_point(data=naxxHS,x=meanClass,y=wins)+
  geom_smooth(method="lm",formula=y~x)+
  facet_wrap(~arenaClassId,ncol=3,scale="free")+
  scale_y_continuous(breaks=seq(0,14,by=2))+
  scale_x_continuous(breaks=seq(0,16,by=0.5))

# Class Cards vanilla vs. Naxx
ggplot()+
  geom_point(data=vanillaHS,aes(x=meanClass,y=wins))+
  geom_point(data=naxxHS,aes(x=meanClass,y=wins),color="red")+
  facet_wrap(~arenaClassId,ncol=3,scale="free")

ggplot()+
  geom_point(data=vanillaHS,aes(x=meanClass,y=wins),alpha=0.3)+
  geom_point(data=naxxHS,aes(x=meanClass,y=wins),color="red")+
  facet_wrap(~arenaClassId,ncol=3,scale="free")

# deckMean and median Vanilla
ggplot()+
  geom_point(data=vanillaHS,aes(x=deckMean,y=wins),color="dark red")+
  geom_point(data=vanillaHS,aes(x=deckMedian,y=wins),color="dark blue")+
  facet_wrap(~arenaClassId,ncol=3)

# deckMean and median Naxx
ggplot()+
  geom_point(data=naxxHS,aes(x=deckMean,y=wins),color="red")+
  geom_point(data=naxxHS,aes(x=deckMedian,y=wins),color="blue")+
  facet_wrap(~arenaClassId,ncol=3)

# both plots
ggplot()+
  geom_point(data=vanillaHS,aes(x=deckMean,y=wins),color="dark red")+
  geom_point(data=vanillaHS,aes(x=deckMedian,y=wins),color="dark blue")+
  geom_point(data=naxxHS,aes(x=deckMean,y=wins),color="red")+
  geom_point(data=naxxHS,aes(x=deckMedian,y=wins),color="blue")+
  facet_wrap(~arenaClassId,ncol=3)

## How have naxx cards helped players
ggplot()+
  geom_point(data=naxxHS,aes(x=meanNaxx,y=wins),color="dark red")+
  facet_wrap(~arenaClassId,ncol=3,scale="free")

corrplot(cor(filter(naxxHS,arenaClassId=="Mage")[,2:17]),method="ellipse",type="upper")


# Look at deck skewness per class
ggplot(melt(select(winDependencies.byClass(6),wins,deckMedian,deckMean, meanAOE, meanDraw, meanTaunt,meanMinion),id.vars="wins"),aes(value,wins))+geom_point()+facet_wrap(~variable,ncol=3,scale="free")


## Histogram overview
qplot(wins,data=filter(winDependencies.byID,!is.na(wins)),binwidth=1)+facet_wrap(~arenaClassId)


## warlock vs mage
winDependencies.warlock<-winDependencies.byClass(8)
winDependencies.mage<-winDependencies.byClass(3)

ggplot()+geom_line(data=winDependencies.byClass(9),aes(meanAOE,wins))+geom_line(data=winDependencies.warlock,aes(meanAOE,wins))

### visualization of card attributes vs win and count in deck
test<-melt(winDependencies.byID,id.vars="wins") %>%
  filter(variable=="drawCount") %>%
  group_by(value) %>%
  summarise()

qplot(data=test,x=value, y=wins,position="jitter", alpha=0.1)

############ Rank Cards by how often they are picked
fullCardRecord=function(eraStart=13,eraEnd=20){
  left_join(arenaDraftPool.full,arenaDraftCards.full,by="rowId")%>%
    left_join(stockCards.abbr,by="cardId") %>%
    select(-ends_with(".y"),-playerNote) %>%
    rename(arenaId=arenaId.x, pickNum=pickNum.x) %>%
    left_join(arenaRecords.abbr,by="arenaId") %>%
    filter(arenaStartDate>=arenaEras[eraStart,4], arenaStartDate<arenaEras[eraEnd,4])
}

# 11-12 wins attributes vs. 0-2 attributes
topDecks<-filter(fullCardRecord(13,20),wins>10)
mostPickedAttributes=function(df)
  df %>%
  group_by(arenaClassId) %>%
  summarise(
    aoePercentage=sum(hasAOEdmg==T & isSelected==1,na.rm=T)/sum(hasAOEdmg==T,na.rm=T),
    tauntPercentage=sum(hasTaunt==T & isSelected==1,na.rm=T)/sum(hasTaunt==T,na.rm=T),
    drawPercentage=sum(hasDraw==T & isSelected==1,na.rm=T)/sum(hasDraw==T,na.rm=T),
    classPercentage=sum(cardClass!=0 & isSelected==1,na.rm=T)/sum(cardClass!=0,na.rm=T)
  )
bottomDecks<-filter(fullCardRecord(13,20),wins<3)

ggplot()+geom_point(data=melt(mostPickedAttributes(topDecks),id.vars="arenaClassId"),aes(x=variable,y=value),color="blue")+
  geom_point(data=melt(mostPickedAttributes(bottomDecks),id.vars="arenaClassId"),aes(x=variable,y=value),color="red")+
  ylim(c(0,1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~arenaClassId,ncol=3)


#### Decks without flamestrike or fireball
head(fullCardRecord.selects)
mage<-filter(fullCardRecord.selects,arenaClassId=="Mage")

cardProportions.mage<-mage %>%
  group_by(arenaId) %>%
  summarise(
    fireballCount=sum(cardId==86),
    flamestrikeCount=sum(cardId==296),
    wins=last(wins)
    ) %>%
  group_by(wins) %>%
  summarise(
    noFlamestrike=sum(flamestrikeCount==0)/n(),
    noFireball=sum(fireballCount==0)/n()
    )
qplot(data=melt(cardProportions.mage,id.vars="wins"),x=value,y=wins,color=variable)

##### Look at each class's wins as a function of how often cards in the top 10 are selected

cardPool.abbr<-select(fullCardRecord(13,20),cardName,cardId,cardRarity.x,cardSet,cardType,cardClass,isSelected,wins)

mostPickedCards=function(whichClass){
  cardPool.abbr %>%
    group_by(cardId) %>%
    summarise(
      name=first(cardName),
      type=first(cardType),
      class=first(cardClass),
      timesPicked=sum(isSelected==1),
      timesSeen=length(cardId),
      percentPicked=timesPicked/timesSeen
    ) %>%
    filter(class==whichClass | class== 0) %>%
    arrange(desc(percentPicked))

}

mostPicked.druid<-arrange(filter(mostPickedCards(),class==0 | class == 1),desc(percentPicked))
mostPicked.hunter<-arrange(filter(mostPickedCards(),class==0 | class == 2),desc(percentPicked))
mostPicked.mage<-arrange(filter(mostPickedCards(),class==0 | class == 3),desc(percentPicked))
mostPicked.paladin<-arrange(filter(mostPickedCards(),class==0 | class == 4),desc(percentPicked))
mostPicked.priest<-arrange(filter(mostPickedCards(),class==0 | class == 5),desc(percentPicked))
mostPicked.rogue<-arrange(filter(mostPickedCards(),class==0 | class == 6),desc(percentPicked))
mostPicked.shaman<-arrange(filter(mostPickedCards(),class==0 | class == 7),desc(percentPicked))
mostPicked.warlock<-arrange(filter(mostPickedCards(),class==0 | class == 8),desc(percentPicked))
mostPicked.warrior<-arrange(filter(mostPickedCards(),class==0 | class == 9),desc(percentPicked))

arrange(mostPicked[mostPicked$type=="Spell",],desc(percentPicked))

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


## Work on subsetting based on the presence of a card
head(fullCardRecord.selects)

trimRecord<-select(fullCardRecord.selects,arenaId,cardId,cardName,arenaClassId,wins,losses,arenaStartDate)
head(trimRecord)

cardSelector=function(class=c("Druid","Mage","Hunter","Paladin","Priest","Rogue","Shaman","Warlock","Warrior"),card){
  trimRecord %>%
  group_by(arenaClassId,wins,arenaId) %>%
  filter(arenaClassId %in% class) %>%
  summarise(queryCard=sum(cardName==card))
  ggplot(data=cardSelector,aes(x=wins,fill=as.factor(queryCard)))+geom_histogram(binwidth=1,position="identity",alpha=0.7)
}
ggplot(data=cardSelector,aes(x=wins,fill=as.factor(queryCard)))+geom_histogram(binwidth=1,position="identity",alpha=0.7)

ggplot(data=cardSelector(card="Boulderfist Ogre"),aes(x=wins,color=as.factor(queryCard)))+geom_density()
