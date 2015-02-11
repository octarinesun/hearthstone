setwd("~/GitHub/NYCDSA/Personal Projects/Hearthstone")

# require(RJSONIO)
# jsondb<-"AllSets.json"
# 
# AllSets<-fromJSON(jsondb)
# unlist(AllSets)

require(sqldf)
require(plyr)
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

hist(stockCards.abbr$cardCost)

# Breakdown card types (minion,spell,weapon)
stockCards.abbr$cardType<-factor(stockCards.abbr$cardType,levels=c(1,2,3),labels=c("Minion","Spell","Weapon"))
summary(stockCards.abbr$cardType)

# Breakdown card attributes (Taunt, Damage character, Damage minion,Deathrattle:,Draw)
# grep(stockCards.abbr$cardText,pattern="Battlecry:")
# grep(stockCards.abbr$cardText,pattern="Deathrattle:")
# grep(stockCards.abbr$cardText,pattern="Taunt")
# grep(stockCards.abbr$cardText,pattern="all")
# stockCards.abbr[grep(stockCards.abbr$cardText,pattern="deal"),]
# stockCards.abbr[grep(stockCards.abbr$cardText,pattern="Draw"),]
# grep(stockCards.abbr$cardText,pattern="Silence")

# Need official arena wins for games that did not retire early and contain data for deck selection

# First get the complete decks
arenaDraftPool.full<-dbGetQuery(con, "SELECT * FROM arenaDraftRow")
completeDecks.arenaIDs<-with(arenaDraftPool.full, unique(arenaId[pickNum==30]))

# There are this many complete decks in the data set
numCompleteDecks<-length(completeDecks.arenaIDs)

# There are this many total decks that were started at all
numStartedDecks<-length(arenaDraftPool.full[arenaDraftPool.full$pickNum==1,1])

# Proportion
numCompleteDecks/numStartedDecks

# Okay, so what about records and win-rates per arenaId (db = arenaArena)
arenaRecords.full<-dbGetQuery(con, "SELECT * FROM arenaArena")
arenaRecords.abbr<-select(arenaRecords.full,arenaId:arenaClassId,wins=arenaOfficialWins,losses=arenaOfficialLosses,retire=arenaRetireEarly)

# Exploratory: what is the spread of arena wins
head(arenaRecords.full)
hist(arenaRecords.abbr$wins)

# what percentage of early retires
sum(arenaRecords.abbr$retire)/nrow(arenaRecords.abbr)
# [1] 0.006569347

# Should early retires be removed? Essentially, they have given up, most likely because of poor performance
# completeRecords<-intersect(completeDecks,arenaRecords.abbr$arenaId[arenaRecords.abbr$retire==0])

nrow(arenaRecords.abbr[arenaRecords.abbr$arenaId %in% completeDecks.arenaIDs,])
head(arenaRecords.abbr[arenaRecords.abbr$arenaId %in% completeDecks.arenaIDs,])

##### take the 
arenaDraftCards.full<-dbGetQuery(con, "SELECT * FROM arenaDraftCards")
selectedDraftCards<-select(arenaDraftCards.full[which(arenaDraftCards.full$isSelected==1),],arenaId,cardId,pickNum)

length(intersect(arenaRecords.abbr$arenaId,completeDecks.arenaIDs))


# fullCardRecord<-merge(arenaDraftPool.full,arenaDraftCards.full,by="rowId")
# save(fullCardRecord,file="fullCardRecord")
# fullCardRecord<-load("fullCardRecord")

fullCardRecord<-left_join(arenaDraftPool.full,arenaDraftCards.full,by="rowId")

fullCardRecord<-left_join(fullCardRecord,stockCards.abbr,by="cardId")

# Remove extraneous columns
fullCardRecord<-select(fullCardRecord,-arenaId.y,-pickNum.y,-cardRarity.y,-playerNote)
names(fullCardRecord)[3]<-"arenaId"
fullCardRecord<-left_join(fullCardRecord,arenaRecords.abbr,by="arenaId")
fullCardRecord<-fullCardRecord[!is.na(fullCardRecord$wins),]

########### Only deal with selected cards
fullCardRecord.selects<-fullCardRecord[fullCardRecord$isSelected==1,]

## for now, omit the records
# fullCardRecord.selects<-left_join(fullCardRecord.selects,arenaRecords.abbr,by="arenaId")

# winDependencies.byWins<-ddply(fullCardRecord.selects,"wins",summarise,
#                        deckCost.median=median(cardCost),
#                        deckCost.mean=mean(cardCost),
#                        deckRarity=mean(cardRarity.x),
#                        testGrep=sum(grepl("Taunt",cardText))
#                        )
# 
# ggplot()+geom_point(data=winDependencies,aes(wins,deckRarity))
# ggplot()+geom_point(data=winDependencies,aes(wins,deckCost.mean))

winDependencies.byID<-ddply(fullCardRecord.selects,"arenaId",summarise,
                            deckCost.median=median(cardCost),
                            deckCost.mean=mean(cardCost),
                            deckRarity=mean(cardRarity.x)
)

winDependencies.byID<-left_join(winDependencies.byID,arenaRecords.abbr,by="arenaId")
winDependencies.byID$winRate<-winDependencies.byID$wins/(winDependencies.byID$wins+winDependencies.byID$losses)

ggplot()+geom_point(data=winDependencies.byID,aes(wins,deckRarity))
ggplot()+geom_point(data=winDependencies.byID,aes(wins,deckCost.mean))

## Add a Taunts Column (0,1) if that card has taunt
### Find out the median cost of taunt cards and split the taunt values to early or late game taunt
findAttribute<-function(df,attribute){
  if(grepl(attribute,df$cardText,ignore.case=T) & !grepl("with",df$cardText,ignore.case=T)){
    1
  }
}
fullCardRecord.selects$hasTaunt<-findAttribute(fullCardRecord.selects,"Taunt")

sapply(fullCardRecord.selects,function(x) x$hasTaunt<-findAttribute(x,"Taunt"))
## Add a Draw Column (0,1)
### Find the median of draw cards to split the draw values by early or late game draw


############ Rank Cards by how often they are picked
cardPool.abbr<-select(fullCardRecord,cardName,cardId,cardSet,cardType,cardClass,isSelected,wins)

mostPicked<-ddply(cardPool.abbr,"cardId",summarise,
                  name=unique(cardName),
                  type=unique(cardType),
                  class=unique(cardClass),
                  timesPicked=sum(isSelected==1),
                  timesSeen=length(cardId),
                  percentPicked=timesPicked/timesSeen
)
mostPicked<-arrange(mostPicked[mostPicked$type=="Minion",],desc(percentPicked))

#### ----- look at the top 10 winners and losers
cardPool.winners<-cardPool.abbr[cardPool.abbr$wins %in% 10:12,]
mostPicked.winners<-ddply(cardPool.winners,"cardId",summarise,
                          name=unique(cardName),
                          type=unique(cardType),
                          class=unique(cardClass),
                          timesPicked=sum(isSelected==1),
                          timesSeen=length(cardId),
                          percentPicked=timesPicked/timesSeen
)
mostPicked.winners.sort<-arrange(mostPicked.winners[mostPicked.winners$type=="Weapon",],desc(percentPicked))[1:10,]

cardPool.losers<-cardPool.abbr[cardPool.abbr$wins %in% 0:3,]
mostPicked.losers<-ddply(cardPool.losers,"cardId",summarise,
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


# some weird gbm stuff????
y<-c(1:5,1:5)
x<-runif(10,9,20)
a<-as.data.frame(cbind(y,x))
z<-as.data.frame(cbind(y=c(1:5),q=c(T,F,T,F,F)))
