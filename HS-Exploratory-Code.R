
# Load Libraries
require(ggplot2)    # plotting package
require(dplyr)      # for working with dataframes
require(RMySQL)     # necessary for working with SQL
require(grid)       # for aid with plot visuals
require(lubridate)  # easy dates
require(kable)      # nice tables in markdown

# Open connections to Local SQL server
db <- src_mysql(dbname = 'AMDB', host = 'localhost', user="root", password="root",unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")

drv <- dbDriver("MySQL")

con <- dbConnect(drv, host = 'localhost', user="root", password="root", dbname = 'AMDB',unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")

# List available Tables
dbListTables(con)

# Read the arena card SQL table
cardPool<-dbGetQuery(con, "SELECT cardId, cardName, cardSet, cardRarity, 
                            cardType, cardClass, cardCost FROM arenaCards")

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

## Exploratory: Card Pool Breakdown
summary(cardPool$cardClass)
summary(cardPool$cardRarity)
prop.table(table(cardPool$cardRarity))
summary(cardPool$cardType)
prop.table(table(cardPool$cardType))

# Load and clean arena records
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
release.naxx<-arenaEras[20,4]-days(9) # account for early availability of naxx cards
endofdata<-max(arenaRecords$arenaStartDate)

arenaRecords<-filter(arenaRecords,arenaStartDate>=release.official)
head(arenaRecords,n=5)


## Declare function to query arena records
# Load data from database
arenaDraftCards<-dbGetQuery(con, "SELECT draftId, cardId, arenaId, rowId, isSelected FROM arenaDraftCards")

arenaDraftPool<-dbGetQuery(con, "SELECT rowId, arenaId, pickNum FROM arenaDraftRow")

# First establish a way to query the full card record by era
# (defaulting to post-release and pre-Naxxramas expansion)
fullCardRecord=function(eraStart=release.official,eraEnd=release.naxx,selectsOnly=T){
  cardRecord<-left_join(arenaDraftPool,arenaDraftCards,by="rowId")%>%
    left_join(cardPool,by="cardId") %>%
    select(-ends_with(".y")) %>%
    rename(arenaId=arenaId.x) %>%
    left_join(arenaRecords,by="arenaId") %>%
    filter(arenaStartDate>=eraStart, arenaStartDate<eraEnd) %>%
    filter(arenaId %in% arenaId[which(pickNum==30)]) # Only consider complete decks
  
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

## Exploratory: How do the mean wins for those who do and do not record decks differ?
arenaID.rec<-unique(allTime$arenaId)
wins.rec<-filter(arenaRecords,arenaId %in% arenaID.rec)$wins
wins.notrec<-filter(arenaRecords,!(arenaId %in% arenaID.rec))$wins

t.test(wins.rec,wins.notrec, var.equal=FALSE, paired=FALSE)

## Exploratory: How do rare cards affect wins?
# Overall deck success
winsByID<-filter(vanilla,isSelected==1) %>%
  group_by(arenaId) %>%
  summarise(
    manaCost.median=median(cardCost),
    manaCost.mean=mean(cardCost),
    minionCount=sum(cardType=="Minion"),
    spellCount=sum(cardType=="Spell"),
    classCount=sum(cardClass!="Neutral"),
    rareCount=sum(cardRarity=="Rare"),
    epicCount=sum(cardRarity=="Epic"),
    legendCount=sum(cardRarity=="Legendary")
  ) %>%
  left_join(arenaRecords,by="arenaId")

# determine mean wins for each class (for normalization)
meanClassWins<-winsByID %>%
  group_by(arenaClassId) %>%
  summarise(classWinMean=mean(wins))

# how do rare cards affect win rate?
rareWins<-winsByID %>%
  group_by(arenaClassId,cardCount=rareCount) %>%
  summarise(
    meanWins=mean(wins),
    count=length(cardCount),
    se=sd(wins)/sqrt(count)
  )%>%
  filter(count>50) %>%
  left_join(meanClassWins,by="arenaClassId")%>%
  mutate(winDiff=meanWins-classWinMean,
         label="Rare")

# how do epic cards affect win rate?
epicWins<-winsByID %>%
  group_by(arenaClassId,cardCount=epicCount) %>%
  summarise(
    meanWins=mean(wins),
    count=length(cardCount),
    se=sd(wins)/sqrt(count)
  )%>%
  filter(count>50)%>%
  left_join(meanClassWins,by="arenaClassId")%>%
  mutate(winDiff=meanWins-classWinMean,
         label="Epic")

# how do legendary cards affect win rate?
legendWins<-winsByID %>%
  group_by(arenaClassId,cardCount=legendCount) %>%
  summarise(
    meanWins=mean(wins),
    count=length(cardCount),
    se=sd(wins)/sqrt(count)
  )%>%
  filter(count>50)%>%
  left_join(meanClassWins,by="arenaClassId")%>%
  mutate(winDiff=meanWins-classWinMean,
         label="Legendary")

rarity<-rbind(rareWins,epicWins,legendWins)
rarity$label<-factor(rarity$label,levels=c("Rare","Epic","Legendary"))

## Putting it all together in a plot: Win Rate vs. Card Rarity by Class
ggplot(data=rarity)+
  geom_hline(aes(yintercept=0))+
  geom_point(aes(x=cardCount,y=winDiff,color=label))+
  geom_errorbar(aes(ymax=winDiff+se,ymin=winDiff-se,x=cardCount,color=label),width=0.25)+
  scale_colour_manual(values = c("Blue","Purple", "Orange"))+
  xlab("Number of Cards")+
  ylab("Change in Average Wins")+
  ggtitle("Change in Win Rate vs. Number of Uncommon Cards")+
  facet_wrap(~arenaClassId)+
  theme_bw()+
  theme(legend.title=element_blank())+
  scale_x_continuous(breaks=seq(0,10,2))


# Exploratory: Card Cost Distribution
ggplot(data=manaCost)+
  geom_point(aes(x=deckMean,y=wins, color="Mean Cost"))+
  geom_point(aes(x=deckMedian,y=wins,color="Median Cost"))+
  facet_wrap(~arenaClassId,ncol=3)+
  ggtitle("Deck win rate vs. mean and median mana cost")+
  xlab("")+
  ylab("Deck Win Record")+
  theme_bw()+
  theme(legend.title=element_blank())+
  scale_x_continuous(breaks=seq(3,4.5,0.5))

## Find Most Picked Cards
cardPoolFull<-select(vanilla,cardName,cardId,cardRarity,
                     cardType,arenaClassId,cardClass,isSelected,wins)

mostPickedCards=function(whichClass,winrate=c(0:12)){
  cardPoolFull %>%
    filter(wins %in% winrate,             # only the win rate of interest
           arenaClassId %in% whichClass,  # only the hero class of interest
           cardRarity!="Legendary"        # exclude legendary cards
    ) %>% 
    group_by(arenaClassId,cardId) %>%
    summarise(
      name=first(cardName),
      type=first(cardType),
      cardRarity=first(cardRarity),
      timesPicked=sum(isSelected==1),
      timesSeen=length(cardId),
      percentPicked=timesPicked/timesSeen
    ) %>%
    ungroup %>%
    arrange(desc(percentPicked))
}

## Find out how duplicate cards affect wins
copyPerformance=function(whichClass,whichCards){
  recordsOfInterest<-filter(vanilla,arenaClassId==whichClass)
  
  meanWins<-mean(recordsOfInterest$wins)
  
  # Loop through each card to get a 0-4 copy tally of results
  checkCards=NULL
  for(i in whichCards) {
    eachSpread<-recordsOfInterest %>%
      filter(arenaClassId==whichClass) %>%
      group_by(wins,arenaId) %>%
      summarise(copies=sum(cardName==i)) %>%
      group_by(copies) %>%
      summarise(deckCount=length(copies),
                winDiff=mean(wins)-meanWins,
                se=sd(wins)/sqrt(deckCount)) %>% # standard error
      mutate(cardLabel=i) %>%
      filter(deckCount>=50)
    checkCards<-rbind(checkCards,eachSpread)
  }
  
  ggplot(data=checkCards)+
    geom_bar(aes(x=as.factor(copies),y=winDiff,fill=cardLabel),stat="identity")+
    geom_errorbar(aes(ymax=winDiff+se,ymin=winDiff-se,x=as.factor(copies)), width=0.25)+
    xlab("Card Copies")+
    ylab("Change in mean wins")+
    geom_hline(yintercept=0)+
    theme_bw()+
    theme(legend.position="none")+
    facet_wrap(~cardLabel)
}

