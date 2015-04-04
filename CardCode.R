require(ggplot2)
require(dplyr)
require(RMySQL)

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

# Code in Card Attributes
hasTaunt<-unlist(lapply(stockCards.abbr$cardText,function(x) grepl("Taunt",x,ignore.case=T) & !grepl("destroy",x,ignore.case=T)))
hasDraw<-unlist(lapply(stockCards.abbr$cardText,function(x) grepl("Draw",x,ignore.case=T)))
hasDestroy<-unlist(lapply(stockCards.abbr$cardText,function(x) (grepl("Destroy",x,ignore.case=T) & grepl("minion",x,ignore.case=T))))
hasAOEdmg<-unlist(lapply(stockCards.abbr$cardText,function(x) grepl("damage",x,ignore.case=T) & grepl("ALL",x,ignore.case=T)))
hasSilence<-unlist(lapply(stockCards.abbr$cardText,function(x) grepl("silence",x,ignore.case=T)))
hasCharge<-unlist(lapply(stockCards.abbr$cardText,function(x) grepl("charge",x,ignore.case=T)))
hasHeal<-unlist(lapply(stockCards.abbr$cardText,function(x) grepl("restore",x,ignore.case=T)))
hasDeathrattle<-unlist(lapply(stockCards.abbr$cardText,function(x) grepl("Deathrattle:",x,ignore.case=T)))
hasEnrage<-unlist(lapply(stockCards.abbr$cardText,function(x) grepl("Enrage",x,ignore.case=T)))

# bind these columns to the original stock cards data frame
cardPool<-cbind(cardPool,
                       hasTaunt,
                       hasDraw,
                       hasDestroy,
                       hasAOEdmg,
                       hasSilence,
                       hasCharge,
                       hasHeal,
                       hasDeathrattle,
                       hasEnrage)

# Plot card cost distribution
qplot(stockCards.abbr$cardCost,bin=1,fill=I("gold"),color=I("black"))+
  xlab("Card Cost")+
  scale_x_continuous(breaks=seq(0,10,by=1),limits=c(0,11))

# Get arena eras
arenaEras<-dbGetQuery(con,"SELECT * FROM statEras")

# Get arena record entries of interest
arenaRecords.abbr<-dbGetQuery(con, "SELECT arenaId, arenaPlayerId, arenaClassId, arenaOfficialWins \"officialWins\",
                              arenaOfficialLosses \"officialLosses\", arenaWins \"wins\", arenaLosses \"losses\",
                              arenaRetireEarly \"retire\", arenaStartDate
                              FROM arenaArena") %>%
  filter(!is.na(wins),!is.na(losses),retire==0)

# A list of classes and their corresponding factor labels
classes<-dbGetQuery(con, "SELECT * FROM arenaClass")
arenaRecords.abbr$arenaClassId<-factor(arenaRecords.abbr$arenaClassId,c(1:9),classes[,2])
head(arenaRecords.abbr,n=5)

# Fration of over-reporting wins
with(arenaRecords.abbr, sum(officialWins-wins,na.rm=T)/sum(officialWins,na.rm=T))

# Fration of under-reporting losses
with(arenaRecords.abbr, sum(officialLosses-losses,na.rm=T)/sum(officialLosses,na.rm=T))

# Retreive draft cards and draft pool
arenaDraftCards.full<-dbGetQuery(con, "SELECT * FROM arenaDraftCards")
arenaDraftPool<-dbGetQuery(con, "SELECT rowId, arenaId, pickNum FROM arenaDraftRow")

# First establish a way to query the full card record by era (defaulting to post-release and pre-expansion)
fullCardRecord=function(eraStart=13,eraEnd=20){
  left_join(arenaDraftPool,arenaDraftCards.full,by="rowId")%>%
    left_join(stockCards.abbr,by="cardId") %>%
    select(-ends_with(".y")) %>%
    rename(arenaId=arenaId.x, pickNum=pickNum.x) %>%
    left_join(arenaRecords.abbr,by="arenaId") %>%
    filter(arenaStartDate>=arenaEras[eraStart,4], arenaStartDate<arenaEras[eraEnd,4]) %>%
    filter(arenaId %in% arenaId[which(pickNum==30)]) # Only consider complete decks
}

# Filter for post-release, pre-expansion, selected cards
fullCardRecord.selects<-fullCardRecord() %>% filter(isSelected==1)

# Fraction of completed card picks that were started
sum(arenaDraftPool$pickNum==30)/sum(arenaDraftPool$pickNum==1)

# Find the most picked cards by class
cardPool.abbr<-select(filter(fullCardRecord(),isSelected==0 | isSelected==1),cardName,cardId,cardRarity,cardSet,cardType,arenaClassId,cardClass,isSelected,wins)

mostPickedCards=function(whichClass=c(1:9),winrate=c(0:12)){
  cardPool.abbr %>%
    filter(wins %in% winrate) %>%
    group_by(cardId) %>%
    summarise(
      name=first(cardName),
      type=first(cardType),
      class=first(cardClass),
      cardRarity=first(cardRarity),
      timesPicked=sum(isSelected==1),
      timesSeen=length(cardId),
      percentPicked=timesPicked/timesSeen
    ) %>%
    filter(class==whichClass | class== 0) %>%
    arrange(desc(percentPicked))
}

# Find the popularity of a card (by name) at various win rates
picksbywins=function(whichCard,whichClass=c("Druid", "Hunter", "Mage", 
                                            "Paladin", "Priest","Rogue",
                                            "Shaman","Warlock", "Warrior")){
  pickRate<-cardPool.abbr %>%
    filter(cardName %in% whichCard,arenaClassId %in% whichClass) %>%
    group_by(arenaClassId,cardName,wins) %>%
    summarise(
      timesPicked=sum(isSelected==1),
      timesSeen=length(cardId),
      percentPicked=timesPicked/timesSeen
    )
  ggplot(data=pickRate)+geom_point(aes(x=wins,y=percentPicked,color=cardName))+
    xlab("Number of Wins")+
    ylab("Times Picked / Times Seen")+
    ggtitle("Pick Percentage of Card vs. Win Record")+
    theme_bw()
}

# Example for Argent Squire
picksbywins("Argent Squire")+facet_wrap(~arenaClassId)

# Example for Bloodsail Raider
picksbywins("Bloodsail Raider")+facet_wrap(~arenaClassId)

# Example recommendation system
picksbywins(whichCard=c("Bloodsail Raider","Mad Bomber","Chillwind Yeti"),whichClass="Warlock")+
  facet_wrap(~cardName)+
  theme(legend.position="none")

# Find wins by arena ID
winsbyID<-filter(fullCardRecord(),isSelected==1) %>%
  group_by(arenaId) %>%
  summarise(
    manaCost.median=median(cardCost),
    manaCost.mean=mean(cardCost),
    deckRarity=mean(cardRarity),
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
    )
    #     naxxCount=sum(cardSet==12) # how many expansion cards (used later)
  ) %>%
  left_join(arenaRecords.abbr,by="arenaId")


# Deck records by class
winsbyClass<-winsbyID %>%
  #     filter(arenaStartDate>eraStart,arenaStartDate<eraEnd) %>%
  group_by(arenaClassId,wins) %>%
  summarise(
    deckMean=mean(manaCost.mean),
    deckMedian=mean(manaCost.median),
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
    meanUncat=mean(uncategorized)
    #       meanNaxx=mean(naxxCount)
  )

ggplot()+
  geom_point(data=winsbyClass,aes(x=deckMean,y=wins, color="Mean Cost"))+
  geom_point(data=winsbyClass,aes(x=deckMedian,y=wins,color="Median Cost"))+
  facet_wrap(~arenaClassId,ncol=3)+
  ggtitle("Deck win rate vs. mean and median mana cost")+
  xlab("")+
  ylab("Deck Win Record")+
  theme_bw()+
  theme(legend.title=element_blank())

ggplot()+
  geom_point(data=winsbyClass,aes(x=meanClass,y=wins),color="brown")+
  facet_wrap(~arenaClassId,ncol=3)+
  ggtitle("Deck win rate vs. class card count")+
  xlab("")+
  ylab("Deck Win Record")+
  theme_bw()+
  theme(legend.title=element_blank())

naxxCards<-left_join(arenaDraftPool,arenaDraftCards.full,by="rowId")%>%
  left_join(stockCards.abbr,by="cardId") %>%
  select(-ends_with(".y")) %>%
  rename(arenaId=arenaId.x, pickNum=pickNum.x) %>%
  left_join(arenaRecords.abbr,by="arenaId") %>%
  filter(arenaStartDate>arenaEras[20,4]) %>% 
  filter(arenaId %in% arenaId[which(pickNum==30)]) %>%
  filter(isSelected==1)

winsbyID.naxx<-naxxCards %>%
  group_by(arenaId) %>%
  summarise(
    manaCost.median=median(cardCost),
    manaCost.mean=mean(cardCost),
    deckRarity=mean(cardRarity),
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
                        !(hasEnrage),
    naxxCount=sum(cardSet==12) # how many expansion cards (used later)
  ) %>%
  left_join(arenaRecords.abbr,by="arenaId") %>%
  group_by(arenaClassId,wins) %>%
  summarise(
    deckMean=mean(manaCost.mean),
    deckMedian=mean(manaCost.median),
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
  )

ggplot()+
  geom_point(data=winsbyClass,aes(x=meanClass,y=wins),color="dark red")+
  geom_point(data=winsbyClass.naxx,aes(x=meanClass,y=wins),color="blue")+
  facet_wrap(~arenaClassId,ncol=3,scale="free")





ggplot()+
  geom_point(data=winsbyClass,aes(x=meanClass,y=wins),color="brown")+
  facet_wrap(~arenaClassId,ncol=3)+
  ggtitle("Deck win rate vs. class card count")+
  xlab("")+
  ylab("Deck Win Record")+
  theme_bw()+
  theme(legend.title=element_blank())


naxxCards<-left_join(arenaDraftPool,arenaDraftCards.full,by="rowId")%>%
left_join(stockCards.abbr,by="cardId") %>%
select(-ends_with(".y")) %>%
rename(arenaId=arenaId.x, pickNum=pickNum.x) %>%
left_join(arenaRecords.abbr,by="arenaId") %>%
filter(arenaStartDate>arenaEras[20,4]) %>% 
filter(arenaId %in% arenaId[which(pickNum==30)]) %>%
filter(isSelected==1)

winsbyClass.naxx<-naxxCards %>%
group_by(arenaId) %>%
summarise(
classCount=sum(cardClass!=0),
naxxCount=sum(cardSet==12)
) %>%
left_join(arenaRecords.abbr,by="arenaId")%>%
group_by(arenaClassId,wins) %>%
summarise(
meanClass=mean(classCount),
meanNaxx=mean(naxxCount)
)

ggplot()+
  geom_point(data=winsbyClass,aes(x=meanClass,y=wins),color="dark red")+
  geom_point(data=winsbyClass.naxx,aes(x=meanClass,y=wins),color="blue")+
  facet_wrap(~arenaClassId,ncol=3,scale="free")

ggplot()+
  geom_point(data=winsbyClass.naxx,aes(x=meanNaxx,y=wins),color="dark green")+
  facet_wrap(~arenaClassId,ncol=3)
