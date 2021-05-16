install.packages("data.table")
library(data.table)
library(readr)
library(plyr)
library(tidyverse)
library(dplyr)
install.packages("tidytext")
library(tidytext)
library(stringr)
library(stringi)
library(lubridate)
install.packages("ngram")
library(ngram)
traindata <- data.table(read_csv("project/volume/data/raw/start_train.csv"))
cardtab <- data.table(read_csv("project/volume/data/raw/card_tab.csv"))
test_data<-data.table(read_csv("project/volume/data/raw/start_test.csv"))
settab <-data.table(read_csv("project/volume/data/raw/set_tab.csv"))

################################################################################################
#Analyze structure
str(traindata)
str(cardtab)

#Merge data set 
setkey(traindata,id)
setkey(cardtab,id)
setkey(test_data,id)

train_merg<-merge(traindata,cardtab,all.x=T)

##current price column pluging values for NA in train data set , according to rarity and types and date
unique(train_merg$current_price)

PriceClean<-train_merg[,.(AverageCurrPrice=mean(current_price,na.rm=T)),by=c("rarity","types","current_date")]


PriceClean2<-train_merg[,.(AverageCurrPrice=mean(current_price,na.rm=T)),by=c("rarity","types")]


PriceClean3<-train_merg[,.(AverageCurrPrice=mean(current_price,na.rm=T)),by="types"]

train_merg[is.na(train_merg$current_price)]
PriceClean[is.na(PriceClean$AverageCurrPrice)]
PriceClean2[is.na(PriceClean2$AverageCurrPrice)]
PriceClean3[is.na(PriceClean3$AverageCurrPrice)]

setkey(PriceClean,"rarity","types")
setkey(PriceClean2,"rarity","types")


#filling NA values in price clean by more general average from priceclean2
for (i in  1:nrow(PriceClean))
{
  if(is.na(PriceClean$AverageCurrPrice[i]))
    PriceClean[i,4]<-PriceClean2[rarity==PriceClean[i,rarity] & types==PriceClean[i,types] ][,AverageCurrPrice]
}


for (i in  1:nrow(train_merg))
{
  if(is.na(train_merg$current_price[i]))
    train_merg[i,4]<-PriceClean[(rarity==train_merg[i,rarity] & types==train_merg[i,types] & current_date==train_merg[i,current_date] )][,AverageCurrPrice]
}

for (i in  1:nrow(train_merg))
{
  if(is.na(train_merg$current_price[i]))
    train_merg[i,4]<-PriceClean3[types==train_merg[i,types]][,AverageCurrPrice]
}

setkey(train_merg,id)
##################################################################################################################
#master data #
##############
#Merging data with card information and setting a column to identify weather from train or not then rbind
train_merg$train<-1
test_mas<-merge(test_data,cardtab,all.x=T)
test_mas$train<-0
test_mas$future_price=0
master<-rbind(train_merg,test_mas)


###############################################################################################################
#################  
#  Clean data   #
#################

#Analyze data table 
str(master)

#Find categories of each variable 
unique(master$mana_cost)
unique(master$colors)
unique(master$type)
unique(master$types)
unique(master$supertypes)
unique(master$subtypes)
unique(master$rarity)
unique(master$power)
unique(master$toughness)
unique(master$loyalty)
unique(master$set)
unique(master$set_name)
unique(master$cmc)

#Mana column
##############
#num of colors which indicates how many unique colors are in mana_cost, Na to zero 
master$num_colors<-stri_count_words(strsplit(master$colors,split=","))
master$num_colors<-nafill(master$num_colors,fill=0)
#num of mana symbols count number of mana symbols doesnot need to be unique , but first accounting for {v/v} 
master$num_mana<-str_count(master$mana_cost,"[A-Z]")
master$num_mana<-nafill(master$num_mana,fill=0)
#cost contains X weather or not there is an X in cost 
master$contains_X<-ifelse(str_detect(master$mana_cost,"X"),yes=1,no=0)
master$contains_X<-nafill(master$contains_X,fill=0)

#no need mana column
master$mana_cost=NULL


#Colors column 
##################

#Fix colour column into sub columns of colours , replacing Na into clear first 
master$colors<-replace(master$colors,is.na(master$colors),"Clear")

master$White<-ifelse(str_detect(master$colors,"White"),yes=1,no=0)
master$Blue<-ifelse(str_detect(master$colors,"Blue"),yes=1,no=0)
master$Red<-ifelse(str_detect(master$colors,"Red"),yes=1,no=0)
master$Black<-ifelse(str_detect(master$colors,"Black"),yes=1,no=0)
master$Green<-ifelse(str_detect(master$colors,"Green"),yes=1,no=0)
master$Clear<-ifelse(str_detect(master$colors,"Clear"),yes=1,no=0)
#no need for colors column
master$colors=NULL

#Type,Types,supertypes,Subtype columns
######################################
#supertypes classification
master$supertypes<-replace(master$supertypes,is.na(master$supertypes),"None")
master$Legendary<-ifelse(master$supertypes=="Legendary",yes=1,no=0)
master$Basic<-ifelse(master$supertypes=="Basic",yes=1,no=0)

#subtypes Na to None 
master$subtypes<-replace(master$subtypes,is.na(master$subtypes),"None")

#Classify into unique types, dummy variable for each categorial variable 
master$Creature<-ifelse(str_detect(master$types,"Creature"),yes=1,no=0)
master$Instant<-ifelse(str_detect(master$types,"Instant"),yes=1,no=0)
master$Enchantment<-ifelse(str_detect(master$types,"Enchantment"),yes=1,no=0)
master$Sorcery<-ifelse(str_detect(master$types,"Sorcery"),yes=1,no=0)
master$Artifact<-ifelse(str_detect(master$types,"Artifact"),yes=1,no=0)
master$Land<-ifelse(str_detect(master$types,"Land"),yes=1,no=0)
master$Planeswalker<-ifelse(str_detect(master$types,"Planeswalker"),yes=1,no=0)
master$Aura<-ifelse(str_detect(master$subtypes,"Aura"),yes=1,no=0)

#no need for supertypes,types anymore 
master$types=NULL
master$supertypes=NULL
master$type=NULL
master$subtypes=NULL


#Rarity Column
##############
#Rarity dummy variables
master$Uncommon<-ifelse(master$rarity=="Uncommon",yes=1,no=0)
master$Common<-ifelse(master$rarity=="Common",yes=1,no=0)
master$Rare<-ifelse(master$rarity=="Rare",yes=1,no=0)
master$Mythic<-ifelse(master$rarity=="Mythic",yes=1,no=0)
#no need to keep rarity column
master$rarity=NULL

#Power Column
#############
#Na to negative one, testing weather astrekit is found add a binary varaible
master$power<-replace(master$power,is.na(master$power),-1)
master$power_astrk<-ifelse(master$power=="*",yes=1,no=0)
master$power<-replace(master$power,master$power=="*",0)
master$power<-as.numeric(master$power)
master$power<-replace(master$power,is.na(master$power),-1)

#Toughness Column
#################
#Na to negative one , testing weather astrekit is found add a binary varaible
master$toughness<-replace(master$toughness,is.na(master$toughness),-1)
master$tough_astrk<-ifelse(master$toughness=="*",yes=1,no=0)
master$toughness<-replace(master$toughness,master$toughness=="*",0)
master$toughness<-as.numeric(master$toughness)


#Creature Classification based on Power and toughness
#####################################################
# master$Offensive<-ifelse(master$power>master$toughness & master$Creature==1,yes=1,no=0)
# master$Defensive<-ifelse(master$power<master$toughness & master$Creature==1,yes=1,no=0)
# master$Balanced<-ifelse(master$power==master$toughness & master$Creature==1,yes=1,no=0)

#Loyalty column
###############
#Na to megative one  "may change to a characteristic column " since only applies to certain type
master$loyalty<-replace(master$loyalty,is.na(master$loyalty),"-1")
master$loyalty<-as.numeric(master$loyalty)
master$loyalty<-replace(master$loyalty,is.na(master$loyalty),-1)


#text insight "keywords"
########################
#Change Na in text to "None"
master$text<-replace(master$text,is.na(master$text),"None")
#Keyword abilities from text into binary 

master$Indestructible<-ifelse(str_detect(master$text,"Indestructible"),yes=1,no=0)
master$Lifelink<-ifelse(str_detect(master$text,"Lifelink"),yes=1,no=0)
master$b11<-ifelse(str_detect(master$text,"Frenzy"),yes=1,no=0)
master$c2<-ifelse(str_detect(master$text,"Scry"),yes=1,no=0)
master$d2<-ifelse(str_detect(master$text,"Miracle"),yes=1,no=0)
master$d3<-ifelse(str_detect(master$text,"Soulbond"),yes=1,no=0)
master$f1<-ifelse(str_detect(master$text,"Escalate"),yes=1,no=0)
master$f6<-ifelse(str_detect(master$text,"Eternalize"),yes=1,no=0)
master$Hide<-ifelse(str_detect(master$text,"Hide|hide"),yes=1,no=0)
master$loses_game<-ifelse(str_detect(master$text,"loses the game"),yes=1,no=0)
master$Meld<-ifelse(str_detect(master$text,"Meld|meld"),yes=1,no=0)
master$Planeswalk<-ifelse(str_detect(master$text,"Planeswalk|planeswalk"),yes=1,no=0)
master$Populate<-ifelse(str_detect(master$text,"Populate|populate"),yes=1,no=0)
master$Regenerate<-ifelse(str_detect(master$text,"Regenerate|regenerate"),yes=1,no=0)
master$Shuffle<-ifelse(str_detect(master$text,"Shuffle|shuffle"),yes=1,no=0)
master$Transform<-ifelse(str_detect(master$text,"Transform|transform"),yes=1,no=0)
master$Flash<-ifelse(str_detect(master$text,"Flash"),yes=1,no=0)
master$Flying<-ifelse(str_detect(master$text,"Flying"),yes=1,no=0)
master$Haste<-ifelse(str_detect(master$text,"Haste"),yes=1,no=0)
master$Hexproof<-ifelse(str_detect(master$text,"Hexproof"),yes=1,no=0)
master$a5<-ifelse(str_detect(master$text,"Cycling"),yes=1,no=0)
master$a8<-ifelse(str_detect(master$text,"Flashback"),yes=1,no=0)
master$b6<-ifelse(str_detect(master$text,"Convoke"),yes=1,no=0)
master$e1<-ifelse(str_detect(master$text,"Prowess"),yes=1,no=0)
master$e2<-ifelse(str_detect(master$text,"Dash"),yes=1,no=0)
master$e3<-ifelse(str_detect(master$text,"Exploit"),yes=1,no=0)
master$e4<-ifelse(str_detect(master$text,"Menace"),yes=1,no=0)
master$e7<-ifelse(str_detect(master$text,"Devoid"),yes=1,no=0)
master$c8<-ifelse(str_detect(master$text,"Exalted"),yes=1,no=0)
master$g3<-ifelse(str_detect(master$text,"Companion"),yes=1,no=0)
master$Exchange<-ifelse(str_detect(master$text,"Exchange|exchange"),yes=1,no=0)





#dealing damage
master$deals_dam<-0
master$deals_dam<-str_extract(master$text, "(?i)(?<=deals\\D)\\d+")
master$deals_dam<-nafill(as.numeric(master$deals_dam),fill=0)

#gaining life
master$gain_lif<-0
master$gain_lif<-str_extract(master$text, "(?i)(?<=gain\\D)\\d+")
master$gain_lif<-nafill(as.numeric(master$gain_lif),fill=0)

#losing life 
master$lose_lif<-0
master$lose_lif<-str_extract(master$text, "(?i)(?<=loses\\D)\\d+")
master$lose_lif<-nafill(as.numeric(master$lose_lif),fill=0)


#Feature of reduction or bonus in power/toughness

master$feature<-str_extract(master$text,"[\\^-|\\^+]+[0-9]+/.+[0-9]/*")
master$feature<-substr(master$feature,1,5)
master$feature<-replace(master$feature,is.na(master$feature),"")


master$bon_pow<-0
master$bon_toug<-0
master$red_pow<-0
master$red_toug<-0


for (i in 1:nrow(master))
{ temp3<-master[i,"feature"][[1]]
 
  if(!(temp3 == ""))
    {if(substr(temp3,1,1) =="+")
      master[i,"bon_pow"]<-as.numeric(substr(temp3,2,((str_locate(temp3,"/")[[1]]))-1) )
    if(substr(temp3,1,1) =="-")
      master[i,"red_pow"]<-as.numeric(substr(temp3,1,((str_locate(temp3,"/")[[1]]))-1) )
    if(substr(temp3,((str_locate(temp3,"/")[[1]])+1),((str_locate(temp3,"/")[[1]])+1)) =="+")
      master[i,"bon_toug"]<-as.numeric(substr(temp3,(str_locate(temp3,"/")[[1]])+2,((str_locate(temp3,"/")[[1]]))+2) )
    if(substr(temp3,(str_locate(temp3,"/")[[1]])+1,(str_locate(temp3,"/")[[1]])+1) =="-")
      master[i,"red_toug"]<-as.numeric(substr(temp3,(str_locate(temp3,"/")[[1]])+1,((str_locate(temp3,"/")[[1]]))+2) )
    }
  else
    next
}

master$bon_pow<-nafill(master$bon_pow,fill = 0)
master$red_pow<-nafill(master$red_pow,fill = 0 )
master$bon_toug<-nafill(master$bon_toug,fill = 0)
master$red_toug<-nafill(master$red_toug,fill = 0)


#No need for text and feature  anymore 
master$text=NULL
master$feature=NULL

#add if expansion or not
########################
setkey(settab,set)
settab[,"test"]=NULL
setkey(master,set)

master_all<-merge(master,settab,all.x=T)

master$Expansion<-ifelse(str_detect(master_all$type,"expansion"),yes=1,no=0)





####################################
#Seperate again into train and test#
####################################
#Subsetting train and test data by trai column intialized earlier
trainprocess<-master[train==1]
testprocess<-master[train==0]



#writing files into data/proccessed
write_csv(trainprocess,"project/volume/data/processed/Clean_train.csv")
write_csv(testprocess,"project/volume/data/processed/Clean_test.csv")



#############################
#Not needed code for refence#
#############################
# master$mana_cost<-str_replace_all(master$mana_cost,"[{}]","")
# master$mana_cost<-replace_na(master$mana_cost,"")
# master$mana_cost<-gsub("[^A-Z]","",master$mana_cost)
# 
# for (i in 1:nrow(master))
# { 
#   count=0
#   temp<-master[i,7][[1]]
#   count<-rawToChar(unique(charToRaw(temp)))
#   
#   count<-nchar(count)
#   master[i,7]<-count
#   
# }

# # master$Deathtouch<-ifelse(str_detect(master$text,"Deathtouch"),yes=1,no=0)
# # master$Defender<-ifelse(str_detect(master$text,"Defender"),yes=1,no=0)
# # master$DoubleStrike<-ifelse(str_detect(master$text,"Double strike"),yes=1,no=0)
# # master$Enchant<-ifelse(str_detect(master$text,"Enchant"),yes=1,no=0)
# # master$Equip<-ifelse(str_detect(master$text,"Equip"),yes=1,no=0)
# # master$FirstStrike<-ifelse(str_detect(master$text,"First strike"),yes=1,no=0)
# # master$Flash<-ifelse(str_detect(master$text,"Flash"),yes=1,no=0)
# # master$Flying<-ifelse(str_detect(master$text,"Flying"),yes=1,no=0)
# # master$Haste<-ifelse(str_detect(master$text,"Haste"),yes=1,no=0)
# # master$Hexproof<-ifelse(str_detect(master$text,"Hexproof"),yes=1,no=0)
# # master$Indestructible<-ifelse(str_detect(master$text,"Indestructible"),yes=1,no=0)
# # master$Lifelink<-ifelse(str_detect(master$text,"Lifelink"),yes=1,no=0)
# # master$Menace<-ifelse(str_detect(master$text,"Menace"),yes=1,no=0)
# # master$Reach<-ifelse(str_detect(master$text,"Reach"),yes=1,no=0)
# # master$Trample<-ifelse(str_detect(master$text,"Trample"),yes=1,no=0)
# # master$Vigilance<-ifelse(str_detect(master$text,"Vigilance"),yes=1,no=0)
# # master$Destroy<-ifelse(str_detect(master$text,"Destroy"),yes=1,no=0)
# # master$Exile<-ifelse(str_detect(master$text,"Exile"),yes=1,no=0)
# # master$a1<-ifelse(str_detect(master$text,"Intimidate"),yes=1,no=0)
# # master$a2<-ifelse(str_detect(master$text,"Protection"),yes=1,no=0)
# # master$a3<-ifelse(str_detect(master$text,"Rampage"),yes=1,no=0)
# # master$a4<-ifelse(str_detect(master$text,"Shadow"),yes=1,no=0)
# # master$a5<-ifelse(str_detect(master$text,"Cycling"),yes=1,no=0)
# # master$a6<-ifelse(str_detect(master$text,"Fading"),yes=1,no=0)
# # master$a7<-ifelse(str_detect(master$text,"Kicker"),yes=1,no=0)
# # master$a8<-ifelse(str_detect(master$text,"Flashback"),yes=1,no=0)
# # master$a9<-ifelse(str_detect(master$text,"Madness"),yes=1,no=0)
# # master$b1<-ifelse(str_detect(master$text,"Fear"),yes=1,no=0)
# # master$b2<-ifelse(str_detect(master$text,"Morph"),yes=1,no=0)
# # master$b3<-ifelse(str_detect(master$text,"Amplify"),yes=1,no=0)
# # master$b4<-ifelse(str_detect(master$text,"Storm"),yes=1,no=0)
# # master$b5<-ifelse(str_detect(master$text,"Offering"),yes=1,no=0)
# # master$b6<-ifelse(str_detect(master$text,"Convoke"),yes=1,no=0)
# # master$b7<-ifelse(str_detect(master$text,"Bloodthirst"),yes=1,no=0)
# # master$b8<-ifelse(str_detect(master$text,"Haunt"),yes=1,no=0)
# # master$b9<-ifelse(str_detect(master$text,"Graft"),yes=1,no=0)
# # master$b10<-ifelse(str_detect(master$text,"Delve"),yes=1,no=0)
# # master$b11<-ifelse(str_detect(master$text,"Frenzy"),yes=1,no=0)
# # master$c1<-ifelse(str_detect(master$text,"Gravestorm"),yes=1,no=0)
# # master$c2<-ifelse(str_detect(master$text,"Champion"),yes=1,no=0)
# # master$c3<-ifelse(str_detect(master$text,"Prowl"),yes=1,no=0)
# # master$c4<-ifelse(str_detect(master$text,"Persist"),yes=1,no=0)
# # master$c5<-ifelse(str_detect(master$text,"Wither"),yes=1,no=0)
# # master$c6<-ifelse(str_detect(master$text,"Retrace"),yes=1,no=0)
# # master$c7<-ifelse(str_detect(master$text,"Devour"),yes=1,no=0)
# # master$c8<-ifelse(str_detect(master$text,"Exalted"),yes=1,no=0)
# # master$c9<-ifelse(str_detect(master$text,"Rebound"),yes=1,no=0)
# # master$c10<-ifelse(str_detect(master$text,"Infect"),yes=1,no=0)
# # master$d1<-ifelse(str_detect(master$text,"Undying"),yes=1,no=0)
# # master$d2<-ifelse(str_detect(master$text,"Miracle"),yes=1,no=0)
# # master$d3<-ifelse(str_detect(master$text,"Soulbond"),yes=1,no=0)
# # master$d4<-ifelse(str_detect(master$text,"Overload"),yes=1,no=0)
# # master$d5<-ifelse(str_detect(master$text,"Scavenge"),yes=1,no=0)
# # master$d6<-ifelse(str_detect(master$text,'Unleash'),yes=1,no=0)
# # master$d7<-ifelse(str_detect(master$text,"Cipher"),yes=1,no=0)
# # master$d8<-ifelse(str_detect(master$text,"Evolve"),yes=1,no=0)
# # master$d9<-ifelse(str_detect(master$text,"Bestow"),yes=1,no=0)
# # master$d10<-ifelse(str_detect(master$text,"Tribute"),yes=1,no=0)
# # master$e1<-ifelse(str_detect(master$text,"Prowess"),yes=1,no=0)
# # master$e2<-ifelse(str_detect(master$text,"Dash"),yes=1,no=0)
# # master$e3<-ifelse(str_detect(master$text,"Exploit"),yes=1,no=0)
# # master$e4<-ifelse(str_detect(master$text,"Menace"),yes=1,no=0)
# # master$e5<-ifelse(str_detect(master$text,"Renown"),yes=1,no=0)
# # master$e6<-ifelse(str_detect(master$text,"Awaken"),yes=1,no=0)
# # master$e7<-ifelse(str_detect(master$text,"Devoid"),yes=1,no=0)
# # master$e8<-ifelse(str_detect(master$text,"Ingest"),yes=1,no=0)
# # master$e9<-ifelse(str_detect(master$text,"Surge"),yes=1,no=0)
# # master$e10<-ifelse(str_detect(master$text,"Skulk"),yes=1,no=0)
# # master$e11<-ifelse(str_detect(master$text,"Emerge"),yes=1,no=0)
# # master$f1<-ifelse(str_detect(master$text,"Escalate"),yes=1,no=0)
# # master$f2<-ifelse(str_detect(master$text,"Crew"),yes=1,no=0)
# # master$f3<-ifelse(str_detect(master$text,"Fabricate"),yes=1,no=0)
# # master$f4<-ifelse(str_detect(master$text,"Improvise"),yes=1,no=0)
# # master$f5<-ifelse(str_detect(master$text,"Embalm"),yes=1,no=0)
# # master$f6<-ifelse(str_detect(master$text,"Eternalize"),yes=1,no=0)
# # master$f7<-ifelse(str_detect(master$text,"Afflict"),yes=1,no=0)
# # master$f8<-ifelse(str_detect(master$text,"Ascend"),yes=1,no=0)
# # master$f9<-ifelse(str_detect(master$text,"Mentor"),yes=1,no=0)
# # master$f10<-ifelse(str_detect(master$text,"Afterlife"),yes=1,no=0)
# # master$g1<-ifelse(str_detect(master$text,"Riot"),yes=1,no=0)
# # master$g2<-ifelse(str_detect(master$text,"Spectacle"),yes=1,no=0)
# # master$g3<-ifelse(str_detect(master$text,"Companion"),yes=1,no=0)
# # master$g4<-ifelse(str_detect(master$text,"produce"),yes=1,no=0)
# 
# 
# # #master$Abandon<-ifelse(str_detect(master$text,"Abandon|abandon"),yes=1,no=0)
# # master$Activate<-ifelse(str_detect(master$text,"Activate|activate"),yes=1,no=0)
# # master$Adapt<-ifelse(str_detect(master$text,"Adapt|adapt"),yes=1,no=0)
# # master$Amass<-ifelse(str_detect(master$text,"Amass|amass"),yes=1,no=0)
# # master$Ante<-ifelse(str_detect(master$text,"Ante|ante"),yes=1,no=0)
# # master$Assemble<-ifelse(str_detect(master$text,"Assemble|assemble"),yes=1,no=0)
# # master$Attach<-ifelse(str_detect(master$text,"Attach|atach"),yes=1,no=0)
# # 
# # master$Ban<-ifelse(str_detect(master$text,"Ban|ban"),yes=1,no=0)
# # master$Bolster<-ifelse(str_detect(master$text,"Bolster|bolster"),yes=1,no=0)
# # #master$Bury<-ifelse(str_detect(master$text,"Bury|bury"),yes=1,no=0)
# # 
# # unique(master$Goad)
# # 
# # master$Cast<-ifelse(str_detect(master$text,"Cast|cast"),yes=1,no=0)
# # master$Challenge<-ifelse(str_detect(master$text,"Challenge|challenge"),yes=1,no=0)
# # #master$Clash<-ifelse(str_detect(master$text,"Clash|clash"),yes=1,no=0)
# # #master$Combine<-ifelse(str_detect(master$text,"Combine|combine"),yes=1,no=0)
# # master$Counter<-ifelse(str_detect(master$text,"Counter|counter"),yes=1,no=0)
# # master$Crank<-ifelse(str_detect(master$text,"Crank|crank"),yes=1,no=0)
# # master$Create<-ifelse(str_detect(master$text,"Create|create"),yes=1,no=0)
# # 
# # master$Destroy<-ifelse(str_detect(master$text,"Destroy|destroy"),yes=1,no=0)
# # master$Detain<-ifelse(str_detect(master$text,"Detain|detain"),yes=1,no=0)
# # master$Discard<-ifelse(str_detect(master$text,"Discard|discard"),yes=1,no=0)
# # master$Double<-ifelse(str_detect(master$text,"Double|double"),yes=1,no=0)
# # #master$Doubletap<-ifelse(str_detect(master$text,"Double tap|double tap"),yes=1,no=0)
# # 
# # master$Exchange<-ifelse(str_detect(master$text,"Exchange|exchange"),yes=1,no=0)
# # master$Exert<-ifelse(str_detect(master$text,"Exert|exert"),yes=1,no=0)
# # master$Exile<-ifelse(str_detect(master$text,"Exile|exile"),yes=1,no=0)
# # master$Explore<-ifelse(str_detect(master$text,"Explore|explore"),yes=1,no=0)
# # 
# # #master$Fateseal<-ifelse(str_detect(master$text,"Fateseal|fatseal"),yes=1,no=0)
# # master$Fight<-ifelse(str_detect(master$text,"Fight|fight"),yes=1,no=0)
# # 
# # #master$Goad<-ifelse(str_detect(master$text,"Goad|good"),yes=1,no=0)
# # 
# # 
# # 
# # master$Hide<-ifelse(str_detect(master$text,"Hide|hide"),yes=1,no=0)
# # 
# # master$Indestructible<-ifelse(str_detect(master$text,"Indestructible"),yes=1,no=0)
# # #master$Install<-ifelse(str_detect(master$text,"Install|install"),yes=1,no=0)
# # #master$Uninstall<-ifelse(str_detect(master$text,"Uninstall|uninstall"),yes=1,no=0)
# # master$Investigate<-ifelse(str_detect(master$text,"Investigate|investigate"),yes=1,no=0)
# # 
# # master$loses_game<-ifelse(str_detect(master$text,"loses the game"),yes=1,no=0)
# # master$Lifelink<-ifelse(str_detect(master$text,"Lifelink|lifelink"),yes=1,no=0)
# # 
# # master$Manifest<-ifelse(str_detect(master$text,"Manifest|manifest"),yes=1,no=0)
# # master$Meld<-ifelse(str_detect(master$text,"Meld|meld"),yes=1,no=0)
# # #master$Mill<-ifelse(str_detect(master$text,"Mill|mill"),yes=1,no=0)
# # master$Monstrosity<-ifelse(str_detect(master$text,"Monstrosity|monstrosity"),yes=1,no=0)
# # 
# # master$Planeswalk<-ifelse(str_detect(master$text,"Planeswalk|planeswalk"),yes=1,no=0)
# # master$Play<-ifelse(str_detect(master$text,"Play|play"),yes=1,no=0)
# # master$Populate<-ifelse(str_detect(master$text,"Populate|populate"),yes=1,no=0)
# # master$Proliferate<-ifelse(str_detect(master$text,"Proliferate|proliferate"),yes=1,no=0)
# # 
# # master$Reflect<-ifelse(str_detect(master$text,"Reflect|reflect"),yes=1,no=0)
# # master$Regenerate<-ifelse(str_detect(master$text,"Regenerate|regenerate"),yes=1,no=0)
# # master$Reveal<-ifelse(str_detect(master$text,"Reveal|reveal"),yes=1,no=0)
# # 
# # master$Sacrifice<-ifelse(str_detect(master$text,"Sacrifice|sacrifice"),yes=1,no=0)
# # master$Scry<-ifelse(str_detect(master$text,"Scry|scry"),yes=1,no=0)
# # master$Search<-ifelse(str_detect(master$text,"Search|search"),yes=1,no=0)
# # #master$Setinmotion<-ifelse(str_detect(master$text,"Set in motion|set in motion"),yes=1,no=0)
# # master$Shuffle<-ifelse(str_detect(master$text,"Shuffle|shuffle"),yes=1,no=0)
# # master$Spark <-ifelse(str_detect(master$text,"Spark|spark"),yes=1,no=0)
# # master$Support<-ifelse(str_detect(master$text,"Support|support"),yes=1,no=0)
# # master$Surveil<-ifelse(str_detect(master$text,"Surveil|surveil"),yes=1,no=0)
# # master$Switch<-ifelse(str_detect(master$text,"Switch|switch"),yes=1,no=0)
# # 
# # master$Tap<-ifelse(str_detect(master$text,"Tap|tap"),yes=1,no=0)
# # master$Transform<-ifelse(str_detect(master$text,"Transform|transform"),yes=1,no=0)
# # 
# # master$Untap<-ifelse(str_detect(master$text,"Untap|untap"),yes=1,no=0)
# # #master$Upgrade<-ifelse(str_detect(master$text,"Upgrade|upgrade"),yes=1,no=0)
# # 
# # master$Vote<-ifelse(str_detect(master$text,"Vote|vote"),yes=1,no=0)

###########
# 
# master$Activate<-ifelse(str_detect(master$text,"Activate|activate"),yes=1,no=0)
# master$Assemble<-ifelse(str_detect(master$text,"Assemble|assemble"),yes=1,no=0)
# master$Attach<-ifelse(str_detect(master$text,"Attach|atach"),yes=1,no=0)
# 
# master$Ban<-ifelse(str_detect(master$text,"Ban|ban"),yes=1,no=0)
# 
# master$Cast<-ifelse(str_detect(master$text,"Cast|cast"),yes=1,no=0)
# master$Challenge<-ifelse(str_detect(master$text,"Challenge|challenge"),yes=1,no=0)
# 
# master$Counter<-ifelse(str_detect(master$text,"Counter|counter"),yes=1,no=0)
# master$Create<-ifelse(str_detect(master$text,"Create|create"),yes=1,no=0)
# 
# master$Destroy<-ifelse(str_detect(master$text,"Destroy|destroy"),yes=1,no=0)
# master$Detain<-ifelse(str_detect(master$text,"Detain|detain"),yes=1,no=0)
# master$Discard<-ifelse(str_detect(master$text,"Discard|discard"),yes=1,no=0)
# master$Double<-ifelse(str_detect(master$text,"Double|double"),yes=1,no=0)
# 
# 
# master$Exchange<-ifelse(str_detect(master$text,"Exchange|exchange"),yes=1,no=0)
# master$Exert<-ifelse(str_detect(master$text,"Exert|exert"),yes=1,no=0)
# master$Exile<-ifelse(str_detect(master$text,"Exile|exile"),yes=1,no=0)
# master$Explore<-ifelse(str_detect(master$text,"Explore|explore"),yes=1,no=0)
# 
# master$Fight<-ifelse(str_detect(master$text,"Fight|fight"),yes=1,no=0)
# master$Hide<-ifelse(str_detect(master$text,"Hide|hide"),yes=1,no=0)
# 
# master$Indestructible<-ifelse(str_detect(master$text,"Indestructible"),yes=1,no=0)
# 
# master$Investigate<-ifelse(str_detect(master$text,"Investigate|investigate"),yes=1,no=0)
# 
# master$loses_game<-ifelse(str_detect(master$text,"loses the game"),yes=1,no=0)
# master$Lifelink<-ifelse(str_detect(master$text,"Lifelink|lifelink"),yes=1,no=0)
# 
# master$Manifest<-ifelse(str_detect(master$text,"Manifest|manifest"),yes=1,no=0)
# master$Meld<-ifelse(str_detect(master$text,"Meld|meld"),yes=1,no=0)
# 
# master$Monstrosity<-ifelse(str_detect(master$text,"Monstrosity|monstrosity"),yes=1,no=0)
# 
# master$Planeswalk<-ifelse(str_detect(master$text,"Planeswalk|planeswalk"),yes=1,no=0)
# master$Play<-ifelse(str_detect(master$text,"Play|play"),yes=1,no=0)
# master$Populate<-ifelse(str_detect(master$text,"Populate|populate"),yes=1,no=0)
# master$Proliferate<-ifelse(str_detect(master$text,"Proliferate|proliferate"),yes=1,no=0)
# 
# master$Reflect<-ifelse(str_detect(master$text,"Reflect|reflect"),yes=1,no=0)
# master$Regenerate<-ifelse(str_detect(master$text,"Regenerate|regenerate"),yes=1,no=0)
# master$Reveal<-ifelse(str_detect(master$text,"Reveal|reveal"),yes=1,no=0)
# 
# master$Sacrifice<-ifelse(str_detect(master$text,"Sacrifice|sacrifice"),yes=1,no=0)
# master$Scry<-ifelse(str_detect(master$text,"Scry|scry"),yes=1,no=0)
# master$Search<-ifelse(str_detect(master$text,"Search|search"),yes=1,no=0)
# 
# master$Shuffle<-ifelse(str_detect(master$text,"Shuffle|shuffle"),yes=1,no=0)
# master$Spark <-ifelse(str_detect(master$text,"Spark|spark"),yes=1,no=0)
# master$Support<-ifelse(str_detect(master$text,"Support|support"),yes=1,no=0)
# master$Switch<-ifelse(str_detect(master$text,"Switch|switch"),yes=1,no=0)
# 
# master$Tap<-ifelse(str_detect(master$text,"Tap|tap"),yes=1,no=0)
# master$Transform<-ifelse(str_detect(master$text,"Transform|transform"),yes=1,no=0)
# 
# master$Untap<-ifelse(str_detect(master$text,"Untap|untap"),yes=1,no=0)
# 
# 
# master$Deathtouch<-ifelse(str_detect(master$text,"Deathtouch"),yes=1,no=0)
# master$Equip<-ifelse(str_detect(master$text,"Equip"),yes=1,no=0)
# master$Flash<-ifelse(str_detect(master$text,"Flash"),yes=1,no=0)
# master$Flying<-ifelse(str_detect(master$text,"Flying"),yes=1,no=0)
# master$Haste<-ifelse(str_detect(master$text,"Haste"),yes=1,no=0)
# master$Hexproof<-ifelse(str_detect(master$text,"Hexproof"),yes=1,no=0)
# master$Menace<-ifelse(str_detect(master$text,"Menace"),yes=1,no=0)
# master$Reach<-ifelse(str_detect(master$text,"Reach"),yes=1,no=0)
# master$Trample<-ifelse(str_detect(master$text,"Trample"),yes=1,no=0)
# master$Vigilance<-ifelse(str_detect(master$text,"Vigilance"),yes=1,no=0)
# master$a5<-ifelse(str_detect(master$text,"Cycling"),yes=1,no=0)
# master$a8<-ifelse(str_detect(master$text,"Flashback"),yes=1,no=0)
# master$b6<-ifelse(str_detect(master$text,"Convoke"),yes=1,no=0)
# master$b10<-ifelse(str_detect(master$text,"Delve"),yes=1,no=0)
# master$b11<-ifelse(str_detect(master$text,"Frenzy"),yes=1,no=0)
# master$d2<-ifelse(str_detect(master$text,"Miracle"),yes=1,no=0)
# master$e1<-ifelse(str_detect(master$text,"Prowess"),yes=1,no=0)
# master$e2<-ifelse(str_detect(master$text,"Dash"),yes=1,no=0)
# master$e4<-ifelse(str_detect(master$text,"Menace"),yes=1,no=0)
# master$e5<-ifelse(str_detect(master$text,"Persist"),yes=1,no=0)
# master$e6<-ifelse(str_detect(master$text,"Level Up"),yes=1,no=0)
# master$e7<-ifelse(str_detect(master$text,"Evoke"),yes=1,no=0)
# master$e8<-ifelse(str_detect(master$text,"Delirium"),yes=1,no=0)
# master$f1<-ifelse(str_detect(master$text,"Escalate"),yes=1,no=0)


