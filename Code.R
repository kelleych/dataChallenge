#Change the path 
data <- load("C:/Users/kelle/Downloads/ICPSR_37639/DS0001/37639-0001-Data.rda")
census <- read.csv("C:/Users/kelle/Downloads/sc-est2019-agesex-civ.csv")
census <- census[,c(1:7,17)]

#cleaning the data
census <- subset(census, STATE != 0)
census <- subset(census, NAME != "District of Columbia")
census <- subset(census, NAME != "United States")
census <- subset(census, AGE != 999)
census <- subset(census, SEX == 0)

#only want to take non minors
census <- subset(census, !(AGE %in% minor))

pop <- aggregate(census$POPEST2018_CIV, by=list(Category=census$NAME), FUN=sum)[,2]


#This part might be different
library(prettyR)
lbls <- sort(levels(da37639.0001$MYVAR))
lbls <- (sub("^\\([0-9]+\\) +(.+$)", "\\1", lbls))
da37639.0001$MYVAR <- as.numeric(sub("^\\(0*([0-9]+)\\).+$", "\\1", da99999.0001$MYVAR))

da37639.0001$MYVAR <- add.value.labels(da99999.0001$MYVAR, lbls)
df <- da37639.0001

df <- df[df$YEAR==2018,]
df <- df[,colSums(is.na(df))<nrow(df)]
df<-subset(df, STATE != "US")
df <- subset(df, STATE != "FE")
df <- subset(df, STATE != "DC")
df <- subset(df, STATE != "ST")
#1 is no restriction
#2 is prison only
#3 is prison and parole
#4 prison, parole, and probation
#5 prison, parole, probation, and post-sentence
disfranchisement <- c(5,4,5,4,3,3,3,5,5,4,2,4,2,2,5,4,5,4,1,2,2,2,4,5,4,2,5,5,2,4,4,3,4,2,2,4,2,2,2,4,4,5,4,2,1,5,3,3,3,5)
df <- cbind(df, disfranchisement)
#1 is solid democratc
#2 is lean democratic
#3 is competitive
#4 is lean republican
#5 is solid republican

party <- c(5,5,3,5,1,2,1,1,3,3,1,5,1,2,3,5,3,4,1,1,1,2,2,5,4,4,3,2,1,1,1,1,3,5,3,4,2,2,2,5,5,5,3,5,1,2,1,5,3,5)
df <- cbind(df, party)

party_fac <- c()

for(i in 1:50){
  if(party[i] == 1){
    party_fac <- c(party_fac, "solid dem")
  }
  else if(party[i] == 2){
    party_fac <- c(party_fac, "leaning dem")
  }
  else if(party[i] == 3){
    party_fac <- c(party_fac, "competitive")
  }
  else if(party[i] == 4){
    party_fac <- c(party_fac, "leaning rep")
  }
  else{
    party_fac <- c(party_fac, "solid rep")
  }
}

df[,"disfranchisement"] <- as.factor(df$disfranchisement)
df[,"party"] <- as.factor(df$party)
df <- df[ , colSums(is.na(df)) == 0]

party_disfranchisement <- table(party, disfranchisement)
plot(party, disfranchisement)
plot(df$REGION, disfranchisement)


table(party_fac, disfranchisement)
table(df$REGION, disfranchisement)

#republican states are more strict
#states in the south are more strict, and states in the northeaast 
#are less strict

south <- subset(df, REGION == "(3) South")
midwest <- subset(df, REGION == "(2) Midwest")
northeast <- subset(df, REGION == "(1) Northeast")
west <- subset(df, REGION == "(4) West")

#pie charts


total.race.northeast <- northeast[,"TOTRACEM"] + northeast[,"TOTRACEF"]
white.northeast <- sum(northeast[,"WHITEM"])+sum(northeast[,"WHITEF"])
black.northeast <- sum(northeast[,"BLACKM"]) + sum(northeast[,"BLACKF"])
hisp.northeast <- sum(northeast[,"HISPM"]) + sum(northeast[,"HISPF"])
asian.northeast <- sum(northeast[,"ASIANM"]) + sum(northeast[,"ASIANF"])
other.male.northeast <- sum(northeast[,"ADDRACEM"], sum(northeast[,"NHPIM"]),sum(northeast[,"TWORACEM"]),sum(northeast[,"UNKRACEM"]))
other.female.northeast <- sum(northeast[,"ADDRACEF"], sum(northeast[,"NHPIF"]),sum(northeast[,"TWORACEF"]),sum(northeast[,"UNKRACEF"]))
other.northeast <- other.male.northeast + other.female.northeast

slices.northeast <- c(white.northeast, black.northeast, hisp.northeast, asian.northeast,other.northeast)
lbls <- c("White", "Black", "Hispanic", "Asian", "Other")
pct <- round(slices.northeast/sum(slices.northeast)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices.northeast,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Races under jurisdiction in the northeast ")




total.race.south <- south[,"TOTRACEM"] + south[,"TOTRACEF"]
white.south <- sum(south[,"WHITEM"])+sum(south[,"WHITEF"])
black.south <- sum(south[,"BLACKM"]) + sum(south[,"BLACKF"])
hisp.south <- sum(south[,"HISPM"]) + sum(south[,"HISPF"])
asian.south <- sum(south[,"ASIANM"]) + sum(south[,"ASIANF"])
other.male.south <- sum(south[,"ADDRACEM"], sum(south[,"NHPIM"]),sum(south[,"TWORACEM"]),sum(south[,"UNKRACEM"]))
other.female.south <- sum(south[,"ADDRACEF"], sum(south[,"NHPIF"]),sum(south[,"TWORACEF"]),sum(south[,"UNKRACEF"]))
other.south <- other.male.south + other.female.south

slices.south <- c(white.south, black.south, hisp.south, asian.south,other.south)
lbls <- c("White", "Black", "Hispanic", "Asian", "Other")
pct <- round(slices.south/sum(slices.south)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices.south,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Races under jurisdiction in the south ")



total.race.midwest <- midwest[,"TOTRACEM"] + midwest[,"TOTRACEF"]
white.midwest <- sum(midwest[,"WHITEM"])+sum(midwest[,"WHITEF"])
black.midwest <- sum(midwest[,"BLACKM"]) + sum(midwest[,"BLACKF"])
hisp.midwest <- sum(midwest[,"HISPM"]) + sum(midwest[,"HISPF"])
asian.midwest <- sum(midwest[,"ASIANM"]) + sum(midwest[,"ASIANF"])
other.male.midwest <- sum(midwest[,"ADDRACEM"], sum(midwest[,"NHPIM"]),sum(midwest[,"TWORACEM"]),sum(midwest[,"UNKRACEM"]))
other.female.midwest <- sum(midwest[,"ADDRACEF"], sum(midwest[,"NHPIF"]),sum(midwest[,"TWORACEF"]),sum(midwest[,"UNKRACEF"]))
other.midwest <- other.male.midwest + other.female.midwest

slices.midwest <- c(white.midwest, black.midwest, hisp.midwest, asian.midwest,other.midwest)
lbls <- c("White", "Black", "Hispanic", "Asian", "Other")
pct <- round(slices.midwest/sum(slices.midwest)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices.midwest,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Races under jurisdiction in the midwest ")



total.race.west <- west[,"TOTRACEM"] + west[,"TOTRACEF"]
white.west <- sum(west[,"WHITEM"])+sum(west[,"WHITEF"])
black.west <- sum(west[,"BLACKM"]) + sum(west[,"BLACKF"])
hisp.west <- sum(west[,"HISPM"]) + sum(west[,"HISPF"])
asian.west <- sum(west[,"ASIANM"]) + sum(west[,"ASIANF"])
other.male.west <- sum(west[,"ADDRACEM"], sum(west[,"NHPIM"]),sum(west[,"TWORACEM"]),sum(west[,"UNKRACEM"]))
other.female.west <- sum(west[,"ADDRACEF"], sum(west[,"NHPIF"]),sum(west[,"TWORACEF"]),sum(west[,"UNKRACEF"]))
other.west <- other.male.west + other.female.west

slices.west <- c(white.west, black.west, hisp.west, asian.west,other.west)
lbls <- c("White", "Black", "Hispanic", "Asian", "Other")
pct <- round(slices.west/sum(slices.west)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices.west,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Races under jurisdiction in the west ")

#breakdown of races are similar throughout all regions


rep <- subset(df, party == 5)
lean_rep <- subset(df, party == 4)
comp <- subset(df, party == 3)
lean_dem <- subset(df, party == 2)
dem <- subset(df, party == 1)



total.race.rep <- rep[,"TOTRACEM"] + rep[,"TOTRACEF"]
white.rep <- sum(rep[,"WHITEM"])+sum(rep[,"WHITEF"])
black.rep <- sum(rep[,"BLACKM"]) + sum(rep[,"BLACKF"])
hisp.rep <- sum(rep[,"HISPM"]) + sum(rep[,"HISPF"])
asian.rep <- sum(rep[,"ASIANM"]) + sum(rep[,"ASIANF"])
other.male.rep <- sum(rep[,"ADDRACEM"], sum(rep[,"NHPIM"]),sum(rep[,"TWORACEM"]),sum(rep[,"UNKRACEM"]))
other.female.rep <- sum(rep[,"ADDRACEF"], sum(rep[,"NHPIF"]),sum(rep[,"TWORACEF"]),sum(rep[,"UNKRACEF"]))
other.rep <- other.male.rep + other.female.rep

slices.rep <- c(white.rep, black.rep, hisp.rep, asian.rep,other.rep)
lbls <- c("White", "Black", "Hispanic", "Asian", "Other")
pct <- round(slices.rep/sum(slices.rep)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices.rep,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Races under jurisdiction in the rep ")



total.race.lean_rep <- lean_rep[,"TOTRACEM"] + lean_rep[,"TOTRACEF"]
white.lean_rep <- sum(lean_rep[,"WHITEM"])+sum(lean_rep[,"WHITEF"])
black.lean_rep <- sum(lean_rep[,"BLACKM"]) + sum(lean_rep[,"BLACKF"])
hisp.lean_rep <- sum(lean_rep[,"HISPM"]) + sum(lean_rep[,"HISPF"])
asian.lean_rep <- sum(lean_rep[,"ASIANM"]) + sum(lean_rep[,"ASIANF"])
other.male.lean_rep <- sum(lean_rep[,"ADDRACEM"], sum(lean_rep[,"NHPIM"]),sum(lean_rep[,"TWORACEM"]),sum(lean_rep[,"UNKRACEM"]))
other.female.lean_rep <- sum(lean_rep[,"ADDRACEF"], sum(lean_rep[,"NHPIF"]),sum(lean_rep[,"TWORACEF"]),sum(lean_rep[,"UNKRACEF"]))
other.lean_rep <- other.male.lean_rep + other.female.lean_rep

slices.lean_rep <- c(white.lean_rep, black.lean_rep, hisp.lean_rep, asian.lean_rep,other.lean_rep)
lbls <- c("White", "Black", "Hispanic", "Asian", "Other")
pct <- round(slices.lean_rep/sum(slices.lean_rep)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices.lean_rep,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Races under jurisdiction in the lean_rep ")




total.race.comp <- comp[,"TOTRACEM"] + comp[,"TOTRACEF"]
white.comp <- sum(comp[,"WHITEM"])+sum(comp[,"WHITEF"])
black.comp <- sum(comp[,"BLACKM"]) + sum(comp[,"BLACKF"])
hisp.comp <- sum(comp[,"HISPM"]) + sum(comp[,"HISPF"])
asian.comp <- sum(comp[,"ASIANM"]) + sum(comp[,"ASIANF"])
other.male.comp <- sum(comp[,"ADDRACEM"], sum(comp[,"NHPIM"]),sum(comp[,"TWORACEM"]),sum(comp[,"UNKRACEM"]))
other.female.comp <- sum(comp[,"ADDRACEF"], sum(comp[,"NHPIF"]),sum(comp[,"TWORACEF"]),sum(comp[,"UNKRACEF"]))
other.comp <- other.male.comp + other.female.comp

slices.comp <- c(white.comp, black.comp, hisp.comp, asian.comp,other.comp)
lbls <- c("White", "Black", "Hispanic", "Asian", "Other")
pct <- round(slices.comp/sum(slices.comp)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices.comp,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Races under jurisdiction in the comp ")




total.race.lean_dem <- lean_dem[,"TOTRACEM"] + lean_dem[,"TOTRACEF"]
white.lean_dem <- sum(lean_dem[,"WHITEM"])+sum(lean_dem[,"WHITEF"])
black.lean_dem <- sum(lean_dem[,"BLACKM"]) + sum(lean_dem[,"BLACKF"])
hisp.lean_dem <- sum(lean_dem[,"HISPM"]) + sum(lean_dem[,"HISPF"])
asian.lean_dem <- sum(lean_dem[,"ASIANM"]) + sum(lean_dem[,"ASIANF"])
other.male.lean_dem <- sum(lean_dem[,"ADDRACEM"], sum(lean_dem[,"NHPIM"]),sum(lean_dem[,"TWORACEM"]),sum(lean_dem[,"UNKRACEM"]))
other.female.lean_dem <- sum(lean_dem[,"ADDRACEF"], sum(lean_dem[,"NHPIF"]),sum(lean_dem[,"TWORACEF"]),sum(lean_dem[,"UNKRACEF"]))
other.lean_dem <- other.male.lean_dem + other.female.lean_dem

slices.lean_dem <- c(white.lean_dem, black.lean_dem, hisp.lean_dem, asian.lean_dem,other.lean_dem)
lbls <- c("White", "Black", "Hispanic", "Asian", "Other")
pct <- round(slices.lean_dem/sum(slices.lean_dem)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices.lean_dem,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Races under jurisdiction in the lean_dem ")





total.race.dem <- dem[,"TOTRACEM"] + dem[,"TOTRACEF"]
white.dem <- sum(dem[,"WHITEM"])+sum(dem[,"WHITEF"])
black.dem <- sum(dem[,"BLACKM"]) + sum(dem[,"BLACKF"])
hisp.dem <- sum(dem[,"HISPM"]) + sum(dem[,"HISPF"])
asian.dem <- sum(dem[,"ASIANM"]) + sum(dem[,"ASIANF"])
other.male.dem <- sum(dem[,"ADDRACEM"], sum(dem[,"NHPIM"]),sum(dem[,"TWORACEM"]),sum(dem[,"UNKRACEM"]))
other.female.dem <- sum(dem[,"ADDRACEF"], sum(dem[,"NHPIF"]),sum(dem[,"TWORACEF"]),sum(dem[,"UNKRACEF"]))
other.dem <- other.male.dem + other.female.dem

slices.dem <- c(white.dem, black.dem, hisp.dem, asian.dem,other.dem)
lbls <- c("White", "Black", "Hispanic", "Asian", "Other")
pct <- round(slices.dem/sum(slices.dem)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices.dem,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Races under jurisdiction in the dem ")




CUSTOTT <- df[,"CUSTOTM"] + df[,"CUSTOTF"]
df <- cbind(df, CUSTOTT)
df <- cbind(df, pop)


#calculating the percent of people in custody in each state
percent_cust_df <- data.frame(df[,3:4], df[,"CUSTOTT"], df[,"party"], df["disfranchisement"], pop)
colnames(percent_cust_df) <- c("STATE", "REGION", "CUSTOTT", "PARTY", "DISFRANCHISEMENT", "POP")
percent_cust <- percent_cust_df$CUSTOTT/percent_cust_df$POP
percent_cust_df <- cbind(percent_cust_df, percent_cust)

plot(percent_cust_df$REGION, percent_cust_df$percent_cust)

#The northeast has the lowest percent of people in custody and the other
#three are similar, but the west has the largest range

plot(percent_cust_df$PARTY, percent_cust_df$percent_cust)

#Republican states have a much higher percentage

plot(df$disfranchisement, percent_cust_df$percent_cust)

#check if more races in density and their 

ca.cust <- 127709
ca.voters <- 12712542

percent_cust_vote <- ca.cust/(ca.cust+ca.voters)

#the amount of potential voters prevented from voting in 2018 is
#not enough to change the decision


#use data that shows the number of prisons and connect to restrictions
prison_df <- percent_cust_df
prison_df <- cbind(prison_df, prison_census[,2:7])
prison_df$Total <- prison_df$Total - prison_df$Local - prison_df$Other

total_fac <- c()
for(i in 1:50){
  if(prison_df$Total[i] <= 15){
    total_fac <- c(total_fac, "15 or less")
  }
  else if(prison_df$Total[i] <= 40){
    total_fac <- c(total_fac, "16 to 40")
  }
  else if(prison_df$Total[i] <= 125){
    total_fac <- c(total_fac, "41 to 125")
  }
  else{
    total_fac <- c(total_fac, "126 to 207")
  }
}

prison_df <- cbind(prison_df, total_fac)
table(prison_df$PARTY, prison_df$total_fac)

#leaning republican states have the least states with many prisons, 
#while solid democrat states have the most states with many prisons

table(prison_df$DISFRANCHISEMENT, prison_df$total_fac)

plot(prison_df$total_fac,prison_df$DISFRANCHISEMENT)

#Stricter states tend to have more states with many prisons
