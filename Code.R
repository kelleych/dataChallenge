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
white.male.northeast <- sum(northeast[,"WHITEM"])
white.female.northeast <- sum(northeast[,"WHITEF"])
black.male.northeast <- sum(northeast[,"BLACKM"])
black.female.northeast <- sum(northeast[,"BLACKF"])
hisp.male.northeast <- sum(northeast[,"HISPM"])
hisp.female.northeast <- sum(northeast[,"HISPF"])
asian.male.northeast <- sum(northeast[,"ASIANM"])
asian.female.northeast <- sum(northeast[,"ASIANF"])
nhpi.male.northeast <- sum(northeast[,"NHPIM"])
nhpi.female.northeast <- sum(northeast[,"NHPIF"])
tworace.male.northeast <- sum(northeast[,"TWORACEM"])
tworace.female.northeast <- sum(northeast[,"TWORACEF"])
other.male.northeast <- sum(northeast[,"ADDRACEM"])
other.female.northeast <- sum(northeast[,"ADDRACEF"])
unknown.male.northeast <- sum(northeast[,"UNKRACEM"])
unknown.female.northeast <- sum(northeast[,"UNKRACEF"])

slices.male.northeast <- c(white.male.northeast, black.male.northeast, hisp.male.northeast, asian.male.northeast, nhpi.male.northeast, tworace.male.northeast, other.male.northeast ,unknown.male.northeast)
lbls <- c("White", "Black", "Hispanic", "Asian", "Native Hawaiian and Pacific Islander", "Two Races", "Other")
pct <- round(slices.male.northeast/sum(slices.male.northeast)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices.male.northeast,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Races under jurisdiction in the northeast ")


total.race.south <- south[,"TOTRACEM"] + south[,"TOTRACEF"]
white.male.south <- sum(south[,"WHITEM"])
white.female.south <- sum(south[,"WHITEF"])
black.male.south <- sum(south[,"BLACKM"])
black.female.south <- sum(south[,"BLACKF"])
hisp.male.south <- sum(south[,"HISPM"])
hisp.female.south <- sum(south[,"HISPF"])
asian.male.south <- sum(south[,"ASIANM"])
asian.female.south <- sum(south[,"ASIANF"])
nhpi.male.south <- sum(south[,"NHPIM"])
nhpi.female.south <- sum(south[,"NHPIF"])
tworace.male.south <- sum(south[,"TWORACEM"])
tworace.female.south <- sum(south[,"TWORACEF"])
other.male.south <- sum(south[,"ADDRACEM"])
other.female.south <- sum(south[,"ADDRACEF"])
unknown.male.south <- sum(south[,"UNKRACEM"])
unknown.female.south <- sum(south[,"UNKRACEF"])

slices.male.south <- c(white.male.south, black.male.south, hisp.male.south, asian.male.south, nhpi.male.south, tworace.male.south, other.male.south ,unknown.male.south)
lbls <- c("White", "Black", "Hispanic", "Asian", "Native Hawaiian and Pacific Islander", "Two Races", "Other")
pct <- round(slices.male.south/sum(slices.male.south)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices.male.south,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Races under jurisdiction in the south ")



total.race.midwest <- midwest[,"TOTRACEM"] + midwest[,"TOTRACEF"]
white.male.midwest <- sum(midwest[,"WHITEM"])
white.female.midwest <- sum(midwest[,"WHITEF"])
black.male.midwest <- sum(midwest[,"BLACKM"])
black.female.midwest <- sum(midwest[,"BLACKF"])
hisp.male.midwest <- sum(midwest[,"HISPM"])
hisp.female.midwest <- sum(midwest[,"HISPF"])
asian.male.midwest <- sum(midwest[,"ASIANM"])
asian.female.midwest <- sum(midwest[,"ASIANF"])
nhpi.male.midwest <- sum(midwest[,"NHPIM"])
nhpi.female.midwest <- sum(midwest[,"NHPIF"])
tworace.male.midwest <- sum(midwest[,"TWORACEM"])
tworace.female.midwest <- sum(midwest[,"TWORACEF"])
other.male.midwest <- sum(midwest[,"ADDRACEM"])
other.female.midwest <- sum(midwest[,"ADDRACEF"])
unknown.male.midwest <- sum(midwest[,"UNKRACEM"])
unknown.female.midwest <- sum(midwest[,"UNKRACEF"])

slices.male.midwest <- c(white.male.midwest, black.male.midwest, hisp.male.midwest, asian.male.midwest, nhpi.male.midwest, tworace.male.midwest, other.male.midwest ,unknown.male.midwest)
lbls <- c("White", "Black", "Hispanic", "Asian", "Native Hawaiian and Pacific Islander", "Two Races", "Other")
pct <- round(slices.male.midwest/sum(slices.male.midwest)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices.male.midwest,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Races under jurisdiction in the midwest ")

total.race.west <- west[,"TOTRACEM"] + west[,"TOTRACEF"]
white.male.west <- sum(west[,"WHITEM"])
white.female.west <- sum(west[,"WHITEF"])
black.male.west <- sum(west[,"BLACKM"])
black.female.west <- sum(west[,"BLACKF"])
hisp.male.west <- sum(west[,"HISPM"])
hisp.female.west <- sum(west[,"HISPF"])
asian.male.west <- sum(west[,"ASIANM"])
asian.female.west <- sum(west[,"ASIANF"])
nhpi.male.west <- sum(west[,"NHPIM"])
nhpi.female.west <- sum(west[,"NHPIF"])
tworace.male.west <- sum(west[,"TWORACEM"])
tworace.female.west <- sum(west[,"TWORACEF"])
other.male.west <- sum(west[,"ADDRACEM"])
other.female.west <- sum(west[,"ADDRACEF"])
unknown.male.west <- sum(west[,"UNKRACEM"])
unknown.female.west <- sum(west[,"UNKRACEF"])

slices.male.west <- c(white.male.west, black.male.west, hisp.male.west, asian.male.west, nhpi.male.west, tworace.male.west, other.male.west ,unknown.male.west)
lbls <- c("White", "Black", "Hispanic", "Asian", "Native Hawaiian and Pacific Islander", "Two Races", "Other")
pct <- round(slices.male.west/sum(slices.male.west)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices.male.west,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Races under jurisdiction in the west ")

#breakdown of races are similar throughout all regions


rep <- subset(df, party == 5)
lean_rep <- subset(df, party == 4)
comp <- subset(df, party == 3)
lean_dem <- subset(df, party == 2)
dem <- subset(df, party == 1)


total.race.rep <- rep[,"TOTRACEM"] + rep[,"TOTRACEF"]
white.male.rep <- sum(rep[,"WHITEM"])
white.female.rep <- sum(rep[,"WHITEF"])
black.male.rep <- sum(rep[,"BLACKM"])
black.female.rep <- sum(rep[,"BLACKF"])
hisp.male.rep <- sum(rep[,"HISPM"])
hisp.female.rep <- sum(rep[,"HISPF"])
asian.male.rep <- sum(rep[,"ASIANM"])
asian.female.rep <- sum(rep[,"ASIANF"])
nhpi.male.rep <- sum(rep[,"NHPIM"])
nhpi.female.rep <- sum(rep[,"NHPIF"])
tworace.male.rep <- sum(rep[,"TWORACEM"])
tworace.female.rep <- sum(rep[,"TWORACEF"])
other.male.rep <- sum(rep[,"ADDRACEM"])
other.female.rep <- sum(rep[,"ADDRACEF"])
unknown.male.rep <- sum(rep[,"UNKRACEM"])
unknown.female.rep <- sum(rep[,"UNKRACEF"])

slices.male.rep <- c(white.male.rep, black.male.rep, hisp.male.rep, asian.male.rep, nhpi.male.rep, tworace.male.rep, other.male.rep ,unknown.male.rep)
lbls <- c("White", "Black", "Hispanic", "Asian", "Native Hawaiian and Pacific Islander", "Two Races", "Other")
pct <- round(slices.male.rep/sum(slices.male.rep)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices.male.rep,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Races under jurisdiction in the rep ")


total.race.lean_rep <- lean_rep[,"TOTRACEM"] + lean_rep[,"TOTRACEF"]
white.male.lean_rep <- sum(lean_rep[,"WHITEM"])
white.female.lean_rep <- sum(lean_rep[,"WHITEF"])
black.male.lean_rep <- sum(lean_rep[,"BLACKM"])
black.female.lean_rep <- sum(lean_rep[,"BLACKF"])
hisp.male.lean_rep <- sum(lean_rep[,"HISPM"])
hisp.female.lean_rep <- sum(lean_rep[,"HISPF"])
asian.male.lean_rep <- sum(lean_rep[,"ASIANM"])
asian.female.lean_rep <- sum(lean_rep[,"ASIANF"])
nhpi.male.lean_rep <- sum(lean_rep[,"NHPIM"])
nhpi.female.lean_rep <- sum(lean_rep[,"NHPIF"])
tworace.male.lean_rep <- sum(lean_rep[,"TWORACEM"])
tworace.female.lean_rep <- sum(lean_rep[,"TWORACEF"])
other.male.lean_rep <- sum(lean_rep[,"ADDRACEM"])
other.female.lean_rep <- sum(lean_rep[,"ADDRACEF"])
unknown.male.lean_rep <- sum(lean_rep[,"UNKRACEM"])
unknown.female.lean_rep <- sum(lean_rep[,"UNKRACEF"])

slices.male.lean_rep <- c(white.male.lean_rep, black.male.lean_rep, hisp.male.lean_rep, asian.male.lean_rep, nhpi.male.lean_rep, tworace.male.lean_rep, other.male.lean_rep ,unknown.male.lean_rep)
lbls <- c("White", "Black", "Hispanic", "Asian", "Native Hawaiian and Pacific Islander", "Two Races", "Other")
pct <- round(slices.male.lean_rep/sum(slices.male.lean_rep)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices.male.lean_rep,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Races under jurisdiction in the lean_rep ")


total.race.comp <- comp[,"TOTRACEM"] + comp[,"TOTRACEF"]
white.male.comp <- sum(comp[,"WHITEM"])
white.female.comp <- sum(comp[,"WHITEF"])
black.male.comp <- sum(comp[,"BLACKM"])
black.female.comp <- sum(comp[,"BLACKF"])
hisp.male.comp <- sum(comp[,"HISPM"])
hisp.female.comp <- sum(comp[,"HISPF"])
asian.male.comp <- sum(comp[,"ASIANM"])
asian.female.comp <- sum(comp[,"ASIANF"])
nhpi.male.comp <- sum(comp[,"NHPIM"])
nhpi.female.comp <- sum(comp[,"NHPIF"])
tworace.male.comp <- sum(comp[,"TWORACEM"])
tworace.female.comp <- sum(comp[,"TWORACEF"])
other.male.comp <- sum(comp[,"ADDRACEM"])
other.female.comp <- sum(comp[,"ADDRACEF"])
unknown.male.comp <- sum(comp[,"UNKRACEM"])
unknown.female.comp <- sum(comp[,"UNKRACEF"])

slices.male.comp <- c(white.male.comp, black.male.comp, hisp.male.comp, asian.male.comp, nhpi.male.comp, tworace.male.comp, other.male.comp ,unknown.male.comp)
lbls <- c("White", "Black", "Hispanic", "Asian", "Native Hawaiian and Pacific Islander", "Two Races", "Other")
pct <- round(slices.male.comp/sum(slices.male.comp)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices.male.comp,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Races under jurisdiction in the comp ")



total.race.lean_dem <- lean_dem[,"TOTRACEM"] + lean_dem[,"TOTRACEF"]
white.male.lean_dem <- sum(lean_dem[,"WHITEM"])
white.female.lean_dem <- sum(lean_dem[,"WHITEF"])
black.male.lean_dem <- sum(lean_dem[,"BLACKM"])
black.female.lean_dem <- sum(lean_dem[,"BLACKF"])
hisp.male.lean_dem <- sum(lean_dem[,"HISPM"])
hisp.female.lean_dem <- sum(lean_dem[,"HISPF"])
asian.male.lean_dem <- sum(lean_dem[,"ASIANM"])
asian.female.lean_dem <- sum(lean_dem[,"ASIANF"])
nhpi.male.lean_dem <- sum(lean_dem[,"NHPIM"])
nhpi.female.lean_dem <- sum(lean_dem[,"NHPIF"])
tworace.male.lean_dem <- sum(lean_dem[,"TWORACEM"])
tworace.female.lean_dem <- sum(lean_dem[,"TWORACEF"])
other.male.lean_dem <- sum(lean_dem[,"ADDRACEM"])
other.female.lean_dem <- sum(lean_dem[,"ADDRACEF"])
unknown.male.lean_dem <- sum(lean_dem[,"UNKRACEM"])
unknown.female.lean_dem <- sum(lean_dem[,"UNKRACEF"])

slices.male.lean_dem <- c(white.male.lean_dem, black.male.lean_dem, hisp.male.lean_dem, asian.male.lean_dem, nhpi.male.lean_dem, tworace.male.lean_dem, other.male.lean_dem ,unknown.male.lean_dem)
lbls <- c("White", "Black", "Hispanic", "Asian", "Native Hawaiian and Pacific Islander", "Two Races", "Other")
pct <- round(slices.male.lean_dem/sum(slices.male.lean_dem)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices.male.lean_dem,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Races under jurisdiction in the lean_dem ")


total.race.dem <- dem[,"TOTRACEM"] + dem[,"TOTRACEF"]
white.male.dem <- sum(dem[,"WHITEM"])
white.female.dem <- sum(dem[,"WHITEF"])
black.male.dem <- sum(dem[,"BLACKM"])
black.female.dem <- sum(dem[,"BLACKF"])
hisp.male.dem <- sum(dem[,"HISPM"])
hisp.female.dem <- sum(dem[,"HISPF"])
asian.male.dem <- sum(dem[,"ASIANM"])
asian.female.dem <- sum(dem[,"ASIANF"])
nhpi.male.dem <- sum(dem[,"NHPIM"])
nhpi.female.dem <- sum(dem[,"NHPIF"])
tworace.male.dem <- sum(dem[,"TWORACEM"])
tworace.female.dem <- sum(dem[,"TWORACEF"])
other.male.dem <- sum(dem[,"ADDRACEM"])
other.female.dem <- sum(dem[,"ADDRACEF"])
unknown.male.dem <- sum(dem[,"UNKRACEM"])
unknown.female.dem <- sum(dem[,"UNKRACEF"])

slices.male.dem <- c(white.male.dem, black.male.dem, hisp.male.dem, asian.male.dem, nhpi.male.dem, tworace.male.dem, other.male.dem ,unknown.male.dem)
lbls <- c("White", "Black", "Hispanic", "Asian", "Native Hawaiian and Pacific Islander", "Two Races", "Other")
pct <- round(slices.male.dem/sum(slices.male.dem)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices.male.dem,labels = lbls, col=rainbow(length(lbls)),
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
