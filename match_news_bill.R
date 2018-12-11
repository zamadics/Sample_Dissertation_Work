

set.seed(1605)
library(stringr)
library(RTextTools)
library(tm)
library(plyr)
library(xts)
library(tsDyn)
library(tseries)
library(ggplot2)
library(ggthemes)
library(MASS)


setwd("C:/Users/diein/Dropbox/Bethany/Data/NYT")

dic <-read.csv("pap_dictionary.csv", stringsAsFactors=FALSE)

news1 <- read.csv("finished_100k.csv", stringsAsFactors=FALSE)
news2 <- read.csv("finished_200k.csv", stringsAsFactors=FALSE)
news3 <- read.csv("finished_300k.csv", stringsAsFactors=FALSE)
news4 <- read.csv("finished_400k.csv", stringsAsFactors=FALSE)
news5 <- read.csv("finished_500k.csv", stringsAsFactors=FALSE)
news6 <- read.csv("finished_600k.csv", stringsAsFactors=FALSE)
news7 <- read.csv("finished_603k.csv", stringsAsFactors=FALSE)

news <- rbind(news1, news2, news3, news4, news5, news6, news7)
news$counter <- 1
news$year <- as.numeric(news$year)
news$month <- as.numeric(news$month)
news$yrmo <- news$year*100 + news$month
news$quarter[news$month > 8] <- 4
news$quarter[news$month > 6 & news$month < 9] <- 3
news$quarter[news$month > 3 & news$month < 6] <- 2
news$quarter[news$month < 4] <- 1
news$yrq <- news$year*10 + news$quarter

news <- news[ which(news$year < 2017 & news$year > 2000), ]

fips <- c(4, 6, 8, 9, 12, 13, 16, 17, 18, 19, 20, 21, 22, 24, 25, 26, 27, 28, 29, 30, 31, 34, 35, 36, 37,
          38, 39, 40, 41, 42, 45, 46, 47, 48, 49, 50, 51, 53, 55, 56)
# removed 33(NH), 

yrmo <- c(200601:200612, 200701:200712, 200801:200812, 200901:200912, 201001:201012, 201101:201112, 201201:201212,
          201301:201312, 201401:201412, 201501:201512, 201601:201612)
yrmov <- c(200601:200612, 200701:200712, 200801:200812, 200901:200912, 201001:201012, 201101:201112, 201201:201212,
           201301:201312, 201401:201412, 201501:201512, 201601:201612)
yrmo <- as.data.frame(yrmo)

news$lower <- str_to_lower(news$content)
news$lower <- removeWords(news$lower, stopwords("english"))
news$lower <- removePunctuation(news$lower)
news$lower <- removeNumbers(news$lower)


setwd("C:/Users/diein/Dropbox/Bethany/Data/LexisNexis/bill")

# load all the state data

co <- read.csv("colorado.csv", stringsAsFactors=FALSE)
fl <- read.csv("florida.csv", stringsAsFactors=FALSE)
ga <- read.csv("georgia.csv", stringsAsFactors=FALSE)
mo <- read.csv("missouri.csv", stringsAsFactors=FALSE)
mn <- read.csv("minnesota.csv", stringsAsFactors=FALSE)
ny <- read.csv("new york.csv", stringsAsFactors=FALSE)
nd <- read.csv("north dakota.csv", stringsAsFactors=FALSE)
oh <- read.csv("ohio.csv", stringsAsFactors=FALSE)
ok <- read.csv("oklahoma.csv", stringsAsFactors=FALSE)
pa <- read.csv("pennsylvania.csv", stringsAsFactors=FALSE)
tx <- read.csv("texas.csv", stringsAsFactors=FALSE)
ut <- read.csv("utah.csv", stringsAsFactors=FALSE)
va <- read.csv("virginia.csv", stringsAsFactors=FALSE)
wa <- read.csv("washington.csv", stringsAsFactors=FALSE)

df <- rbind(co,fl,ga,mo,mn,ny,nd,oh,ok,pa,tx,ut,va,wa)

df$year <- as.numeric(substr(df$date, 1, 4))
df$month <- as.numeric(substr(df$date, 6, 7))

df$lower <- str_to_lower(df$title)
df$lower <- removeWords(df$lower, stopwords("english"))
df$lower <- removePunctuation(df$lower)
df$lower <- removeNumbers(df$lower)

find.stuff <- function(ss, kw){
  ss <- strsplit(ss, split = " ")
  #try(ss <- wordStem(ss[[1]]), silent = T)
  #ss <- trimws(ss)
  return(sum(pmatch(kw, ss[[1]]), na.rm = T) > 0)
}

find.stuff2 <- function(ss, kw){
  ss <- strsplit(ss, split = " ")
  try(ss <- wordStem(ss[[1]]), silent = T)
  ss <- trimws(ss)
  gsub("thing", "", ss)
  temp <- pmatch(kw, ss)
  temp <- sum(temp > 0, na.rm = T)
  return(sum(temp, na.rm = T) > 1)
}



df <- df[ which(df$year > 2009 & df$year < 2017),]
# list of keyword stuff #
gun.kw <- c("gun", "firearm", "shoot", "shooting", "rifle", "conceal", "handgun")
mj.kw <- c("marijuana", "cannabis", "hemp", "thc")
frack.kw <- c("frack", "fracking")
opiod.kw <- c("opiod", "inject", "needle", "herion")
med.kw <- c("medicaid", "medicare")
dis.kw <- c("disease")

legislative.kw <- c("state", "gov", "legislature", "governor")

# subset data #
ln <- news[ which(news$source == "Lexis Nexis" | news$source == "LexisNexis"),]
ln <- ln[ which(ln$year > 2008 & ln$year < 2017),]


df$gun <- sapply(df$lower, FUN = find.stuff, kw = gun.kw)
ln$gun <- sapply(ln$lower, FUN = find.stuff, kw = gun.kw)


#tri.kw <- c("police")
#tri2.kw <- c("misconduct", "camera", "cameras", "officerinvolved", "officer involved")
tri.kw <- c("storm", "flood", "hurricane", "floods", "storms")
ln$tri <- sapply(ln$lower, FUN = find.stuff, kw = tri.kw)
#ln$tri2 <- sapply(ln$lower, FUN = find.stuff, kw = tri2.kw)
#ln$dis <- 0
#ln$dis[ln$tri == TRUE & ln$tri2 == TRUE] <- 1
#table(ln$dis)
#table(ln$tri)
#tri <- ln[ which(ln$tri == TRUE), ]
ln$popo <- ln$tri


#tri.kw <- c("police")
#tri2.kw <- c("misconduct", "camera", "cameras", "officerinvolved", "officer involved")
tri.kw <- c("storm", "flood", "hurricane", "floods", "storms")
df$tri <- sapply(df$lower, FUN = find.stuff, kw = tri.kw)
#df$tri2 <- sapply(df$lower, FUN = find.stuff, kw = tri2.kw)
#df$dis <- 0
#df$dis[df$tri == TRUE & df$tri2 == TRUE] <- 1
#table(df$dis)
#tri <- df[ which(df$dis == TRUE), ]
#df$popo <- df$dis
df$popo <- df$tri

ln$sl <- sapply(ln$lower, FUN = find.stuff2, kw = legislative.kw)


# look at list of bill distributions by state

sub <- df[ which(df$state == "CO"), ]
barplot(table(sub$month))
table(sub$month)
barplot(table(sub$year))
table(sub$year)

# create event data

ln$gun.leg <- 0
ln$gun.leg[ln$gun == TRUE & ln$sl == TRUE] <- 1
ln$gun.event <- 0
ln$gun.event[ln$gun == TRUE & ln$sl == FALSE] <- 1


ln$popo.leg <- 0
ln$popo.leg[ln$popo == TRUE & ln$sl == TRUE] <- 1
ln$popo.event <- 0
ln$popo.event[ln$popo == TRUE & ln$sl == FALSE] <- 1


# create time series plot (shaded for in session out session)

library(ggplot2)
library(lubridate)
ln <- ln[ which(ln$mo < 13), ]

df$yrmo <- df$year*100 + df$month



# try this for colorado first

ln.sub <- ln[ which(ln$fips == 8 & ln$year > 2009), ]

ag <- aggregate(ln.sub$gun.leg, by=list(Category=ln.sub$yrmo), FUN=sum)
ag2 <- aggregate(ln.sub$gun.event, by=list(Category=ln.sub$yrmo), FUN=sum)

st <- as.Date("2010-1-01")
en <- as.Date("2016-12-01")
dates <- seq(st, en, by = "month")
Type <- c(rep("Legislative", length(dates)), rep("Event", length(dates)))
value <- c(ag$x,ag2$x)

gdat <- data.frame(dates, Type, value)


ggplot(gdat, aes(x = dates, y = value)) + 
  geom_area(aes(color = Type, fill = Type), alpha = 0.3, position = position_dodge(0.8)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  xlab("Month") + ylab("Frontpage Stories")


dateRanges <- data.frame(
  start = seq(as.POSIXct("2000-01-01", format = "%Y-%m-%d"), as.POSIXct("2020-01-01", format = "%Y-%m-%d"), "1 year"),
  end = seq(as.POSIXct("2000-05-01", format = "%Y-%m-%d"), as.POSIXct("2020-05-01", format = "%Y-%m-%d"), "1 year")
)

gdat$xdates <- seq(as.POSIXct("2010-01-01", format = "%Y-%m-%d"), as.POSIXct("2016-12-31", format = "%Y-%m-%d"), "1 month")
gdat <- gdat[85:nrow(gdat),]

ggplot(gdat) + 
  geom_rect(data = dateRanges, aes(xmin = start , xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes=FALSE, alpha = 0.4, fill = c("lightblue"))+
  geom_line(aes(x= xdates, y = value), size = .69) + 
  coord_cartesian(xlim = range(gdat$xdates)) + xlab("Month") + ylab("Frontpage Stories") +
  ggtitle("Event Driven Firearm Stories Over Time (Colorado)")


### create data ###

ln$state <- NA
ln$state[ln$fips == 8] <- "CO"
ln$state[ln$fips == 12] <- "FL"
ln$state[ln$fips == 13] <- "GA"
ln$state[ln$fips == 27] <- "MN"
ln$state[ln$fips == 29] <- "MO"
ln$state[ln$fips == 36] <- "NY"
ln$state[ln$fips == 40] <- "OK"
ln$state[ln$fips == 42] <- "PA"
ln$state[ln$fips == 48] <- "TX"
ln$state[ln$fips == 49] <- "UT"
ln$state[ln$fips == 51] <- "VA"
ln$state[ln$fips == 53] <- "WA"

# create skeleton

states.l <- c("CO", "FL", "GA", "MN", "MO", "NY", "OK", "PA", "TX", "UT", "VA", "WA")
years <- 2010:2016


states <- rep(states.l, each = length(years))
years <- rep(years, length(states.l))

dat <- data.frame(states,years)

# get bill data in dat

for(i in 1:nrow(dat)){
  sub <- df[ which(df$year == dat$years[i] & df$state == dat$states[i]), ]
  dat$gun.bill[i] <- sum(sub$gun) / nrow(sub)
  dat$popo.bill[i] <- sum(sub$popo) / nrow(sub)
  dat$gun.bill.c[i] <- sum(sub$gun, na.rm = T)
  dat$popo.bill.c[i] <- sum(sub$popo, na.rm = T)
  print(i)
}

#dat.os.na <- is.na(dat$gun.os)
#dat$gun.os[dat.os.na == TRUE] <- 0
#dat$popo.os[dat.os.na == TRUE] <- 0



# get party bill data in dat

for(i in 1:nrow(dat)){
  sub <- df[ which(df$year == dat$years[i] & df$state == dat$states[i]), ]
  subr <- df[ which(df$year == dat$years[i] & df$state == dat$states[i] & df$party == "R"), ]
  subd <- df[ which(df$year == dat$years[i] & df$state == dat$states[i] & df$party == "D"), ]
  dat$gunr.bill[i] <- sum(subr$gun) / nrow(sub)
  dat$popor.bill[i] <- sum(subr$popo) / nrow(sub)
  dat$gund.bill[i] <- sum(subd$gun) / nrow(sub)
  dat$popod.bill[i] <- sum(subd$popo) / nrow(sub)
  print(i)
}


# create in session variable #

ln$odd <- 0
ln$odd[ln$year == 2011] <- 1
ln$odd[ln$year == 2013] <- 1
ln$odd[ln$year == 2015] <- 1

ln$session <- 0

ln$session[ln$state == "CO" & ln$month < 4] <- 1
ln$session[ln$state == "FL" & ln$month == 4] <- 1
ln$session[ln$state == "FL" & ln$month == 3] <- 1
ln$session[ln$state == "FL" & ln$year == 2012 & ln$month < 3] <- 1
ln$session[ln$state == "FL" & ln$year == 2016 & ln$month < 3] <- 1
ln$session[ln$state == "GA" & ln$month < 4] <- 1
ln$session[ln$state == "MO" & ln$month < 5] <- 1
ln$session[ln$state == "MN" & ln$year == 2010 & ln$month < 5 & ln$month > 3] <- 1
ln$session[ln$state == "MN" & ln$year == 2011 & ln$month < 5] <- 1
ln$session[ln$state == "MN" & ln$year == 2012 & ln$month < 5 & ln$month > 2] <- 1
ln$session[ln$state == "MN" & ln$year == 2013 & ln$month < 6] <- 1
ln$session[ln$state == "MN" & ln$year == 2014 & ln$month < 6 & ln$month > 2] <- 1
ln$session[ln$state == "MN" & ln$year == 2015 & ln$month < 6] <- 1
ln$session[ln$state == "MN" & ln$year == 2016 & ln$month < 6 & ln$month > 3] <- 1
ln$session[ln$state == "NY" & ln$month < 7] <- 1
ln$session[ln$state == "OK" & ln$month < 6 & ln$month > 1] <- 1
ln$session[ln$state == "PA" & ln$month < 12] <- 1
ln$session[ln$state == "TX" & ln$month < 6 & ln$odd == 1] <- 1
ln$session[ln$state == "UT" & ln$month == 2] <- 1
ln$session[ln$state == "VA" & ln$month < 3] <- 1
ln$session[ln$state == "WA" & ln$month < 3] <- 1


# create main ivs #


for(i in 1:nrow(dat)){
  dyear <- dat$years[i]
  dstate <- dat$states[i]
  
  subt <- ln[ which(ln$state == dstate & ln$year == dyear), ]
  subt1 <- ln[ which(ln$state == dstate & ln$year == dyear - 1), ]
  
  sub.is <- subt[ which(subt$session == 1), ]
  
  dat$gun.is[i] <- sum(sub.is$gun.event, na.rm = T) / nrow(sub.is)
  dat$popo.is[i] <- sum(sub.is$popo.event, na.rm = T) / nrow(sub.is)
  
  sub.os <- subt1[ which(subt$session == 0), ]
  
  dat$gun.os[i] <- sum(sub.os$gun.event, na.rm = T) / nrow(sub.os)
  dat$popo.os[i] <- sum(sub.os$popo.event, na.rm = T) / nrow(sub.os)

  
  print(i)
}

# create lagged DV and lagged news variables
dat$gun.bill.c.l <- NA
dat$popo.bill.c.l <- NA

for(i in 1:nrow(dat)){
  sub <- dat[ which(dat$states == dat$states[i] & dat$years == dat$years[i]-1), ]
  
  if (nrow(sub) == 0) {
    dat$gun.bill.l[i] <- NA
    dat$popo.bill.l[i] <- NA
    dat$gun.is.l[i] <- NA
    dat$popo.is.l[i] <- NA
    dat$gun.os.l[i] <- NA
    dat$popo.os.l[i] <- NA

  } else {
    dat$gun.bill.l[i] <- sub$gun.bill
    dat$popo.bill.l[i] <- sub$popo.bill
    dat$gun.bill.c.l[i] <- sub$gun.bill.c
    dat$popo.bill.c.l[i] <- sub$popo.bill.c
    dat$gun.is.l[i] <- sub$gun.is
    dat$popo.is.l[i] <- sub$popo.is
    dat$gun.os.l[i] <- sub$gun.os
    dat$popo.os.l[i] <- sub$popo.os

  }
  print(i)
}

# fold in state ideology and professionalism
library(DescTools)

squire <- read.csv("squire.csv", stringsAsFactors=FALSE)
stateideology <- read.csv("stateideology.csv", stringsAsFactors=FALSE)

state.dat <- data.frame(state.abb, state.name)
colnames(state.dat) <- c("ab", "name")

for(i in 1:nrow(stateideology)){
  sub <- state.dat[ which(state.dat$name == stateideology$state[i]), ]
  try(stateideology$stateab[i] <- as.character(sub$ab), silent = T)
  print(i)
}



dat <- as.data.frame(dat, stringsAsFactors=FALSE)

for(i in 1:nrow(dat)){
  year <- dat$years[i]
  state <- dat$states[i]
  
  squire.sub <- squire[ which(squire$stateabv == state), ]
  squire.years <- unique(squire.sub$year)

  
  s.year <- Closest(squire.years,year, which = F, na.rm = T)
  s.year <- s.year[1]

  
  subs <- squire[ which(squire$stateabv == state & squire$year == s.year), ]
  dat$squire[i] <- subs$mds1
  
  
  sisub <- stateideology[ which(stateideology$stateab == state), ]
  si.year <- unique(sisub$year)
  si.year <- Closest(si.year,year, which = F, na.rm = T)
  
  subsi <- stateideology[ which(stateideology$stateab == as.character(state) & stateideology$year == si.year), ]
  
  dat$ideo.cit[i] <- subsi$citizen
  dat$ideo.gov[i] <- subsi$government
  
  print(i)
}

# fold in government information 

sls <- read.csv("sl_2010_2016.csv", stringsAsFactors=FALSE)

dat$sl.united <- NA

for(i in 1:nrow(dat)){
  year <- dat$years[i]
  state <- dat$states[i]
  
  sub <- sls[ which(sls$state == state & sls$year == year), ]
  
  dat$sl.lower[i] <- sub$sl_lower
  dat$sl.upper[i] <- sub$sl_upper
  dat$sl[i] <- sub$sl
  dat$gov[i] <- sub$gov
  
  if(dat$sl == 0 | dat$sl == 2){
    dat$sl.united[i] <- 1
  }
  
  print(i)
}


# determine if it became law
df.law <- df[which(df$status == 4), ]

for(i in 1:nrow(dat)){
  sub <- df.law[ which(df.law$year == dat$years[i] & df.law$state == dat$states[i]), ]
  dat$gun.law[i] <- sum(sub$gun) 
  dat$popo.law[i] <- sum(sub$popo) 
  print(i)
}

# create lagged law DV

for(i in 1:nrow(dat)){
  sub <- dat[ which(dat$states == dat$states[i] & dat$years == dat$years[i]-1), ]
  
  if (nrow(sub) == 0) {
    dat$gun.law.l[i] <- NA
    dat$popo.law.l[i] <- NA
    
  } else {
    dat$gun.law.l[i] <- sub$gun.law
    dat$popo.law.l[i] <- sub$popo.law
    
  }
  print(i)
}

# create national measure

years <- 2011:2016

dat$gun.nat <- NA
dat$popo.nat <- NA

for(j in 1:length(years)){
  
  year <- years[j]-1
  sub <- ln[which(ln$year == year), ]
  states <- unique(sub$state)
  
  gunres <- NA
  weares <- NA
  for(i in 1:length(states)){
    subsub <- sub[which(sub$state == states[i]), ]
    gunres[i] <- sum(subsub$gun.event, na.rm = T) / nrow(subsub)
    weares[i] <- sum(subsub$popo.event, na.rm = T) / nrow(subsub)
    
  }
  
  dat$gun.nat[dat$years == year+1] <- mean(gunres, na.rm = T)
  dat$popo.nat[dat$years == year+1] <- mean(weares, na.rm = T)
 print(j) 
}


# total session bills


for(i in 1:nrow(dat)){
  year <- dat$years[i]
  state <- dat$states[i]
  
  temp <- df[ which(df$year == year & df$state == state), ]
  dat$total.bill[i] <- nrow(temp)
  print(i)
}


### run models ###
dat2 <- dat[which(dat$states != "PA"),]

m <- lm(gun.bill ~ gun.bill.l + gun.is + gun.os + squire + ideo.cit + ideo.gov + as.factor(states), data = dat)
summary(m)

ml <- lm(gun.bill ~ gun.bill.l + gun.is + gun.os + gun.is.l + squire + ideo.cit + ideo.gov + as.factor(states), data = dat)
summary(ml)


m2 <- lm(popo.bill ~ popo.bill.l + popo.is + popo.os + squire + ideo.cit + ideo.gov + as.factor(states), data = dat)
summary(m2)

ml2 <- lm(popo.bill ~ popo.bill.l + popo.is + popo.os + popo.is.l + squire + ideo.cit + ideo.gov + as.factor(states), data = dat)
summary(ml2)

# count
m <- glm.nb(gun.bill.c ~ gun.bill.c.l + gun.is + gun.os + squire + ideo.cit + ideo.gov + total.bill + as.factor(states), data = dat2)
summary(m)

m2 <- glm.nb(popo.bill.c ~ popo.bill.c.l + popo.is + popo.os + squire + ideo.cit + ideo.gov + total.bill + as.factor(states), data = dat2)
summary(m2)

# law
m <- glm.nb(gun.law ~ gun.law.l + gun.is + gun.os + squire + ideo.cit + ideo.gov + as.factor(states), data = dat)
summary(m)

m2 <- glm.nb(popo.law ~ popo.law.l + popo.is + popo.os + squire + ideo.cit + ideo.gov + as.factor(states), data = dat)
summary(m2)





aggregate(dat$popo.bill, by=list(Category=dat$states), FUN=mean)
aggregate(dat$popo.bill, by=list(Category=dat$years), FUN=mean)

aggregate(dat$popo.is, by=list(Category=dat$states), FUN=mean, na.rm = T)
aggregate(dat$popo.is, by=list(Category=dat$years), FUN=mean, na.rm = T)
aggregate(dat$popo.os, by=list(Category=dat$states), FUN=mean, na.rm = T)
aggregate(dat$popo.os, by=list(Category=dat$years), FUN=mean, na.rm = T)










# run again to make nicer for stargazer

vars <- c("gun.bill", "gun.bill.l", "gun.is",  "gun.os", "gun.is.l", "gun.os.l", "squire", "ideo.cit", "ideo.gov", "states")
gun.dat <- dat[vars]
colnames(gun.dat) <- c("gun.bill", "bill.lag", "ins", "out", "ins.lag", "out.lag", "squire", "ideo.cit", "ideo.gov", "states")
ml <- lm(gun.bill ~ bill.lag + ins + out + ins.lag + out.lag + squire + ideo.cit + ideo.gov + as.factor(states), data = gun.dat)
summary(ml)

vars <- c("mj.bill", "mj.bill.l", "mj.is",  "mj.os", "mj.is.l", "mj.os.l", "squire", "ideo.cit", "ideo.gov", "states")
mj.dat <- dat[vars]
colnames(mj.dat) <- c("mj.bill", "bill.lag", "ins", "out", "ins.lag", "out.lag", "squire", "ideo.cit", "ideo.gov", "states")
ml2 <- lm(mj.bill ~ bill.lag + ins + out + ins.lag + out.lag + squire + ideo.cit + ideo.gov + as.factor(states), data = mj.dat)
summary(ml2)

vars <- c("med.bill", "med.bill.l", "med.is",  "med.os", "med.is.l", "med.os.l", "squire", "ideo.cit", "ideo.gov", "states")
med.dat <- dat[vars]
colnames(med.dat) <- c("med.bill", "bill.lag", "ins", "out", "ins.lag", "out.lag", "squire", "ideo.cit", "ideo.gov", "states")
ml3 <- lm(med.bill ~ bill.lag + ins + out + ins.lag + out.lag + squire + ideo.cit + ideo.gov + as.factor(states), data = med.dat)
summary(ml3)






########## corr plot ##########
library(ggcorrplot)

years <- 2010:2016
states <- c("CO", "FL", "GA", "MO", "MN", "NY", "OK", "TX", "UT", "VA", "WA")

res <- matrix(NA, nrow = length(years), ncol = length(states))

for(j in 1:length(states)){
  state <- states[j]
  
  for(i in 1:length(years)){
    year <- years[i]
    sub <- ln[ which(ln$year == year & ln$state == state),]
    res[i,j] <- sum(sub$gun.event, na.rm = T) / nrow(sub)
    
    
  }
    
  print(j)
}

res <- as.data.frame(res)
colnames(res) <- states



corr <- round(cor(res), 2)


ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram: Some Policy", 
           ggtheme=theme_bw)

