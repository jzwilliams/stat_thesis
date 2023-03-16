###############################################################################
###                     EVE Online killldump analysis                       ###
###############################################################################

# Created by Jacob Williams for STAT 4870
# Built under R, version 3.5.3
# Last modified: 4/28/2019

###############################################################################
###                             Libraries                                   ###
###############################################################################
library(data.table)             # Fast data read-in
library(tidyverse)              # Fast data cleanup
library(car)                    # Model comparisons

###############################################################################
###                           ggplot2 parameters                            ###
###############################################################################

theme_set(theme_bw())     # Black and white
theme_update(text=element_text(size=20),
             axis.text.x=element_text(size=16),
             axis.text.y=element_text(size=16))


###############################################################################
###                             Read in data                                ###
###############################################################################

# Function by leerssej on StackOverflow:
#   https://stackoverflow.com/questions/11433432/
dat <- list.files(pattern = "*.csv") %>% 
  map_df(~fread(., stringsAsFactors = TRUE))


###############################################################################
###                             Clean data                                  ###
###############################################################################

# Set NA bounties to zero
dat$bountyClaimed[is.na(dat$bountyClaimed)] <- 0
sum(dat$bountyClaimed != 0)  # 54,265 nonzero bounties

# We remove fine-grained factors:
#   victimCorp
#   finalCorp
#   solarSystemName
#   destroyedShipType
dat$victimCorp <- NULL
dat$finalCorp <- NULL
dat$destroyedShipType <- NULL
dat$solarSystemName <- NULL

# Set factor variables to be factors (region is already)
dat$destroyedShipGroup <- as.factor(dat$destroyedShipGroup) # 89 levels
dat$victimAlliance <- as.factor(dat$victimAlliance)         # 1426 levels
dat$finalAlliance <- as.factor(dat$finalAlliance)           # 1021 levels

# Define response variable: recoverable ISK
dat$iskRecoverable <- dat$iskLost - dat$iskDestroyed

# Rename factors for convenience
colnames(dat)[colnames(dat)=="destroyedShipGroup"] <- "shipGroup"
colnames(dat)[colnames(dat)=="regionName"] <- "region"
colnames(dat)[colnames(dat)=="bountyClaimed"] <- "bounty"

# Fix the datetime variable, time only at the hour level
dat$date <- as.Date(dat$killTime)
dat$time <- format(as.POSIXct(dat$killTime),"%H:%M:%S")
dat$hour <- format(strptime(dat$time,"%H:%M:%S"),"%H") # get just hours
dat$hour <- as.factor(dat$hour)
dat$killTime <- NULL
dat$time <- NULL


###############################################################################
###                          Collapse data                                  ###
###############################################################################

# With well over 800,000 data points, collapsing is a good plan

# Reasonable factors to collapse: victimAlliance, shipGroup, region?
dat$regClaVal <- paste(dat$region,dat$shipGroup,dat$victimAlliance)
dat$regClaVal <- as.factor(dat$regClaVal) # 85,280 levels
dat$regClaVal <- NULL

# Could we add hour?
dat$regClaValHr <- paste(dat$region,dat$shipGroup,dat$victimAlliance,
                         dat$hour)
dat$regClaValHr <- as.factor(dat$regClaValHr) # 238,881 levels
dat$regClaValHr <- NULL

# There are too many levels with hour. Probably one of the other groups
#   would need to be cut out.

# Collapse the data by victimAlliance, shipGroup, region: 85,280 observations
byAGR <- dat %>% group_by(victimAlliance,shipGroup,region) %>%
  summarize(iskLost=mean(iskLost),iskDestroyed=mean(iskDestroyed),
            bounty=mean(bounty),iskRecoverable=mean(iskRecoverable))

# We wish to sort region and shipGroup by mean iskRecoverable


###############################################################################
###                       Initial modeling                                  ###
###############################################################################

  # Order regions from lowest to largest iskRecoverable
byAGR$region <- reorder(byAGR$region,byAGR$iskRecoverable)
lmReg <- lm(iskRecoverable~region,data=byAGR)
summary(lmReg)
# Not very significant results: only one p-value < 0.01, and it is 0.0077
  # E-R00026 (wormhole class 5) is the one W-space region that matters
  # Apparently lucrative regions: 
  #   Oasa, Derelik, Branch, Molden Heath, E-R00026
  #   Cobalt Edge, The Forge, ADR01, Tash-Murkon, Perrigen Falls,
  #   Wicked Creek, ADR03 (borderline), The Spire
# The R^2 value is only 0.000480, indicating very little variability in
#   iskRecoverable can be explained by the region.


# By shipGroup alone
byAGR$shipGroup <- reorder(byAGR$shipGroup,byAGR$iskRecoverable)
lmGro <- lm(iskRecoverable~shipGroup,data=byAGR) # cheapest: mining drone
summary(lmGro) # There are substantial differences here

# By bounty alone
lmBou <- lm(iskRecoverable~bounty,data=byAGR)
summary(lmBou)
  # There is a relationship, but is it this simple?

# We have reason to suppose that bounty is exponentially distributed:
hist(byAGR$bounty) # most bounties are zero
hist(byAGR$bounty[byAGR$bounty > 0])
hist(byAGR$bounty[byAGR$bounty > 1000000])
hist(byAGR$bounty[byAGR$bounty > 100000000]) # memorylessness property
hist(log10(byAGR$bounty[byAGR$bounty > 0])) # looks virtually normal

# Is there a logarithmic relationship between the log of bounty
#   and iskRecoverable?
byAGR$hasBounty <- 0
byAGR$hasBounty[byAGR$bounty > 0] <- 1
byAGR$hasBounty <- as.factor(byAGR$hasBounty)

byAGR$logBounty <- 0
byAGR$logBounty[byAGR$bounty > 0] <- log(byAGR$bounty[byAGR$bounty > 0])
lmLBo <- lm(iskRecoverable~hasBounty+logBounty,data=byAGR)
summary(lmLBo)

byHBo <- lm(iskRecoverable~hasBounty+bounty,data=byAGR)
summary(byHBo)
  # The variability explained by linear bounty is greater than logarithmic
  # but still small (R^2 < 0.01 in all cases)

# Predict by hour (with the full dataset)
lmFullHou <- lm(iskRecoverable~hour,data=dat)
summary(lmFullHou)
  # Only 04, 10, 11 are different from hour00
  # Probably not a useful predictor
rm(lmFullHou)

  # Too many rows in victimAlliance for direct lm
byAGR$victimAlliance <- reorder(byAGR$victimAlliance,byAGR$iskRecoverable)
allyISK <- group_by(byAGR,victimAlliance) %>% 
  summarize(iskRecoverable=mean(iskRecoverable))
hist(allyISK$iskRecoverable)
hist(allyISK$iskRecoverable[allyISK$iskRecoverable > 1000000])
hist(allyISK$iskRecoverable[allyISK$iskRecoverable > 100000000])
hist(allyISK$iskRecoverable[allyISK$iskRecoverable > 1000000000])
  # This appears to be exponential, too.
hist(log10(allyISK$iskRecoverable[allyISK$iskRecoverable > 0]))
  # Slightly right-skewed, perhaps, but much more reasonable again

# The format of the victim's alliances suggests a grouping
#   according to order of magnitude of mean iskRecoverable.

###############################################################################
###                   Continued data manipulation                           ###
###############################################################################

# We wish to combine into one group any ship with "mining" in its group name;
# likewise those with "electronic".
byAGR$shipGroup <- as.character(byAGR$shipGroup)

byAGR$isMining <- 0
byAGR$isMining[byAGR$shipGroup %like% "Mining"] <- 1

byAGR$isElectronic <- 0
byAGR$isElectronic[byAGR$shipGroup %like% "Electronic"] <- 1

byAGR$isMobile <- 0
byAGR$isMobile[byAGR$shipGroup %like% "Mobile"] <- 1

byAGR$shipGroup[byAGR$isMining==1] <- "Mining ship"
byAGR$shipGroup[byAGR$isElectronic==1] <- "Electronic ship"
byAGR$shipGroup[byAGR$isMobile==1] <- "Mobile ship"

byAGR$shipGroup <- as.factor(byAGR$shipGroup)

byAGR$isMining <- NULL
byAGR$isElectronic <- NULL
byAGR$isMobile <- NULL

# So we have reduced shipGroup to 75 factors, from 89; none were significantly
# different in iskRecoverable from the cheapest one.
# We need to reorder the group again.
byAGR$shipGroup <- reorder(byAGR$shipGroup,byAGR$iskRecoverable)
lmGroRed <- lm(iskRecoverable~shipGroup,data=byAGR)
summary(lmGroRed)


###############################################################################
###                         Logarithmic response                            ###
###############################################################################

# We seek to investigate whether we should work on orders of magnitude of
# iskRecoverable instead of the raw values. 

hist(byAGR$iskRecoverable)
hist(byAGR$iskRecoverable[byAGR$iskRecoverable > 1000])
hist(byAGR$iskRecoverable[byAGR$iskRecoverable > 100000])
hist(byAGR$iskRecoverable[byAGR$iskRecoverable > 10000000])
hist(log10(byAGR$iskRecoverable))

# Yes, we should. However, we have 4,702 zero values in byAGR, as well as some
# values in (0,1]. So we will log(iskRecoverable+1) for our models.
dat$logRecoverable <- log10(dat$iskRecoverable + 1)
byAGR$logRecoverable <- log10(byAGR$iskRecoverable + 1)

logReg <- lm(logRecoverable~region,data=byAGR)
summary(logReg) # This still does not appear useful!

logGro <- lm(logRecoverable~shipGroup,data=byAGR)
summary(logGro) # Virtually everything is incredibly significant!
  # We will still get a smaller number of shipGroups and work from thence

logBou <- lm(logRecoverable~bounty,data=byAGR)
summary(logBou) # R^2 = 0.002216, still pretty small

logLBo <- lm(logRecoverable~logBounty,data=byAGR)
summary(logLBo) # R^2 = 0.005346, but highly significant coefficients

logHou <- lm(logRecoverable~hour,data=dat)
summary(logHou)


###############################################################################
###                         Combine ship groups                             ###
###############################################################################

# We reassign the shipGroup variable to one with a smaller number of groups:
#   combatSmall: frigates, destroyers
#   combatMedium: cruisers, battlecruisers
#   combatLarge: battleships
#   combatCapital: carriers, dreadnoughts, force auxiliaries
#   combatSuperCapital: supercarriers and Titans
#   noncombatSmall: mining and expedition frigates, barges, exhumers
#   noncombatMedium: exhumers, industrials, blockade runners
#   noncombatLarge: command industrial ships
#   noncombatFreighter: freighters and jump freighters
#   noncombatCapital: capital industrial ships
#   starbase: Player-owned starbases and Upwell structures such as citadels

# Current levels of shipGroup (there are 89) and their reassignments
dat$shipKind[dat$shipGroup=="Assault Frigate"] <- "combatSmall"
dat$shipKind[dat$shipGroup=="Assembly Array"] <- "starbase"
dat$shipKind[dat$shipGroup=="Attack Battlecruiser"] <- "combatMedium"
dat$shipKind[dat$shipGroup=="Battleship"] <- "combatLarge"
dat$shipKind[dat$shipGroup=="Black Ops"] <- "combatLarge"
dat$shipKind[dat$shipGroup=="Blockade Runner"] <- "noncombatMedium"
dat$shipKind[dat$shipGroup=="Capital Industrial Ship"] <- "noncombatCapital"
dat$shipKind[dat$shipGroup=="Carrier"] <- "combatCapital"
dat$shipKind[dat$shipGroup=="Citadel"] <- "starbase"
dat$shipKind[dat$shipGroup=="Combat Battlecruiser"] <- "combatMedium"

dat$shipKind[dat$shipGroup=="Combat Recon Ship"] <- "combatMedium"  # Cruiser
dat$shipKind[dat$shipGroup=="Command Destroyer"] <- "combatSmall"
dat$shipKind[dat$shipGroup=="Command Ship"] <- "combatMedium" # battlecruiser
dat$shipKind[dat$shipGroup=="Compression Array"] <- "starbase"
dat$shipKind[dat$shipGroup=="Control Tower"] <- "starbase"
dat$shipKind[dat$shipGroup=="Corporate Hangar Array"] <- "starbase"
dat$shipKind[dat$shipGroup=="Corvette"] <- "combatSmall" #starter ship
dat$shipKind[dat$shipGroup=="Covert Ops"] <- "combatSmall" # frigate
dat$shipKind[dat$shipGroup=="Cruiser"] <- "combatMedium"
dat$shipKind[dat$shipGroup=="Cynosural Generator Array"] <- "starbase"

dat$shipKind[dat$shipGroup=="Cynosural System Jammer"] <- "starbase"
dat$shipKind[dat$shipGroup=="Deep Space Transport"] <- "noncombatMedium"
dat$shipKind[dat$shipGroup=="Destroyer"] <- "combatSmall"
dat$shipKind[dat$shipGroup=="Dreadnought"] <- "combatCapital"
dat$shipKind[dat$shipGroup=="Electronic Attack Ship"] <- "combatSmall"
dat$shipKind[dat$shipGroup=="Electronic Warfare Battery"] <- "starbase"
dat$shipKind[dat$shipGroup=="Encounter Surveillance System"] <- "starbase" #??
dat$shipKind[dat$shipGroup=="Energy Neutralizing Battery"] <- "starbase"
dat$shipKind[dat$shipGroup=="Engineering Complex"] <- "starbase"
dat$shipKind[dat$shipGroup=="Exhumer"] <- "noncombatLarge" # Could be medium

dat$shipKind[dat$shipGroup=="Expedition Frigate"] <- "noncombatSmall"
dat$shipKind[dat$shipGroup=="Flag Cruiser"] <- "combatMedium"
dat$shipKind[dat$shipGroup=="Force Auxiliary"] <- "combatCapital"
dat$shipKind[dat$shipGroup=="Force Recon Ship"] <- "combatMedium" # cruiser
dat$shipKind[dat$shipGroup=="Forward Operating Base"] <- "starbase"
dat$shipKind[dat$shipGroup=="Freighter"] <- "noncombatFreighter"
dat$shipKind[dat$shipGroup=="Frigate"] <- "combatSmall"
dat$shipKind[dat$shipGroup=="Heavy Assault Cruiser"] <- "combatMedium"
dat$shipKind[dat$shipGroup=="Heavy Fighter"] <- "combatSmall" # drone
dat$shipKind[dat$shipGroup=="Heavy Interdiction Cruiser"] <- "combatMedium"

dat$shipKind[dat$shipGroup=="Industrial"] <- "noncombatMedium"
dat$shipKind[dat$shipGroup=="Industrial Command Ship"] <- "noncombatLarge"
dat$shipKind[dat$shipGroup=="Interceptor"] <- "combatSmall" # frigate
dat$shipKind[dat$shipGroup=="Interdictor"] <- "combatSmall" # destroyer
dat$shipKind[dat$shipGroup=="Jump Freighter"] <- "noncombatFreighter"
dat$shipKind[dat$shipGroup=="Jump Portal Array"] <- "starbase"
dat$shipKind[dat$shipGroup=="Laboratory"] <- "starbase"
dat$shipKind[dat$shipGroup=="Light Fighter"] <- "combatSmall" # drone
dat$shipKind[dat$shipGroup=="Logistics"] <- "combatSmall" # cruiser
dat$shipKind[dat$shipGroup=="Logistics Frigate"] <- "combatSmall" 

dat$shipKind[dat$shipGroup=="Marauder"] <- "combatLarge" # battleship
dat$shipKind[dat$shipGroup=="Mining Barge"] <- "noncombatMedium"
dat$shipKind[dat$shipGroup=="Mining Drone"] <- "noncombatSmall"
dat$shipKind[dat$shipGroup=="Mobile Cyno Inhibitor"] <- "starbase"
dat$shipKind[dat$shipGroup=="Mobile Depot"] <- "starbase"
dat$shipKind[dat$shipGroup=="Mobile Hybrid Sentry"] <- "starbase"
dat$shipKind[dat$shipGroup=="Mobile Laser Sentry"] <- "starbase"
dat$shipKind[dat$shipGroup=="Mobile Micro Jump Unit"] <- "starbase"
dat$shipKind[dat$shipGroup=="Mobile Missile Sentry"] <- "starbase"
dat$shipKind[dat$shipGroup=="Mobile Projectile Sentry"] <- "starbase"

dat$shipKind[dat$shipGroup=="Mobile Reactor"] <- "starbase"
dat$shipKind[dat$shipGroup=="Mobile Scan Inhibitor"] <- "starbase"
dat$shipKind[dat$shipGroup=="Mobile Siphon Unit"] <- "starbase"
dat$shipKind[dat$shipGroup=="Mobile Tractor Unit"] <- "starbase"
dat$shipKind[dat$shipGroup=="Mobile Warp Disruptor"] <- "starbase"
dat$shipKind[dat$shipGroup=="Moon Mining"] <- "starbase"
dat$shipKind[dat$shipGroup=="Orbital Construction Platform"] <- "starbase"
dat$shipKind[dat$shipGroup=="Orbital Infrastructure"] <- "starbase"
dat$shipKind[dat$shipGroup=="Personal Hangar"] <- "starbase"
dat$shipKind[dat$shipGroup=="Prototype Exploration Ship"] <- "combatSmall" 

dat$shipKind[dat$shipGroup=="Refinery"] <- "starbase"
dat$shipKind[dat$shipGroup=="Reprocessing Array"] <- "starbase"
dat$shipKind[dat$shipGroup=="Scanner Array"] <- "starbase"
dat$shipKind[dat$shipGroup=="Sensor Dampening Battery"] <- "starbase"
dat$shipKind[dat$shipGroup=="Shield Hardening Array"] <- "starbase"
dat$shipKind[dat$shipGroup=="Ship Maintenance Array"] <- "starbase"
dat$shipKind[dat$shipGroup=="Shuttle"] <- "noncombatSmall"
dat$shipKind[dat$shipGroup=="Silo"] <- "starbase"
dat$shipKind[dat$shipGroup=="Stasis Webification Battery"] <- "starbase"
dat$shipKind[dat$shipGroup=="Stealth Bomber"] <- "combatSmall"

dat$shipKind[dat$shipGroup=="Strategic Cruiser"] <- "combatMedium"
dat$shipKind[dat$shipGroup=="Supercarrier"] <- "combatSuperCapital"
dat$shipKind[dat$shipGroup=="Support Fighter"] <- "combatSmall"
dat$shipKind[dat$shipGroup=="Tactical Destroyer"] <- "combatSmall"
dat$shipKind[dat$shipGroup=="Titan"] <- "combatSmall"
dat$shipKind[dat$shipGroup=="Upwell Cyno Beacon"] <- "starbase"
dat$shipKind[dat$shipGroup=="Upwell Cyno Jammer"] <- "starbase"
dat$shipKind[dat$shipGroup=="Upwell Jump Gate"] <- "starbase"
dat$shipKind[dat$shipGroup=="Warp Scrambling Battery"] <- "starbase"

dat$shipKind <- as.factor(dat$shipKind)
levels(dat$shipKind) # looks as we would like

###############################################################################
###                         Logarithmic bounty                              ###
###############################################################################

# Given our histograms, we wish bounty to operate on a logarithmic scale also
# There are lots of zero bounties, as well as bounties less than 1
sum(dat$bounty == 0)
sum((dat$bounty != 0 & dat$bounty < 1)) # 203 nonzero bounties less than 1

# Because we are concerned with order of magnitude, it's not a big deal to
#   treat these bounties as zero. So we can log10(bounty + 1) as we did with
#   iskRecoverable!
dat$logBounty <- 0
dat$logBounty[dat$bounty > 0] <- log10(dat$bounty[dat$bounty > 0] + 1)

###############################################################################
###                           Regroup the data                              ###
###############################################################################

# We have found that:
#   region is not a good predictor
#   hour is not a good predictor
#   victimAlliance and finalAlliance have too many levels
#     and too much political complexity to group tighter
#   shipGroup, or at least shipKind, is a good predictor
#   bounty is as well (we log it)

# We will create a new condensed dataset accounting for this.
byAARK <- dat %>% group_by(victimAlliance,finalAlliance,region,shipKind) %>% 
  summarize(logRecoverable=mean(logRecoverable),logBounty=mean(logBounty))
  # We can customize the collapsed variables to control the size of the data

byAARK$victimAlliance <- NULL
byAARK$finalAlliance <- NULL

###############################################################################
###                 Analysis with the new dataset                           ###
###############################################################################

# We conduct a linear model of the logarithm of ISK recoverable against the 
# shipKind and the logarithm of the bounty, allowing for interactions between
# them.
byAARK$shipKind <- reorder(byAARK$shipKind,byAARK$logRecoverable)

logKi <- lm(logRecoverable~shipKind,data=byAARK)
summary(logKi)
confint(logKi)

logKiBo <- lm(logRecoverable~shipKind*logBounty,data=byAARK)
summary(logKiBo)
  # Most interactions, except between bounty and small ships, are not sig.

# Without interactions:
logKB <- lm(logRecoverable~shipKind+logBounty,data=byAARK)
summary(logKB)
  # Very little difference in R^2 or residual SE! And now bounty is sig.

byAARK$region <- reorder(byAARK$region,byAARK$logRecoverable)
logReg2 <- lm(logRecoverable~region,data=byAARK)
summary(logReg2)
  # On the logarithmic scale, this becomes significant...

lmReg2 <- lm(I(exp(logRecoverable)-1)~region,data=byAARK)
summary(lmReg2)
  # The ordering isn't necessarily on point, but there is significance heres

### Let's create a model involving region, shipKind, and bounty
logKiBoRe <- lm(logRecoverable~shipKind*logBounty+region*logBounty,data=byAARK)
summary(logKiBoRe)

# Bounty matters for little ships only, but we don't want to drag others in


### Without interactions
byAARK$shipKind <- reorder(byAARK$shipKind,byAARK$logRecoverable)
logKBR <- lm(logRecoverable~shipKind+region+logBounty,data=byAARK)
summary(logKBR)
plot(logKBR)

###############################################################################
###                 A further analysis of time of day                       ###
###############################################################################

# We can manage the amount of data that we have, and suspect that on the
#   logarithmic scale the hour of day, or perhaps the date, will be important.
byHour <- dat %>% group_by(hour) %>% 
  summarize(logRecoverable=mean(logRecoverable))

plot(logRecoverable~hour,data=byHour)
  # See, now, this looks like something important

# With error bars, though, perhaps not
byHAARK <- dat %>% group_by(hour,shipKind,region,victimAlliance,
                            finalAlliance) %>%
  summarize(logRecoverable=mean(logRecoverable),logBounty=mean(logBounty))

plot(logRecoverable~hour,data=byHAARK) # It's much less clear now

logHour <- lm(logRecoverable~hour,data=byHAARK)
summary(logHour)
  # We see differences!

# It is not entirely clear how these should be ordered, to my mind,
# although hours 12-23 seem to be grouped somewhat. Maybe a sinusoid?
byHAARK$hour <- reorder(byHAARK$hour,byHAARK$logRecoverable)
logHouOrd <- lm(logRecoverable~hour,data=byHAARK)
summary(logHouOrd)

# Let's plot a time series with date and hour
byHoDa <- dat %>% group_by(date,hour) %>%
  summarize(logRecoverable=mean(logRecoverable))

ggplot(aes(x=date,y=logRecoverable),data=byHoDa) + geom_point()
ggplot(aes(x=hour,y=logRecoverable),data=byHoDa) + geom_point()
  # I think the difference is more obvious in the date plot

logDate <- lm(logRecoverable~date,data=byHoDa)
summary(logDate)
  # In the linear model context, the date doesn't matter

hourACF <- acf(byHoDa$logRecoverable,xlab="Lag (hours)",ylab="Autocorrelation",
    main="Autocorrelation of recoverable ISK by hour")
# We see autocorrelation peaking at period 23-24!
  # And 1, but this is perhaps to be expected

# Can we find this just grouping by date?
byDate <- dat %>% group_by(date,victimAlliance) %>% 
  summarize(logRecoverable=mean(logRecoverable))

acf(byDate$logRecoverable) # Not a whole lot going on here!


###############################################################################
###                           The final model                               ###
###############################################################################

### We found that shipKind is the major predictor, but that region and
###   logBounty matter as well. In addition, when the ship is of a small kind,
###   there is an interaction between shipKind and logBounty.
### The response is logRecoverable.

# An indicator variable for small ships
byAARK$smallCombat <- 0
byAARK$smallCombat[byAARK$shipKind=="combatSmall"] <- 1
byAARK$smallNonCombat <- 0
byAARK$smallNonCombat[byAARK$shipKind=="noncombatSmall"] <- 1

byAARK$shipKind <- reorder(byAARK$shipKind,byAARK$logRecoverable)
byAARK$region <- reorder(byAARK$region,byAARK$logRecoverable)
fit <- lm(logRecoverable~shipKind+logBounty+region+
            logBounty:smallCombat+logBounty:smallNonCombat,data=byAARK)
summary(fit)
plot(fit)


###############################################################################
###                     Graphics and interpretations                        ###
###############################################################################

# Do we need different indicators for small combat and small noncombat ships?
linearHypothesis(fit,"logBounty:smallCombat = logBounty:smallNonCombat") # yes

# The autocorrelation in hours is interesting, but it is a very subtle distinction,
#   so we will relegate its analysis to future work
acf(byHoDa$logRecoverable,xlab="Lag (hours)",ylab="Autocorrelation",
               main="Autocorrelation of recoverable ISK by hour")


# Prediction graph for logBounty.
#   For our prediction, we will use combatMedium and The Citadel as "average"
#   shipKind and region, respectively.
predNotSmall <- tibble(
  logBounty=seq(min(byAARK$logBounty),max(byAARK$logBounty),length=5000),
  smallCombat=rep(0,5000),
  smallNonCombat=rep(0,5000),
  region=rep("The Citadel",5000),
  shipKind=rep("combatMedium",5000)
)
predNotSmall$logRecoverable <- predict(fit,newdata=predNotSmall)

predSmallCombat <- tibble(
  logBounty=seq(min(byAARK$logBounty),max(byAARK$logBounty),length=5000),
  smallCombat=rep(1,5000),
  smallNonCombat=rep(0,5000),
  region=rep("The Citadel",5000),
  shipKind=rep("combatSmall",5000)
)
predSmallCombat$logRecoverable <- predict(fit,newdata=predSmallCombat)

predSmallNonCombat <- tibble(
  logBounty=seq(min(byAARK$logBounty),max(byAARK$logBounty),length=5000),
  smallCombat=rep(0,5000),
  smallNonCombat=rep(1,5000),
  region=rep("The Citadel",5000),
  shipKind=rep("noncombatSmall",5000)
)
predSmallNonCombat$logRecoverable <- predict(fit,newdata=predSmallNonCombat)

byAARK$colorVar <- "Large ship"
byAARK$colorVar[byAARK$smallCombat==1] <- "Small combat ship"
byAARK$colorVar[byAARK$smallNonCombat==1] <- "Small noncombat ship"

ggplot(byAARK,aes(logBounty,logRecoverable)) + 
  geom_point(aes(color=colorVar),show.legend=TRUE) + 
  geom_line(data=predNotSmall,color="green",size=2) + 
  geom_line(data=predSmallCombat,color="red",size=2) + 
  geom_line(data=predSmallNonCombat,color="blue",size=2) + 
  theme(legend.title = element_blank()) +
  xlab("Logarithm of bounty") + ylab("Logarithm of recoverable ISK") + 
  scale_color_manual(values=c("#00BA38","#F8766D","#00BFC4"))

# Histograms for the recoverable ISK and its logarithm
byAARK$iskRecoverable <- 10^(byAARK$logRecoverable) + 1

ggplot(byAARK,aes(iskRecoverable)) + 
  geom_histogram(bins=50,color="red") + 
  xlab("Recoverable ISK") + ylab("Count")

ggplot(byAARK,aes(logRecoverable)) + 
  geom_histogram(bins=50,color="blue") + 
  xlab("Logarithm of recoverable ISK") + ylab("Count")

# Boxplot for the shipKind effect
ggplot(byAARK,aes(shipKind,logRecoverable)) + 
  geom_boxplot() + 
  theme(axis.text.x=element_text(angle=90,vjust=-0.5)) +
  xlab("") + ylab("Logarithm of recoverable ISK")
