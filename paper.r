devel.df <- read.delim("development.txt")
devel.df$TempNom <- factor(devel.df$TempNom)

days.lm <- lm(Days~ TempNom + Sex, data = devel.df)
anova(days.lm)
anova(update(days.lm, . ~. -Sex))
new.df <- unique(devel.df[, c("TempNom", "Sex")])
day.pred <- predict(days.lm, new.df, se.fit = TRUE)
new.df$Days <- day.pred$fit
new.df$Days.sem <- day.pred$se.fit

bwplot(TempNom ~ Days | Sex, data = devel.df)
bwplot(Sex~ Days | TempNom , data = devel.df)
bwplot(Sex~ Length | TempNom , data = devel.df)

## require(reshape2)
## dcast(devel.df, Length~ TempNom, fun.aggregate = mean)

lengthD.aov <- aov(Length~ TempNom, data = devel.df)
model.tables(lengthD.aov, type = "means", se = TRUE)
summary(length.lm)

lengthD.lm <- lm(Length~ TempNom + Sex, data = devel.df)
anova(lengthD.lm)
anova(update(lengthD.lm, . ~. -Sex))
newL.df <- data.frame(TempNom = unique(devel.df[,1]))
len.pred <- predict(update(lengthD.lm, . ~. -Sex), newL.df, se.fit = TRUE)
lengthD.df <- within(newL.df, Length <- len.pred$fit)
lengthD.df <- within(lengthD.df, SE <- len.pred$se.fit)
require("multcompView")
lengthD.df$Group <- multcompLetters(TukeyHSD(lengthD.aov)$TempNom[,4])$
monospacedLetters[levels(lengthD.df$TempNom)]

## for comparison, what comes from simply aggregating it all
len.agg.df <- aggregate(Length~ TempNom, data = devel.df, FUN = mean)
len.agg.df$SE <- aggregate(Length~ TempNom, data = devel.df, FUN = sem)$Length
len.agg.df$N <- aggregate(Length~ TempNom, data = devel.df, FUN = length)$Length

aggregate(Days~ TempNom, data = devel.df, FUN = mean)
aggregate(Days~ TempNom, data = devel.df, FUN = sem)
aggregate(Days~ TempNom, data = devel.df, FUN = length)$Days

## Tab: Development Rate
##########################################

## non-linear fit (Lactin, et al model)
plot.develop(devel.df, rnd = 7) # Non-linearFit.pdf
## show below the x axis
plot.develop2(devel.df, rnd = 7) # Non-linearFit2.pdf 
## Logan model of same data
plot.developA(devel.df, rnd = 7) # Non-linearFitA.pdf 
options(digits = 10)

## with a Tmin with its own exponent multiplier and an intercept 
plot.developC2() # >> Non-linearFitC2.pdf
## then with no intercept
plot.developC() # >> Non-linearFitC.pdf

#####
## Tab: Emergence
##########################################
emerge.df <- read.delim("emerge.txt")
long.emerge.df <- melt(emerge.df, id = names(emerge.df)[1:3],
                       variable.name = "State", value.name = "Count")
long.emerge.df <- within(long.emerge.df, NomTemp <- factor(NomTemp))

bwplot(State~Count | TempRec, data = long.emerge.df)
bwplot(State~Count | NomTemp, data = long.emerge.df)
xyplot(jitter(Count) ~ jitter(TempRec) | State, data = long.emerge.df)
emerge.df <- within(emerge.df, NomTemp <- factor(NomTemp))
require(MASS)
emerge.glms()

## Tab Adult Emergence
##########################################
adultEmerg.df <- read.delim("AdultEmergence.txt")
adultEmerg.df <- within(adultEmerg.df, Length <- Length/20)
adultEmerg.df <- within(adultEmerg.df, TempNom <- factor(TempNom))

bwplot(TempNom ~ Days | Sex, data = adultEmerg.df)
bwplot(TempNom ~ Length | Sex, data = adultEmerg.df)
bwplot(Sex~ Days | TempNom, data = adultEmerg.df)
bwplot(Sex~ Length | TempNom , data = adultEmerg.df)

lengthE.lm <- lm(Length ~ TempNom * Sex, data = adultEmerg.df)
anova(lengthE.lm)

lengthE.aov <- aov(Length~ TempNom + Sex, data = adultEmerg.df)
model.tables(lengthE.aov, type = "means", se = TRUE)
summary(lengthE.lm)

lengthE.df$Group <- multcompLetters(TukeyHSD(lengthE.aov)$TempNom[,4])$
monospacedLetters[levels(lengthD.df$TempNom)]

newE.df <- unique(adultEmerg.df[,c(1, 3)])
len.predE <- predict(lengthE.lm, newE.df, se.fit = TRUE)
lengthE.df <- within(newE.df, Length <- len.predE$fit)
lengthE.df <- within(lengthE.df, SE <- len.predE$se.fit)

### Won't use the continuous, but maybe interesting
## as continuous temperatures
lengthE_C.lm <- lm(Length ~ TempRecord * Sex, data = adultEmerg.df)
anova(lengthE_C.lm)
anova(update(lengthE_C.lm, . ~. -TempRecord:Sex))
newE_C.df <- unique(adultEmerg.df[,c(2, 3)])
len.predE_C <- predict(lengthE_C.lm, newE_C.df, se.fit = TRUE)
lengthE_C.df <- within(newE_C.df, Length <- len.predE_C$fit)
lengthE_C.df <- within(lengthE_C.df, SE <- len.predE_C$se.fit)
summary(update(lengthE_C.lm, . ~. -TempRecord:Sex))

## Development time (days) -- uses same newE.df and newE_C)
daysE.lm <- lm(Days ~ TempNom * Sex, data = adultEmerg.df)
anova(daysE.lm)
anova(update(daysE.lm, . ~. -TempNom:Sex))
newE.df <- unique(adultEmerg.df[,c(1, 3)])
day.predE <- predict(daysE.lm, newE.df, se.fit = TRUE)
daysE.df <- within(newE.df, Days <- len.predE$fit)
daysE.df <- within(daysE.df, SE <- len.predE$se.fit)

# as continuous temperatures
daysE_C.lm <- lm(Days ~ TempRecord * Sex, data = adultEmerg.df)
anova(daysE_C.lm)
anova(update(daysE_C.lm, . ~. -TempRecord:Sex))
newE_C.df <- unique(adultEmerg.df[,c(2, 3)])
day.predE_C <- predict(daysE_C.lm, newE_C.df, se.fit = TRUE)
daysE_C.df <- within(newE_C.df, Days <- day.predE_C$fit)
daysE_C.df <- within(daysE_C.df, SE <- day.predE_C$se.fit)
summary(update(daysE_C.lm, . ~. -TempRecord:Sex))

############################################################
## Use separate M and F aovs to get Tukey groups
newES.df <- data.frame(TempNom = unique(adultEmerg.df[,c(1)]))

MadultEmerg.df <- adultEmerg.df[adultEmerg.df$Sex == "M",]
FadultEmerg.df <- adultEmerg.df[adultEmerg.df$Sex == "F",]
## separate dataframes for predictions
MlengthE.df <- lengthE.df[lengthE.df$Sex == "M", -2]
FlengthE.df <- lengthE.df[lengthE.df$Sex == "F", -2]
MlengthE.aov <- aov(Length~ TempNom, data = MadultEmerg.df)
FlengthE.aov <- aov(Length~ TempNom, data = FadultEmerg.df)
MlengthE.df$Group <- multcompLetters(TukeyHSD(MlengthE.aov)$TempNom[,4])$
monospacedLetters[levels(lengthE.df$TempNom)]
FlengthE.df$Group <- multcompLetters(TukeyHSD(FlengthE.aov)$TempNom[,4],
                                     reversed = TRUE)$
monospacedLetters[levels(lengthE.df$TempNom)]

MdaysE.df <- daysE.df[daysE.df$Sex == "M", -2]
FdaysE.df <- daysE.df[daysE.df$Sex == "F", -2]
MdaysE.aov <- aov(Days~ TempNom, data = MadultEmerg.df)
FdaysE.aov <- aov(Days~ TempNom, data = FadultEmerg.df)
MdaysE.df$Group <- multcompLetters(TukeyHSD(MdaysE.aov)$TempNom[,4])$
monospacedLetters[levels(daysE.df$TempNom)]
FdaysE.df$Group <- multcompLetters(TukeyHSD(FdaysE.aov)$TempNom[,4])$
  monospacedLetters[levels(daysE.df$TempNom)]



################################################################
## Stuff not to do with temperature: different Excel file 14/5/13
############################################################
## Tab Female longevity with no hosts
FlongNohost.df <- read.delim("FlongevityNohost.txt")
with(FlongNohost.df, table(Diet))
Flong.aov <- aov(Longevity ~ Diet, data = FlongNohost.df)
summary(Flong.aov)
anova(Flong.aov)
summarize(FlongNohost.df, "Longevity", "Diet")
Flong.pred <- predict(Flong.aov,
                      newdat = data.frame(Diet =unique(FlongNohost.df$Diet)),
                      se.fit = TRUE)

FlongNH.pred.df <- as.data.frame(Flong.pred[1:2])
row.names(FlongNH.pred.df) <- unique(FlongNohost.df$Diet)
names(FlongNH.pred.df) <- c("Longevity", "SE")
FlongNH.pred.df # OK
TukeyHSD(Flong.aov)

FlongNH.pred.df$Group <- multcompLetters(TukeyHSD(Flong.aov)$Diet[,4])$monospacedLetters[levels(FlongNohost.df$Diet)]

FlongTib.aov <- aov(Longevity ~ Tibia, data = FlongNohost.df)
summary(FlongTib.aov) # no Tibia "effect" p=0.796

dietsNH.df <- data.frame(Diet =unique(FlongNohost.df$Diet))
###########
## Tab Female longevity with hosts
FlongHost.df <- read.delim("FlongevityHost.txt")
with(FlongHost.df, table(Diet, Virgin))
dietsH.df <- data.frame(Diet = factor(unique(FlongHost.df$Diet)))

FlongH.aov <- aov(Longevity ~ Diet + Virgin, data = FlongHost.df)
bwplot(Diet ~ Longevity | Virgin, data = FlongHost.df, layout = c(1,2))
bwplot(Virgin ~ Longevity | Diet, data = FlongHost.df, layout = c(1,3))
summary(FlongH.aov)
anova(FlongH.aov)

FoffH.glm <- glm(Offspring ~ Diet + Virgin, data = FlongHost.df, family = poisson)
summary(FoffH.glm)
FoffHQ.glm <- glm(Offspring ~ Diet + Virgin, data = FlongHost.df, family = quasipoisson)
summary(FoffHQ.glm)
FoffNB.glm <- glm.nb(Offspring ~ Diet + Virgin, data = FlongHost.df)
summary(FoffNB.glm)
FoffNB.pred <- predict(update(FoffNB.glm, .~. - Virgin),
                       newdat = dietsH.df, se.fit = TRUE)

summarize(FlongHost.df, "Offspring", "Diet")
FoffPred <- as.data.frame(lapply(FoffNB.pred[1:2], exp))
dimnames(FoffPred) <- list((dietsH.df$Diet), c("Offspring", "SE"))
## and if done from ANOVA
FoffH.aov <- aov(Offspring ~ Diet + Virgin, data = FlongHost.df)
summary(FoffH.aov)
FoffAOV.pred <- predict(update(FoffH.aov, .~. - Virgin),
                       newdat = dietsH.df, se.fit = TRUE)
FoffPredAOV <- as.data.frame(FoffAOV.pred[1:2])
dimnames(FoffPredAOV) <- list((dietsH.df$Diet), c("Offspring", "SE"))
FoffPredAOV$Group <- multcompLetters(TukeyHSD(FoffH.aov)$Diet[,4])$
monospacedLetters[rownames(FoffPredAOV)]

###
## fecundity
summarize(FlongHost.df, "Fecundity", "Diet")
FfecH.aov <- aov(Fecundity ~ Diet + Virgin, data = FlongHost.df)
summary(FfecH.aov)
anova(FfecH.aov)
bwplot(Diet ~ Fecundity | Virgin, data = FlongHost.df, layout = c(1,2))
bwplot(Virgin ~ Fecundity |Diet , data = FlongHost.df, layout = c(1,3))
FfecAOV.pred <- predict(update(FfecH.aov, .~. - Virgin),
                       newdat = dietsH.df, se.fit = TRUE)
FfecPredAOV <- as.data.frame(FfecAOV.pred[1:2])
dimnames(FfecPredAOV) <- list((dietsH.df$Diet), c("Fecundity", "SE"))
FfecPredAOV$Group <- multcompLetters(TukeyHSD(FfecH.aov)$Diet[,4])$
monospacedLetters[rownames(FfecPredAOV)]

################################
## Tab Male longevity
Mlong.df <- read.delim("Mlongevity.txt")
bwplot(Diet ~ Longevity, data = Mlong.df)
Mlong.aov <- aov(Longevity ~ Diet, data = Mlong.df)
summary(Mlong.aov)
MlongAOV.pred <- predict(Mlong.aov, newdat = dietsH.df, se.fit = TRUE)

MlongPredAOV <- as.data.frame(MlongAOV.pred[1:2])
dimnames(MlongPredAOV) <- list((dietsH.df$Diet), c("Longevity", "SE"))
MlongPredAOV$Group <- multcompLetters(TukeyHSD(Mlong.aov)$Diet[,4])$
monospacedLetters[rownames(MlongPredAOV)]
MlongPredAOV # OK

bwplot(Diet ~ Tibia, data = Mlong.df)
Mtib.aov <- aov(Tibia ~ Diet, data = Mlong.df)
summary(Mtib.aov)
MtibAOV.pred <- predict(Mtib.aov, newdat = dietsH.df, se.fit = TRUE)

MtibPredAOV <- as.data.frame(MtibAOV.pred[1:2])
dimnames(MtibPredAOV) <- list((dietsH.df$Diet), c("Fecundity", "SE"))
MtibPredAOV$Group <- multcompLetters(TukeyHSD(Mtib.aov)$Diet[,4])$
  Letters[rownames(MtibPredAOV)]
MtibPredAOV

################################
## Tab Offspring produced
offspringNo.df <- read.delim("OffspringNo.txt") # development time, not numbers
OffDev.aov <- aov(Development ~ Diet + Virgin + Sex, data = offspringNo.df)
summary(OffDev.aov)
dietsOff.df <- unique(offspringNo.df[, c(1:2, 4)])
dietsOff.df <- df.sort(dietsOff.df, 3:1) # for predictions

OffDev.pred <- predict(OffDev.aov, newdat = dietsOff.df, se.fit = TRUE)
OffDev.pred.df <- cbind(dietsOff.df, as.data.frame(OffDev.pred[1:2])) # bit silly

## Mated only
offspringDevM.df <- offspringNo.df[offspringNo.df$Virgin == "Mated",]

OffDevM.aov <- aov(Development ~ Diet * Sex, data = offspringDevM.df)
summary(OffDevM.aov)# interaction Diet:Sex


OffDevMf.aov <- aov(Development ~ Diet, data = offspringDevM.df,
                    subset = Sex == "Female")
summary(OffDevMf.aov)# major effect of diet
OffDevMm.aov <- aov(Development ~ Diet, data = offspringDevM.df,
                    subset = Sex == "Male")
summary(OffDevMm.aov)#  major effect of diet here also

dietsOff.dfM <- dietsOff.df[dietsOff.df$Virgin != "Virgin",] # for predictions
dietsOff.dfMf <- dietsOff.dfM[dietsOff.dfM$Sex == "Female",]
dietsOff.dfMm <- dietsOff.dfM[dietsOff.dfM$Sex == "Male",]
OffDev.predMf <- predict(OffDevMf.aov, dietsOff.dfMf, se.fit = TRUE)
OffDev.predMm <- predict(OffDevMm.aov, dietsOff.dfMm, se.fit = TRUE)

OffDev.predMf.df <- cbind(dietsOff.dfMf, as.data.frame(OffDev.predMf[1:2]))
names(OffDev.predMf.df)[4:5] <- c("Days", "SE") # OK (p << 0.001)
OffDev.predMm.df <- cbind(dietsOff.dfMm, as.data.frame(OffDev.predMm[1:2]))
names(OffDev.predMm.df)[4:5] <- c("Days", "SE") # OK (p << 0.001)

## Unmated only
offspringDevV.df <- offspringNo.df[offspringNo.df$Virgin == "Virgin",]
OffDevV.aov <- aov(Development ~ Diet , data = offspringDevV.df)
summary(OffDevV.aov)
dietsOff.dfV <- dietsOff.df[dietsOff.df$Virgin == "Virgin",] # for predictions
OffDev.predV <- predict(OffDevV.aov, dietsOff.dfV, se.fit = TRUE)
OffDev.predV.df <- cbind(dietsOff.dfV, as.data.frame(OffDev.predV[1:2]))
names(OffDev.predV.df)[4:5] <- c("Days", "SE") # OK pval = 0.000126

OffDev.predAll.df <- rbind(OffDev.predV.df, OffDev.predMm.df, OffDev.predMf.df)

######  Use Size column instead of Development column
###### size (maybe none, altogether might work -- no such luck
OffSize.aov <- aov(Size ~ Diet + Virgin + Sex, data = offspringNo.df)
bwplot(Diet ~ Size | Virgin + Sex, data = offspringNo.df)
bwplot(Virgin ~ Size | Sex  + Diet, data = offspringNo.df)
bwplot(Diet ~ Development | Virgin + Sex, data = offspringNo.df)
bwplot(Virgin ~ Development |Sex  + Diet, data = offspringNo.df)
OffSize.aov <- aov(Size ~ Diet *Virgin * Sex, data = offspringNo.df)
summary(OffSize.aov)## can't stay

OffSizeMf.aov <- aov(Size ~ Diet, data = offspringDevM.df,
                    subset = Sex == "Female")
summary(OffSizeMf.aov)# major effect of diet
OffSizeMm.aov <- aov(Size ~ Diet, data = offspringDevM.df,
                    subset = Sex == "Male")
summary(OffSizeMm.aov)#  large effect of diet here also

dietsOff.dfM <- dietsOff.df[dietsOff.df$Virgin != "Virgin",] # for predictions
dietsOff.dfMf <- dietsOff.dfM[dietsOff.dfM$Sex == "Female",]
dietsOff.dfMm <- dietsOff.dfM[dietsOff.dfM$Sex == "Male",]
OffSize.predMf <- predict(OffSizeMf.aov, dietsOff.dfMf, se.fit = TRUE)
OffSize.predMm <- predict(OffSizeMm.aov, dietsOff.dfMm, se.fit = TRUE)

OffSize.predMf.df <- cbind(dietsOff.dfMf, as.data.frame(OffSize.predMf[1:2]))
names(OffSize.predMf.df)[4:5] <- c("Size", "SE") # OK (p << 0.001)
OffSize.predMm.df <- cbind(dietsOff.dfMm, as.data.frame(OffSize.predMm[1:2]))
names(OffSize.predMm.df)[4:5] <- c("Size", "SE") # OK (p << 0.001)


summary(aov(Size ~ Virgin, data = offspringDevMating.df))# p <<0.001
summary(aov(Development ~ Virgin, data = offspringDevMating.df)) # p=0.191

### Unmated:
OffSizeV.aov <- aov(Size ~ Diet , data = offspringDevV.df)
summary(OffSizeV.aov)# major effect of diet
dietsOff.dfV <- dietsOff.df[dietsOff.df$Virgin == "Virgin",] # for predictions
OffSize.predV <- predict(OffSizeV.aov, dietsOff.dfV, se.fit = TRUE)
OffSize.predV.df <- cbind(dietsOff.dfV, as.data.frame(OffSize.predV[1:2]))
names(OffSize.predV.df)[4:5] <- c("Size", "SE") # OK pval = 0.000126

OffSize.predAll.df <- rbind(OffSize.predV.df, OffSize.predMm.df, OffSize.predMf.df)


## effect of mating:
offspringDevMating.df <- offspringNo.df[offspringNo.df$Sex == "Male",]

require(plyr)

############################################################
## Tab: Hosts parasitised per day
parasitised.df <- read.delim("Parasitised.txt")
## ditch(cols 1 & 3)
parasitised.df <- parasitised.df[-c(2, 4)]#  Day column already
require(plyr)
names(parasitised.df) <- gsub("\\.", "", names(parasitised.df))

parasitised.df$Day <- seq(nrow(parasitised.df))
parasitised.df1 <- melt(parasitised.df[1:30,], id = "Day", value.name = "Eggs") 
parasitised.df1 <- within(parasitised.df1, Eggs[is.na(Eggs)] <- 0)
with(parasitised.df1, sunflowerplot(Day, variable, Eggs, yaxt = "n",
                                    seg.lwd = .5, size = 2/25.4, ylab = ""))
axis(2, at = seq(10), labels = levels(parasitised.df1$variable),
     las = 2)

############################################################
## Tab: Offspring produced per day

offspring.df <- read.delim("Offspring.txt")
names(offspring.df) <- gsub("\\.", "", names(offspring.df))
## ditch(cols 1 & 3)
offspring.df <- offspring.df[-c(1, 3)]# no Day column yet
offspring.df$Day <- seq(nrow(offspring.df))

offspring.df1 <- melt(offspring.df[1:30,], id = "Day", value.name = "Eggs") 
offspring.df1 <- within(offspring.df1, Eggs[is.na(Eggs)] <- 0)
with(offspring.df1, sunflowerplot(Day, variable, Eggs, yaxt = "n",
                                    seg.lwd = .5, size = 2/25.4, ylab = ""))
axis(2, at = seq(10), labels = levels(offspring.df1$variable),
     las = 2)
 
############################################################
## Tab: Combined

combined.df <- read.delim("Combined.txt")
levels(combined.df$Hosts) <- c("No hosts", "Hosts")
bwplot(Diet ~ Longevity | Hosts + Virgin, data = combined.df)
bwplot(Virgin ~ Longevity | Hosts + Diet, data = combined.df)
file:///home/hrapgc/Rstuff/Bioprotection/asha/mastrus/paper/

## bwplot(Diet ~ Codling | Hosts + Virgin, data = combined.df)
## bwplot(Virgin ~ Codling | Hosts + Diet, data = combined.df)

## bwplot(Diet ~ Offspring | Hosts + Virgin, data = combined.df)
## bwplot(Virgin ~ Offspring | Hosts + Diet, data = combined.df)



summarize(offspringNo.df, "Size", c("Diet", "Virgin", "Sex"  ))


lookseePlots ## >>  MastrusOverview.pdf

install.packages("nlmrt")
