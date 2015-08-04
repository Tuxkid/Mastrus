adult.df <- read.delim("adult.txt")
size.df <- read.delim("size.txt")
sizeA.df <- read.delim("size2.txt")
egg.df <- read.delim("egg.txt")

parasite.df <- read.delim("parasite.txt")
check.fact(egg.df)

levels(adult.df$Species)
levels(size.df$Species)
levels(egg.df$Species)
size.df$Species <- factor(sub("^+ ", "", as.character(size.df$Species)))
sizeA.df$Species <- factor(sub("^+ ", "", as.character(sizeA.df$Species)))
egg.df$Species <- factor(sub("^+ ", "", as.character(egg.df$Species)))
adult.df$Species <- factor(sub("^+ ", "", as.character(adult.df$Species)))
parasite.df$Species <- factor(sub("^+ ", "", as.character(parasite.df$Species)))

with(adult.df, table(Species, Rep))
with(egg.df, table(Species, Rep))
with(egg.df, table(Species, Laid))

aggregate(AdultSize ~ Sex + Species, length, data = size.df)
aggregate(AdultSize ~ Sex + Species, mean, data = size.df)
aggregate(AdultSize ~ Sex + Species, sem, data = size.df)

## Emerge

emerge.glm <- glm(cbind(Adults, Laid - Adults) ~ Species, data = adult.df,
                  family = binomial)
anova(emerge.glm, test = "Chi")
summary(emerge.glm) ## rather overdispersed.... try neg binom
require(MASS)
emerge.nb <- glm.nb(Adults/Laid ~ Species, weights = Laid, data = adult.df)
summary(emerge.nb)
anova(emerge.nb)

aggregate(Adults/Laid ~ Species, mean, data = adult.df)
aggregate(Adults/Laid ~ Species, length, data = adult.df)

av.dat <- data.frame(Species = unique(adult.df$Species), Laid = 100)
predict(emerge.nb, newdata = av.dat)
## Start again: make codling moth the control
species <- levels(adult.df$Species)
species.o <- species[c(3, (1:6)[-3])]

adult.df$Species <- factor(adult.df$Species, levels = species.o)
emerge.nb <- glm.nb(Adults/Laid ~ Species, weights = Laid, data = adult.df)
summary(emerge.nb)
anova(emerge.nb)
## line up fitted values and get proportions emerged
emergence.df <- get.pred() ## Keep
## <late edit>
### 18/1/12: error in adult.df's text file: reread, etc...
adult.df <- read.delim("adult.txt")
adult.df$Species <- factor(sub("^+ ", "", as.character(adult.df$Species)))
adult.df$Species <- factor(adult.df$Species, levels = species.o)
emerge.nb <- glm.nb(Adults/Laid ~ Species, weights = Laid, data = adult.df)
summary(emerge.nb)
anova(emerge.nb)
## pretty much the same if done this way.....
emerge.nb2 <- glm.nb(Adults ~ Species + offset(log(Laid)), data = adult.df)
summary(emerge.nb2)
anova(emerge.nb2)
## line up fitted values and get proportions emerged
emergence.df <- get.pred(emerge.nb) ## Keep

## </late edit>

emerge.nb2 <- glm.nb(Adults ~ Species + offset(log(Laid)), data = adult.df)
summary(emerge.nb2)
anova(emerge.nb2)

get.pred13()
## Body size
aggregate(Adult.size ~ Sex + Species, length, data = size.df)
aggregate(Adult.size ~ Sex + Species, mean, data = size.df)

xyplot(Sex ~ jitter(Adult.size) | Species, data = size.df)
bwplot(Sex ~ jitter(Adult.size) | Species, data = size.df)

size.df$Species <- factor(size.df$Species, levels = species.o)
sizeA.df$Species <- factor(sizeA.df$Species, levels = species.o)


size.glm <- glm(Adult.size ~ Species * Sex, data = size.df)
summary(size.glm)
anova(size.glm, test = "F")
size.glmA<- glm(Adult.size ~ Species * Sex, data = sizeA.df)
summary(size.glmA)
anova(size.glmA, test = "F")# from this experiment only
## No Sex effect or interaction.
size.glmB<- glm(Adult.size ~ Species, data = sizeA.df)
summary(size.glmB)
anova(size.glmB, test = "F")

size.pred <- predict(size.glmB, type = "response", se = TRUE)
sizeA.df$Fit <- size.pred$fit
sizeA.df$Fit.se <- size.pred$se.fit
size.fit.df <- unique(sizeA.df[c(1,4:5)])
size.fit.df$`t-val` <- NA # extra column 21/3/13
size.fit.df$`P-val` <- NA
rownames(size.fit.df) <- size.fit.df$Species
for(i in size.fit.df$Species){
size.fit.df[i, "t-val"] <- # extra calculation 21/3/13
  round(as.numeric(try(summary(size.glmB)$coefficients[ppaste("Species", i),
                                                       3])),3)
size.fit.df[i, "P-val"] <-
  round(as.numeric(try(summary(size.glmB)$coefficients[ppaste("Species", i),
                                                       4])),3)
}

Size.df <- size.fit.df[-1] ## Use this dataframe for ouput table

###  Sex ratio
sex.tab <- with(sizeA.df, table(Species, Sex))
attributes(sex.tab)$class <- "matrix"
sex.df <- as.data.frame(sex.tab)
sexRatio.df <- sex.check() ## Use this one.
## <mostly useless>
egg2.df <- parasite.df[with(parasite.df, Laid < 7 & Laid >0 & Condition == "Paralysed"),]
egg2.df[egg2.df$Species == "Planotortrix octo",]
egg.df[egg.df$Species == "Planotortrix octo",]
egg2.df[egg2.df$Species == "Grapholita molesta",]
egg.df[egg.df$Species == "Grapholita molesta",]

om2.df <- egg2.df[egg2.df$Species == "Cydia succedana",]
om.df <- egg.df[egg.df$Species == "Cydia succedana",]
egg3.df <- parasite.df[with(parasite.df, Laid < 7 & Laid >0 & Condition == "Paralysed"),]
## </mostly useless>

## Look only at eggs in paralysed larvae, even those not used in size measure
paralysed.df <- parasite.df[with(parasite.df, Condition == "Paralysed"),]
paralysed.df$Species <- factor(sub("^+ ", "", as.character(paralysed.df$Species)))
summarize(paralysed.df, "Laid", "Species")
dotplot(Species ~ jitter(Laid), data = paralysed.df)
bwplot(Species ~ Laid, data = paralysed.df)
## Fix up level order
paralysed.df$Species <- factor(paralysed.df$Species, levels = species.o)

paralysed.glm <- glm(Laid ~ Species, family = "poisson", data = paralysed.df)
summary(paralysed.glm)

paralysed .glm2 <- glm.nb(Laid ~ Species, data = paralysed.df)
paralysed.pred <- predict(paralysed.glm2, type = "response", se = TRUE)

## Tests on paralysed.glm2's residuals
c(theta = paralysed.glm2$theta, SE = paralysed.glm2$SE)
rs <- resid(paralysed.glm2, type = "deviance")
plot(predict(paralysed.glm2),  rs, xlab= "Linear predictors", ylab =
     "Deviance residuals")
abline(h = 0, lty = 2)
qqnorm(rs, ylab = "Deviance residuals")
qqline(rs)
## </end> residual tests


paralysed.df$Fit <- paralysed.pred$fit
paralysed.df$Fit.se <- paralysed.pred$se.fit
coefficients(summary(paralysed.glm2))
summary(paralysed.glm2)$df


Paralysed.df$t.val <- round(coefficients(summary(paralysed.glm2))[,3], 4)
Paralysed.df$p.val <- coefficients(summary(paralysed.glm2))[,4]
Paralysed.df$t.val[1] <- Paralysed.df$p.val[1] <- NA




paralysed.fit.df <- unique(paralysed.df[c(1,5:6)]) ## Use this one for parasitized

## some of the above lines might be out of date.  Below worked at the time.

Paralysed.df <- paralysed.fit.df
ToExcel(c("Paralysed.df", "emergence.df", "sexRatio.df", "Size.df"),, out = "Ridens")



### Development
hostlong.df <- read.delim("HostLongevity.txt")
bwplot(Mated ~ Longevity | Diet, data = hostlong.df)
bwplot(Mated ~ Offspring | Diet, data = hostlong.df)
bwplot(Mated ~ Size| Diet, data = hostlong.df)


summary(lm(Longevity ~ Diet * Mated, data = hostlong.df)) # no mating effect
summary(lm(Longevity ~ Diet, data = hostlong.df))$coefficients

summary(lm(Offspring ~ Diet * Mated, data = hostlong.df)) # no mating effect
summary(lm(Offspring ~ Diet + Mated, data = hostlong.df)) # 
summary(lm(Offspring ~ Diet, data = hostlong.df))$coefficients # 


bwplot(Diet ~ Longevity | Mated, data = hostlong.df)
bwplot(Diet ~ Size | Mated, data = hostlong.df)
bwplot(Diet ~ Offspring | Mated, data = hostlong.df)
## does longevity effect fecundity?
xyplot(Offspring ~ Longevity | Diet, data = hostlong.df)

long.fec.lm <- lm(Offspring ~ Diet/ Longevity, data = hostlong.df)
summary(long.fec.lm)



nonhostlong.df <- read.delim("NonHostLongevity.txt")

bwplot(Diet ~ Longevity, data = nonhostlong.df)
bwplot(Diet ~ Size, data = nonhostlong.df)

## does longevity improve with size?
xyplot(Longevity ~ Size | Diet, data = nonhostlong.df)
## linear model
nh.long.size.mat <- long.size(nonhostlong.df) # output matrix saved
## Effect of diet on longevity
nh.long.diet.mat <- long.diet(nonhostlong.df)

## Hosts
## Effect of diet and mating on longevity
h.long.diet.mat.p <- long.fec(hostlong.df)

aggregate(Longevity ~ Diet + Mated, data = hostlong.df, mean)
aggregate(Longevity ~ Diet + Mated, data = hostlong.df, sem)
require(reshape2)
h.long.diet.mat.mn <- dcast(aggregate(Longevity ~ Diet + Mated, data = hostlong.df, mean), ...~Mated)
names(h.long.diet.mat.mn)[2:3] <- ppaste("Mated:", names(h.long.diet.mat.mn)[2:3])

long.fec(hostlong.df)

## output from above functions put into the bottom of stats.blurb.

#### Examination and pre-contact times:
examine.df <- read.delim("examination.txt")
preContact.df <- read.delim("PreContact.txt")

examine.df$Species <- factor(sub("^+ ", "", as.character(examine.df$Species)))
preContact.df$Species <- factor(sub("^+ ", "",
                                    as.character(preContact.df$Species)))
bwplot(Species ~ Time| Lifestage, data = examine.df)
bwplot(Species ~ Time| Lifestage, data = preContact.df)
## looks like codling is a lot shorter pre-contact

##<don't redo>
examineOut.df <- observe("examine")
preContact.out.df <- observe("preContact")
##</don't redo>

## examine.df & preContact.df reread in and done slightly differently

examineOutB.df <- observeB("examine")
preContact.outB.df <- observeB("preContact")

## Sama as above except comparison is with Cydia pomonella (larvae)
examineOutC.df <- observeC("examine")
preContact.outC.df <- observeC("preContact")


###############################################################################
## 3/7/12 eventual response giving feedback on which comparisons

nh.long.diet.mat2 <- long.diet2(nonhostlong.df)

round(long.diet2(), 4)
round(long.size2(), 4)

long.fec2(hostlong.df) # a three part list somewhat redesigned

###############################################################################
## 20/12/12 Get Tukey groupings of examine and preContact 
examineOutD.df <- observeD("examine")
preContact.outD.df <- observeD("preContact")

## 12/2/2013 Do Fisher's Exact test on proportions of contact, etc.
percent.df <- read.delim("percent.txt")

no.df <- within(percent.df, {Sweep <- round(Sweep/100 * Mridens);
                             Tap <- round(Tap/100 * Mridens);
                             Ovipositor <- round(Ovipositor/100 * Mridens);
                             Terebra <- round(Terebra/100 * Mridens);
                             Oviposition<- round(Oviposition/100 * Mridens)
                             })
two.fish(one = "Argyroploce chlorosaris", two = "Cydia pomonella")
two.fish(one = "Ctenopseustis obliquana", two = "Cydia pomonella")
two.fish(one = "Cydia succedana", two = "Cydia pomonella")
two.fish(one = "Ctenopseustis obliquana", two = "Cydia succedana")
two.fish(one = "Ctenopseustis obliquana", two = "Grapholita molesta")
two.fish(one = "Ctenopseustis obliquana", two = "Planotortrix octo")
two.fish(one = "Grapholita molesta", two = "Planotortrix octo")

two.fish(one = "Argyroploce chlorosaris", two = "Planotortrix octo")
two.fish(one = "Argyroploce chlorosaris", two = "Ctenopseustis obliquana")
two.fish(one = "Argyroploce chlorosaris", two = "Cydia succedana")
two.fish(one = "Argyroploce chlorosaris", two = "Grapholita molesta")

two.fish(one = "Cydia succedana", two = "Grapholita molesta")


### Also ran 

observeB("examine") # and the like with browsers in to capture t-stats and df
## so there are now breaks in the running of those functions.


###############################################################################
###
###
###############################################################################
##
## Feb 2013: Development at various temperatures 
##
###############################################################################


devel.df <- read.delim("development.txt")
devel.df$TempNom <- factor(devel.df$TempNom)

bwplot(TempNom ~ Days | Sex, data = devel.df)
bwplot(Sex~ Days | TempNom , data = devel.df)
bwplot(Sex~ Length | TempNom , data = devel.df)

length.lm <- lm(Length~ TempNom + Sex, data = devel.df)
anova(length.lm)
anova(update(length.lm, . ~. -Sex))
new.df <- data.frame(TempNom = unique(devel.df[,1]))
predict(update(length.lm, . ~. -Sex), new.df)
length.df <- within(new.df, Length <- predict(update(length.lm, . ~. -Sex), new.df))
aggregate(Length~ TempNom, data = devel.df, FUN = mean)
aggregate(Length~ TempNom, data = devel.df, FUN = sem)
aggregate(Length~ TempNom, data = devel.df, FUN = length)

aggregate(Days~ TempNom, data = devel.df, FUN = mean)
aggregate(Days~ TempNom, data = devel.df, FUN = sem)
aggregate(Days~ TempNom, data = devel.df, FUN = length)


require(reshape2)
dcast(devel.df, Length~ TempNom, fun.aggregate = mean)

length.aov <- aov(Length~ TempNom, data = devel.df)
model.tables(length.aov, type = "means", se = TRUE)
summary(length.lm)

## non-linear fit (Lactin, et al model)
plot.develop(devel.df, rnd = 7) # Non-linearFit.pdf
## show below the x axis
plot.develop2(devel.df, rnd = 7) # Non-linearFit2.pdf 
## Logan model of same data
plot.developA(devel.df, rnd = 7) # Non-linearFitA.pdf 
options(digits = 10)



## with a Tmin with its own exponent multiplier and an intercept 
plot.developC2() # >> Non-linearFitC2.pdf
## with no intercept
plot.developC() # >> Non-linearFitC.pdf


#####
## Emerged
emerge.df <- read.delim("emerge.txt")
long.emerge.df <- melt(emerge.df, id = names(emerge.df)[1:3],
                       variable.name = "State", value.name = "Count")
long.emerge.df <- within(long.emerge.df, NomTemp <- factor(NomTemp))

bwplot(State~Count | TempRec, data = long.emerge.df)
bwplot(State~Count | NomTemp, data = long.emerge.df)
xyplot(jitter(Count) ~ jitter(TempRec) | State, data = long.emerge.df)
emerge.df <- within(emerge.df, NomTemp <- factor(NomTemp))
emerge.glms()




###############################################################################
## Check some plotmath stuff
pdf(file = "plotmath.pdf", family = "Bookman")
plot(1:10, 1:10, pch = "")
text(6, 9, expression(Temperature * degree ~ C))
text(6, 8.5, "expression(Temperature * degree ~ C)")
text(6, 7, substitute(paste(Temperature, B * degree, "C)"), list(B = " (")))
text(6, 6.5, 'substitute(paste(Temperature, B * degree, "C)"), list(B = " ("))')
text(6, 5, expression(Temperature~(degree*C)))
text(6, 4.5, 'expression(Temperature~(degree*C))')

text(6, 3, substitute(paste(B * degree, "C)"), list(B = " (")))
text(6, 2.5, expression((degree*C)))
dev.off()
