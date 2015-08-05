devel14.df <- read.delim("development14.txt")
devel14.df$TempNom <- factor(devel14.df$TempNom)

new14.df <- unique(devel14.df[, c("TempNom", "Sex")])

 plot.developB_N(devel14.df) ## > Non-linearFitB_N.pdf

plot.developC2_N(post = FALSE)
plot.developC2_N(post = TRUE) ## > Non-linearFitC2_N.pdf

## reducing the points used in linear fit
plot.developC2_Na(hilin = 22, post = FALSE)
plot.developC2_Na(hilin = 24, post = FALSE)


### Tried skew-normal
plot.developSN() # up the wrong tree completely


## Tab development time (and tibia length)
devel14.df$TempNom <- factor(devel14.df$TempNom)

days.lm14 <- lm(Days~ TempNom * Sex, data = devel14.df)
anova(days.lm14)
anova(update(days.lm14, . ~. -TempNom:Sex)) # no interaction

new14.df <- unique(devel14.df[, c("TempNom", "Sex")])
day.pred14 <- predict(days.lm14, new14.df, se.fit = TRUE)
new14.df$Days <- day.pred14$fit
new14.df$Days.sem <- day.pred14$se.fit

bwplot(TempNom ~ Days | Sex, data = devel14.df)
bwplot(Sex~ Days | TempNom , data = devel14.df)
bwplot(Sex~ Length | TempNom , data = devel14.df)
bwplot(Sex~ Length | TempNom , data = devel14.df, layout = c(1 ,8))

qplot(TempNom, Days, data = devel14.df, geom =  "boxplot")
last_plot() + coord_flip()
qplot(TempNom, Days, data = devel14.df, geom =  "boxplot", facets = Sex ~ .)
last_plot() + coord_flip()

qplot(TempNom, Length, data = devel14.df, geom =  "boxplot", facets = Sex ~ .)
qplot(TempNom, Length, data = devel14.df, geom =  "boxplot", facets = . ~Sex)
last_plot() + coord_flip()
qplot(Sex, Length, data = devel14.df, geom =  "boxplot", facets = TempNom ~ .)
last_plot() + coord_flip()
ggsave("Lengths.pdf", width = 190, height = 199, units = "mm")
ggsave("LengthsMF.pdf", width = 190, height = 199, units = "mm")


## require(reshape2)
## dcast(devel14.df, Length~ TempNom, fun.aggregate = mean)
length.lm14 <- lm(Length~ Sex*TempNom , data = devel14.df)
options(digits = 7)
lengthD14.aov <- aov(Length~ Sex*TempNom, data = devel14.df)
model.tables(lengthD14.aov, type = "means", se = TRUE)
summary(lengthD14.aov)


lengthD14.lm <- lm(Length~ Sex * TempNom, data = devel14.df)
anova(lengthD14.lm)
newL14.df <- data.frame(unique(devel14.df[, c(3, 1)]))
len.pred14 <- predict(lengthD14.lm,  newL14.df, se.fit = TRUE)
#len.pred14 <- predict(update(lengthD14.lm, . ~. -Sex), newL14.df, se.fit = TRUE)
lengthD14.df <- within(newL14.df, Length <- len.pred14$fit)
lengthD14.df <- within(lengthD14.df, SE <- len.pred14$se.fit)

require("multcompView")
lengthD14.df$Group <- multcompLetters(TukeyHSD(lengthD14.aov)$TempNom[,4])$
monospacedLetters[levels(lengthD14.df$TempNom)] # don't you believe it!

### Separating by sex Begin here: do days same way as length

## Tab development time (and tibia length)

## Tibia length
lengthD14.aovM <- aov(Length~ TempNom, data = devel14.df, subset = Sex == "M")
model.tables(lengthD14.aovM, type = "means", se = TRUE)
summary(lengthD14.aovM)
lengthD14.aovF <- aov(Length~ TempNom, data = devel14.df, subset = Sex == "F")
model.tables(lengthD14.aovF, type = "means", se = TRUE)
summary(lengthD14.aovF)

lengthD14.dfM <- lengthD14.dfF <- data.frame(TempNom = unique(devel14.df[, 1]))
len.pred14M <- predict(lengthD14.aovM, lengthD14.dfM, se.fit = TRUE)
len.pred14F <- predict(lengthD14.aovF, lengthD14.dfF, se.fit = TRUE)
lengthD14.dfM <- within(lengthD14.dfM, Length <- len.pred14M$fit)
lengthD14.dfM <- within(lengthD14.dfM, SE <- len.pred14M$se.fit)

lengthD14.dfF<- within(lengthD14.dfF, Length <- len.pred14F$fit)
lengthD14.dfF<- within(lengthD14.dfF, SE <- len.pred14F$se.fit)

lengthD14.dfF$Group <- multcompLetters(TukeyHSD(lengthD14.aovF)$TempNom[,4])$
monospacedLetters[levels(lengthD14.dfF$TempNom)] # nearly OK -- swap b and c.
lengthD14.dfM$Group <- multcompLetters(TukeyHSD(lengthD14.aovM)$TempNom[,4])$
monospacedLetters[levels(lengthD14.dfM$TempNom)] # OK

## Same idea with days of development

daysD14.aovM <- aov(Days~ TempNom, data = devel14.df, subset = Sex == "M")
model.tables(daysD14.aovM, type = "means", se = TRUE)
summary(daysD14.aovM)
daysD14.aovF <- aov(Days~ TempNom, data = devel14.df, subset = Sex == "F")
model.tables(daysD14.aovF, type = "means", se = TRUE)
summary(daysD14.aovF)

daysD14.dfM <- daysD14.dfF <- data.frame(TempNom = unique(devel14.df[, 1]))
day.pred14M <- predict(daysD14.aovM, daysD14.dfM, se.fit = TRUE)
day.pred14F <- predict(daysD14.aovF, daysD14.dfF, se.fit = TRUE)
daysD14.dfM <- within(daysD14.dfM, Days <- day.pred14M$fit)
daysD14.dfM <- within(daysD14.dfM, SE <- day.pred14M$se.fit)

daysD14.dfF<- within(daysD14.dfF, Days <- day.pred14F$fit)
daysD14.dfF<- within(daysD14.dfF, SE <- day.pred14F$se.fit)

daysD14.dfF$Group <- multcompLetters(TukeyHSD(daysD14.aovF)$TempNom[,4])$
monospacedLetters[levels(daysD14.dfF$TempNom)] # nearly OK: tinker with letters
daysD14.dfM$Group <- multcompLetters(TukeyHSD(daysD14.aovM)$TempNom[,4])$
monospacedLetters[levels(daysD14.dfM$TempNom)] # nearly OK: tinker with letters


## for comparison, what comes from simply aggregating it all
len.agg14.df <- aggregate(Length~ Sex + TempNom, data = devel14.df, FUN = mean)
len.agg14.df$SE <- aggregate(Length~ Sex + TempNom, data = devel14.df, FUN = sem)$Length
len.agg14.df$N <- aggregate(Length~ Sex + TempNom, data = devel14.df, FUN = length)$Length

aggregate(Days~ Sex + TempNom, data = devel14.df, FUN = mean)
aggregate(Days~ Sex + TempNom, data = devel14.df, FUN = sem)
aggregate(Days~ Sex + TempNom, data = devel14.df, FUN = length)$Days

## Tab: Development Rate
##########################################

## non-linear fit (Lactin, et al model)
plot.develop(devel14.df, rnd = 7) # Non-linearFit.pdf
## show below the x axis
plot.develop2(devel14.df, rnd = 7) # Non-linearFit2.pdf 
## Logan model of same data
plot.developA(devel14.df, rnd = 7) # Non-linearFitA.pdf 
options(digits = 10)

## with a Tmin with its own exponent multiplier and an intercept 
plot.developC2_N() # >> Non-linearFitC2_N.pdf
## then with no intercept
plot.developC() # >> Non-linearFitC.pdf (don't use)
####################################################################################


#####
## Tab: Emergence
##########################################
emerge14.df <- read.delim("emerge14.txt")
long.emerge14.df <- melt(emerge14.df, id = names(emerge14.df)[1:3],
                       variable.name = "State", value.name = "Count")
long.emerge14.df <- within(long.emerge14.df, NomTemp <- factor(NomTemp))

bwplot(State~Count | TempRec, data = long.emerge14.df)
bwplot(State~Count | NomTemp, data = long.emerge14.df)
xyplot(jitter(Count) ~ jitter(TempRec) | State, data = long.emerge14.df)
emerge14.df <- within(emerge14.df, NomTemp <- factor(NomTemp))
require(MASS)
emerge.glms14() # extract bits marked ~~~

## Tab Adult Emergence
##########################################
adultEmerg14.df <- read.delim("AdultEmergence14.txt")
### adultEmerg14.df <- within(adultEmerg14.df, Length <- Length/20) already done
adultEmerg14.df <- within(adultEmerg14.df, TempNom <- factor(TempNom))

bwplot(TempNom ~ Days | Sex, data = adultEmerg14.df)
bwplot(TempNom ~ Length | Sex, data = adultEmerg14.df)
bwplot(Sex ~ Days | TempNom, data = adultEmerg14.df)
bwplot(Sex ~ Length | TempNom , data = adultEmerg14.df)

### Tibia length
lengthE14.lm <- lm(Length ~ TempNom * Sex, data = adultEmerg14.df)
anova(lengthE14.lm)

lengthE14.aov <- aov(Length~ TempNom + Sex, data = adultEmerg14.df)
model.tables(lengthE14.aov, type = "means", se = TRUE)
summary(lengthE14.aov)

lengthE14.aovF <- aov(Length ~ TempNom, data = adultEmerg14.df, subset = Sex == "F")
lengthE14.aovM <- aov(Length ~ TempNom, data = adultEmerg14.df, subset = Sex == "M")

lengthE14.dfM <- lengthE14.dfF <- data.frame(TempNom = unique(adultEmerg14.df[, 1]))
options(digits = 7)
lengthE14.predF <- predict(lengthE14.aovF, lengthE14.dfF, se.fit = TRUE)
lengthE14.dfF$Length <- lengthE14.predF$fit
lengthE14.dfF$SE <- lengthE14.predF$se.fit
lengthE14.dfF$Group <- multcompLetters(TukeyHSD(lengthE14.aovF)$TempNom[,4])$
monospacedLetters[levels(adultEmerg14.df$TempNom)]

lengthE14.predM <- predict(lengthE14.aovM, lengthE14.dfM, se.fit = TRUE)
lengthE14.dfM$Length <- lengthE14.predM$fit
lengthE14.dfM$SE <- lengthE14.predM$se.fit
lengthE14.dfM$Group <- multcompLetters(TukeyHSD(lengthE14.aovM)$TempNom[,4])$
monospacedLetters[levels(adultEmerg14.df$TempNom)]

### Days to emergence


daysE14.lm <- lm(Days ~ TempNom * Sex, data = adultEmerg14.df)
anova(daysE14.lm)

daysE14.aov <- aov(Days~ TempNom + Sex, data = adultEmerg14.df)
model.tables(daysE14.aov, type = "means", se = TRUE)
summary(daysE14.aov)

daysE14.aovF <- aov(Days ~ TempNom, data = adultEmerg14.df, subset = Sex == "F")
daysE14.aovM <- aov(Days ~ TempNom, data = adultEmerg14.df, subset = Sex == "M")

daysE14.dfM <- daysE14.dfF <- data.frame(TempNom = unique(adultEmerg14.df[, 1]))
options(digits = 7)
daysE14.predF <- predict(daysE14.aovF, daysE14.dfF, se.fit = TRUE)
daysE14.dfF$Days <- daysE14.predF$fit
daysE14.dfF$SE <- daysE14.predF$se.fit
daysE14.dfF$Group <- multcompLetters(TukeyHSD(daysE14.aovF)$TempNom[,4])$
monospacedLetters#[levels(adultEmerg14.df$TempNom)]

daysE14.predM <- predict(daysE14.aovM, daysE14.dfM, se.fit = TRUE)
daysE14.dfM$Days <- daysE14.predM$fit
daysE14.dfM$SE <- daysE14.predM$se.fit
daysE14.dfM$Group <- multcompLetters(TukeyHSD(daysE14.aovM)$TempNom[,4])$
monospacedLetters#[levels(adultEmerg14.df$TempNom)]


## See if we can get a piece as example for list
mm.aov <- aov(Days ~ TempNom, data = mm)
model.tables(mm.aov, "means")
multcompLetters(TukeyHSD(mm.aov)$TempNom[,4])


TT <- data.frame(T = c(model.tables(mm.aov, "means")[[1]]$TempNom))
TT$Group <- multcompLetters(TukeyHSD(mm.aov)$TempNom[,4])$monospacedLetters[rownames(TT)]
TT$GroupA <- multcompLetters(TukeyHSD(mm.aov)$TempNom[,4])$monospacedLetters


sessionInfo()




dput(mm, file = "TempDaysData")
summary(mm.aov)

TukeyHSD(mm.aov)$TempNom[,4]

##data from last year: use those plots and more analysis
tbl_df(FlongNohost.df) %>%
  group_by(Diet) %>%
  summarise_each(funs(min,max, mean), Longevity, Tibia)

tbl_df(FlongHost.df) %>%
  group_by(Diet) %>%
  summarise_each(funs(min, max, mean), Longevity, Tibia)

nohost.aov <- aov(Longevity ~ Diet, data = FlongNohost.df)
anova(nohost.aov)

### Look at water/water+diet on Females, no hosts

FlongNohost() # stop and look at browser.P1
FlongHost() # P2-4
maleLong() # P5
offspringNo() # P6-7
HostNohost() # P9-10  >>> Diets_mating_tests_Out.docx


###################################################################################
##
## Send to github
##
####################################################################

git config --global user.name "Tuxkid"
git config --global core.editor "emacs"

git init
git add .
git commit
git remote add origin https://github.com/Tuxkid/Mastrus.git 
git push -u origin master
####################################################################
