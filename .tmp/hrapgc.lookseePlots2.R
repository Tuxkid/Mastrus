lookseePlots2 <- function()
{
### Purpose:- 
### ----------------------------------------------------------------------
### Modified from:- lookseePlots
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:-  4 Nov 2014, 14:16
### ----------------------------------------------------------------------
### Revisions:- 


trellis.device("pdf", file = "MastrusOverview2.pdf", height = 280/25.4,
               width = 180/25.4)
##
trellis.par.set(kullas())
remove.shading()
on.exit(dev.off())

## Tab Female longevity with no hosts
flnhPik <- bwplot(Diet ~ Longevity , data = FlongNohost.df,
       main = "Tab: Female longevity with no hosts")
flnhPik2 <- bwplot(Diet ~ Tibia , data = FlongNohost.df,
       main = "Tab: Female longevity with no hosts")

print(flnhPik, position = c(0, .5, 1, .9), more = TRUE)
print(flnhPik2, position = c(0, 0.1, 1, .5))


## Tab Female longevity with hosts
flhPik <- bwplot(Diet ~ Longevity | Virgin, data = FlongHost.df,
                 main = "Tab: Female longevity with hosts", layout = c(1,2))
flhPik2 <- bwplot(Virgin ~ Longevity | Diet, data = FlongHost.df,
                 main = "Tab: Female longevity with hosts", layout = c(1,3))
print(flhPik, position = c(0, .5, 1, .9), more = TRUE)
print(flhPik2, position = c(0, 0.1, 1, .5))

flhPik3 <- bwplot(Diet ~ Tibia | Virgin, data = FlongHost.df,
                 main = "Tab: Female longevity with hosts", layout = c(1,2))
flhPik4 <- bwplot(Virgin ~ Tibia | Diet, data = FlongHost.df,
                 main = "Tab: Female longevity with hosts", layout = c(1,3))

print(flhPik3, position = c(0, .5, 1, .9), more = TRUE)
print(flhPik4, position = c(0, 0.1, 1, .5))

fecHostPik3 <- bwplot(Diet ~ Fecundity | Virgin, data = FlongHost.df,
                      layout = c(1,2), main = "Tab: Female longevity with hosts")
fecHostPik4 <- bwplot(Virgin ~ Fecundity |Diet , data = FlongHost.df,
                      layout = c(1,3), main = "Tab: Female longevity with hosts")

print(fecHostPik3, position = c(0, .5, 1, .9), more = TRUE)
print(fecHostPik4, position = c(0, 0.1, 1, .5))

## male longevity
maleLong1 <- bwplot(Diet ~ Longevity, data = Mlong.df,
                    main = "Tab: Male longevity")
maleLong2 <- bwplot(Diet ~ Tibia, data = Mlong.df,
                    main = "Tab: Male longevity")
print(maleLong1, position = c(0, .5, 1, .9), more = TRUE)
print(maleLong2, position = c(0, 0.1, 1, .5))

## Offspring produced
pikDev1 <- bwplot(Diet ~ Development | Virgin + Sex, data = offspringNo.df,
                  main = "Tab: Offspring produced")
pikDev2 <- bwplot(Virgin ~ Development |Sex  + Diet, data = offspringNo.df,
                  main = "Tab: Offspring produced")
print(pikDev1, position = c(0, .5, 1, .9), more = TRUE)
print(pikDev2, position = c(0, 0.1, 1, .5))

pikSize1 <- bwplot(Diet ~ Size | Virgin + Sex, data = offspringNo.df,
                   main = "Tab: Offspring produced", layout = c(2,2))
pikSize2 <- bwplot(Virgin ~ Size |Sex  + Diet, data = offspringNo.df,
                   main = "Tab: Offspring produced")
print(pikSize1, position = c(0, .5, 1, .9), more = TRUE)
print(pikSize2, position = c(0, 0.1, 1, .5))


oldpar <- par(mfrow = c(2, 1), mai = c(35, 35, 25, 5)/25.4, cex.lab = 1.3)
## Tab: Hosts parasitised per day

with(parasitised.df1, sunflowerplot(Day, variable, Eggs, yaxt = "n",
                                    main = "Tab: Hosts parasitised per day",
                                    seg.lwd = .5, size = 2/25.4, ylab = ""))
axis(2, at = seq(10), labels = levels(parasitised.df1$variable),
     las = 2)
 dailyParasit <- sapply(parasitised.df, sum, na.rm = TRUE)[-1]
text(rep(1, 10), 1:10, dailyParasit, cex = .8, col = "blue")
## Tab: Offspring produced per day

with(offspring.df1, sunflowerplot(Day, variable, Eggs, yaxt = "n",
                                    main = "Tab: Offspring produced per day",
                                    seg.lwd = .5, size = 2/25.4, ylab = ""))
axis(2, at = seq(10), labels = levels(offspring.df1$variable),
     las = 2)
  dailyOff <- sapply(offspring.df, sum, na.rm = TRUE)
## extra Day column is a hassle
dailyOff <- dailyOff[jettison("Day", names(dailyOff))]
text(rep(1, 10), 1:10, dailyOff, cex = .8, col = "purple")

## Tab: Hosts parasitised per day (again)
plot.parasite(,.6, cex.lab = 1.3)
## Tab: Offspring produced per day (again)
plot.offspring(,.6, cex.lab = 1.3)

## Tab: Combined
par(oldpar)
pikComb1 <- bwplot(Diet ~ Longevity | Hosts + Virgin, data = combined.df,
                   main = "Tab: Combined")
pikComb2 <- bwplot(Virgin ~ Longevity | Hosts + Diet, data = combined.df,
                   main = "Tab: Combined")
pikComb3 <- bwplot(Hosts ~ Longevity | Virgin + Diet, data = combined.df,
                   main = "Tab: Combined")
pikComb4 <- bwplot(Hosts ~ Longevity | Diet + Virgin, data = combined.df,
                   main = "Tab: Combined")
print(pikComb1, position = c(0, .5, 1, 1), more = TRUE)
print(pikComb2, position = c(0, 0, 1, .5))
print(pikComb3, position = c(0, .5, 1, 1), more = TRUE)
print(pikComb4, position = c(0, 0, 1, .5))


  
}
