FlongHost <- function(xx = FlongHost.df)
{
### Purpose:- Look at honey and agar with hosts
### ----------------------------------------------------------------------
### Modified from:- FlongNohost
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:-  6 Nov 2014, 16:20
### ----------------------------------------------------------------------
### Revisions:- 

  browser()



  tbl_df(xx) %>%
    group_by(Diet, Virgin) %>%
      summarise_each(funs(mean, sem, N = length), Longevity)

  tbl_df(xx) %>%
    group_by(Diet) %>%
      summarise_each(funs(mean, sem, N = length), Longevity)


  diet.aov <- aov(Longevity ~ Diet * Virgin, data = xx)
  anova(diet.aov)

  model.tables(diet.aov, type =  "means", se = TRUE)
 diet.aov <- aov(Longevity ~ Diet + Virgin, data = xx)
  anova(diet.aov)
  model.tables(diet.aov, type =  "means", se = TRUE)
  
 diet.aov <- aov(Longevity ~ Diet, data = xx)
  anova(diet.aov)
  model.tables(diet.aov, type =  "means", se = TRUE)

  honey.aov <- aov(Longevity ~ Diet, data = xx[!xx$Diet %in% "Water only",])
xxx <- xx[!xx$Diet %in% "Water only",]
  anova(honey.aov)

  tbl_df(xxx) %>%
    group_by(Diet, Virgin) %>%
      summarise_each(funs(mean, sem, N = length), Fecundity)


  tbl_df(xx) %>%
    group_by(Diet, Virgin) %>%
      summarise_each(funs(mean, sem, N = length), Offspring)


## Offspring
  
  offs.aov <- aov(Offspring ~ Diet * Virgin, data = xx)
  anova(offs.aov)
  offs.aov <- aov(Offspring ~ Diet, data = xx)
  anova(offs.aov)
  offs.aov2 <- aov(Offspring ~ Diet, data = xxx)
  anova(offs.aov2)
  tbl_df(xx) %>%
    group_by(Diet) %>%
      summarise_each(funs(mean, sem, N = length), Offspring)


  fecund.aov <- aov(Fecundity ~ Diet * Virgin, data = xx)
  anova(fecund.aov)
  fecund.aov <- aov(Fecundity ~ Diet, data = xx)
  anova(fecund.aov)
  fecund.aov2 <- aov(Fecundity ~ Diet, data = xxx)
  anova(fecund.aov2)
  tbl_df(xx) %>%
    group_by(Diet) %>%
      summarise_each(funs(mean, sem, N = length), Fecundity)



  ## model.tables(fecund.aov2, type =  "means", se = TRUE)


  


  tbl_df(xx) %>%
    group_by(Diet, Virgin) %>%
      summarise_each(funs(mean, sem), Offspring)





  tibia.aov <- aov(Tibia ~ Diet, data = xx)
  anova(tibia.aov)
  model.tables(tibia.aov, type =  "means", se = TRUE)

  ## size to longevityy
  tib.long.lm <- lm(Longevity ~ Tibia, data = xx)
  anova(tib.long.lm)



  
}
