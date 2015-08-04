FlongNohost <- function(xx = FlongNohost.df)
{
### Purpose:- Look at water/water+diet on Females, no hosts
### ----------------------------------------------------------------------
### Modified from:- 
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:-  6 Nov 2014, 15:45
### ----------------------------------------------------------------------
### Revisions:-

  water.levs <- levels(xx$Diet)[c(2, 4)]

  browser()
  water.aov <- aov(Longevity ~ Diet, data = xx[xx$Diet %in% water.levs,])
  anova(water.aov)


  tbl_df(xx) %>%
  group_by(Diet) %>%
    summarise_each(funs(mean, sem, N = length), Longevity)
  

  tbl_df(xx[xx$Diet %in% water.levs,]) %>%
  group_by(Diet) %>%
    summarise_each(funs(mean, sem, N = length), Longevity)

## tbl_df(xx[xx$Diet %in% water.levs,]) %>%
##   group_by(Diet, Virgin) %>%
##     summarise_each(funs(mean, sem, N = length), Tibia)


##   model.tables(water.aov, type =  "means", se = TRUE)
  
  honey.aov <- aov(Longevity ~ Diet, data = xx[!xx$Diet %in% water.levs,])
  xxx <- xx[!xx$Diet %in% water.levs,]
  xxx$Diet <- factor(xxx$Diet)
  honey.aov <- aov(Longevity ~ Diet, data = xxx)
  
  anova(honey.aov)

  tbl_df(xx[!xx$Diet %in% water.levs,]) %>%
  group_by(Diet) %>%
    summarise_each(funs(mean, sem, N = length), Longevity)


  
##   model.tables(honey.aov, type =  "means", se = TRUE)
##   se.contrast(honey.aov, list(xxx$Diet == "Honey agar diet and water",
##                               xxx$Diet == "Undiluted honey and water"))


##   tibia.aov <- aov(Tibia ~ Diet, data = xx)
##   anova(tibia.aov)
##   model.tables(tibia.aov, type =  "means", se = TRUE)
##   se.contrast(tibia.aov, list(xx$Diet == "Honey agar diet and water",
##                               xx$Diet == "No diet or water",
##                               xx$Diet == "Water only",
##                               xx$Diet == "Undiluted honey and water"))

  

  ## size to longevityy
  tib.long.lm <- lm(Longevity ~ Tibia, data = xx)
  anova(tib.long.lm)

}
