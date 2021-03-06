HostNohost <- function(xx = combined.df)
{
### Purpose:- 
### ----------------------------------------------------------------------
### Modified from:- 
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 13 Nov 2014, 14:58
### ----------------------------------------------------------------------
### Revisions:-

browser()

head(xx)
  tbl_df(xx) %>%
    group_by(Diet, Virgin, Hosts) %>%
      summarise_each(funs(mean, sem, N = length), Longevity)

diet.aov <- aov(Longevity ~ Diet * Virgin  * Hosts, data = xx)
  anova(diet.aov)

xx <- xx[!xx$Diet == "No diet or water",]
xxx <- xx[!xx$Diet %in% "Water only",]
  tbl_df(xxx) %>%
    group_by(Diet, Virgin, Hosts) %>%
      summarise_each(funs(mean, sem, N = length), Longevity)

diet.aov2 <- aov(Longevity ~ Diet * Virgin  * Hosts, data = xxx)
  anova(diet.aov2)

xxxH <- tbl_df(xxx) %>% select(Diet, Virgin, Hosts,Longevity) %>%
  filter( Hosts == "Hosts")
  diet.aov2H <- aov(Longevity ~ Diet * Virgin, data = xxxH)
  anova(diet.aov2H)
xxxH%>%
    group_by(Diet, Virgin, Hosts) %>%
      summarise_each(funs(mean, sem, N = length), Longevity)

xxxNH <- tbl_df(xxx) %>% select(Diet, Virgin, Hosts,Longevity) %>%
  filter( Hosts != "Hosts")
xxxNH%>%
    group_by(Diet, Virgin, Hosts) %>%
      summarise_each(funs(mean, sem, N = length), Longevity)

  diet.aov2NH <- aov(Longevity ~ Diet, data = xxxNH)
  anova(diet.aov2NH)

## Virgin only

xxxV <- tbl_df(xxx) %>% select(Diet, Virgin, Hosts,Longevity) %>%
  filter(Virgin  == "Virgin")

  diet.aov2V <- aov(Longevity ~ Diet * Hosts, data = xxxV)
  anova(diet.aov2V)

xxxV%>%
    group_by(Diet, Hosts) %>%
      summarise_each(funs(mean, sem, N = length), Longevity)



}
