maleLong <- function(xx = Mlong.df)
{
### Purpose:-   Male longevity tab
### ----------------------------------------------------------------------
### Modified from:- 
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 12 Nov 2014, 14:30
### ----------------------------------------------------------------------
### Revisions:- 


browser()
head(xx)
  tbl_df(xx) %>%
    group_by(Diet) %>%
      summarise_each(funs(mean, sem), Longevity)

diet.aov <- aov(Longevity ~ Diet, data = xx)
anova(diet.aov)

xxx <- xx[!xx$Diet %in% "Water only",]
 
diet.aov2 <- aov(Longevity ~ Diet, data = xxx)
anova(diet.aov2)


}
