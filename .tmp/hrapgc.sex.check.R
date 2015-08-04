sex.check <- function(xx = sex.df)
{
### Purpose:- Checks if sex ratios are random
### ----------------------------------------------------------------------
### Modified from:- 
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 17 Jan 2012, 14:38
### ----------------------------------------------------------------------
### Revisions:- 21/3/13: redone to use Fisher's test, not prop.test

species <- rownames(xx)
  out.p <- numeric(length(species))
names(out.p) <- species
for(i in species){
  row.i <- unlist(xx[i,])
  fish.i <- c(sum(row.i), 100, xx[i,1], 50)
  test.i <- fish(fish.i)
  out.p[i] <- test.i
}

sex.df$`p-val` <- out.p
sex.df
}
