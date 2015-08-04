tuk.groups <-
structure(function(xx, y, sing.top.ok = TRUE, print.mat = FALSE, conf = 0.95,
           full.names = FALSE, print.grid = FALSE, print.sim.df = FALSE)
{
### Purpose: Get Tukey HSD groupings from an aov
### ----------------------------------------------------------------------
### Modified from: Various -- terminology is from palatability of plants
### ----------------------------------------------------------------------
### Arguments: sing.top.ok Sometimes need to fudge last value (Why??)
###            not entirely trustable -- use with caution
###            can't handle factor levels containing a '-' character
###            xx: an anova object
###            y: which factor are we interested in?
###            sing.top.ok: Is it OK to have a singular top value? (fudge?)
###            full.names: long names will maker colnames too long
###            print.mat:  Do we want to see the matrix with astrixes?
###            print.grid: Do we want the table printed with grids?
###            print.sim.df: Print the dataframe with levels that
###                          do not have an HSD with some other?
### ----------------------------------------------------------------------
### Author: Patrick Connolly, Creation date: 20 Aug 2003, 14:07

  xx.tuk <- TukeyHSD(xx, ordered = T, conf.level = conf)
  tuk.df <- as.data.frame(xx.tuk[[y]])
  tuk.df$HSD[tuk.df$lwr < 0 ] <- "*"
  ## Get order of most palatable host
  palatable <- names(sort(model.tables(xx, type = "means")$tables[[y]]))
  plants <- rownames(tuk.df)
  plants.list <- strsplit(plants, "-")
  tuk.df$Plant1 <- unlist(lapply(plants.list, "[", 2))
  tuk.df$Plant2 <- unlist(lapply(plants.list, "[", 1))
  same.groups <- list()
  tuk.same <- tuk.df[tuk.df$lwr < 0 , ]
  tuk.diff <- tuk.df[tuk.df$lwr >= 0 , ]
  if(print.sim.df)
    print(tuk.same)
  if(print.mat){ ## Print similarity matrix
    diff.mat <- matrix("", nrow = length(palatable), ncol = length(palatable))
    dimnames(diff.mat) <- list(palatable, palatable)
    for(i in palatable){
      for(j in palatable){
        if(i != j ){
          first <- match(i, tuk.df$Plant1)
          second <- match(j, tuk.df$Plant2)
          whichrow <- paste(j, i, sep = "-")
          diff.mat[i, j] <- tuk.df[whichrow, ]$HSD
        }    
      }
    }
    diag(diff.mat) <- "·"
    diff.mat[is.na(diff.mat)] <- ""
    diff.df <- as.data.frame(t(diff.mat))
    if(!full.names)
      names(diff.df) <- LETTERS[seq(palatable)]
    if(print.grid)
      print.char.matrix(as.data.frame(diff.df), col.names = T)
    else print(as.data.frame(diff.df))
    cat("\n")
  }
  for(i in palatable){
    same.i1 <- tuk.same[tuk.same$Plant1 == i, "Plant2"]
    same.i2 <- tuk.same[tuk.same$Plant2 == i, "Plant1"]
    same.groups[[i]]$max <- same.groups[[i]]$min <- match(i, palatable)
    if(length(same.i1) > 0)
      same.groups[[i]]$max <- match(rev(same.i1)[1], palatable)
    if(length(same.i2) > 0)
      same.groups[[i]]$min <- match(same.i2, palatable)
  }
### Find corners in similarity matrix
  mins <- sapply(same.groups, function(x) x$min[1])
  maxs <- sapply(same.groups, function(x) x$max[1]) 
  low <- rep(seq(unique(mins)), table(mins))
                                        #
  if(!sing.top.ok){# might be inaccurate to stop singular top value
    if(rev(low)[1] != rev(low)[2]) 
      low[length(low)] <- low[length(low) - 1]
  }
  high <- rep(seq(unique(maxs)), table(maxs)) # same length as palatable
  if(length(low) != length(high))
    return()
  group <- list()
  for(i in seq(palatable))
    group[[palatable[i]]] <- paste(sort(unique(letters[seq(low[i], high[i])])),
                                   collapse = "")
### return a named vector
  groups <- sapply(group, function(x) x)
  groups
}
, comment = "18/03/2005")
