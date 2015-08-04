fish <-
function(vec = c(a = 31, b = 24, x = 19, y = 10))
{
### Purpose:- Does fisher test on 4 integers
###            x out of a c.f. y out of b
### ----------------------------------------------------------------------
### Modified from:- 
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 11 Feb 2013, 16:21
### ----------------------------------------------------------------------
### Revisions:- 6/3/13 Correction to calculation of matrix elements
  
a <- vec[1]
b <- vec[2]
x <- vec[3]
y <- vec[4]

##  round(fisher.test(cbind(c(a, a-x), c(b, b-y)))$p.value, 4)
round(fisher.test(cbind(c(x, a-x), c(y, b-y)))$p.value, 4)


}
