ditch <-
function(z = "A", x)
{
### Purpose:- Ditches columns z from dataframe x
### ----------------------------------------------------------------------
### Modified from:- jettison
### ----------------------------------------------------------------------
### Arguments:- z: vector of column names
###             x: dataframe
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 12 Nov 2012, 14:10
### ----------------------------------------------------------------------
### Revisions:-

  x[, jettison(z, names(x))] 
}
