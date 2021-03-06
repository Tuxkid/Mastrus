multcompLetters <-
function(x, compare="<",
   threshold=0.05, Letters=c(letters, LETTERS, "."),
   reversed = FALSE){
##
## 1.  Covert to logical
##
  x.is <- deparse(substitute(x))
  if(class(x)=="dist")x <- as.matrix(x)  
  if(!is.logical(x))
    x <- do.call(compare, list(x, threshold))
##
## 2.  Create array of distinct pairs
##
  dimx <- dim(x)
  {
    if((length(dimx)==2) && (dimx[1]==dimx[2])){
      Lvls <- dimnames(x)[[1]]
      if(length(Lvls)!=dimx[1])
        stop("Names requred for ", x.is)
      else{
#       Create a matrix with 2 columns
#       with the names of all pairs         
        x2. <- t(outer(Lvls, Lvls, paste,
                     sep=""))
        x2.n <- outer(Lvls, Lvls,
           function(x1, x2)nchar(x2))
        x2.2 <- x2.[lower.tri(x2.)]
        x2.2n <- x2.n[lower.tri(x2.n)]
        x2a <- substring(x2.2, 1, x2.2n)
        x2b <- substring(x2.2, x2.2n+1)
        x2 <- cbind(x2a, x2b)
        x <- x[lower.tri(x)]        
      }
    }
    else{  
      namx <- names(x)
      if(length(namx)!=length(x))
        stop("Names required for ", x.is)
      x2 <- vec2mat2(namx)
      Lvls <- unique(as.vector(x2))
    }
  }
##
## 3.  Find the names of the levels 
##  
  n <- length(Lvls)
#   Generate an initial column
  LetMat <- array(TRUE, dim=c(n, 1),
               dimnames=list(Lvls, NULL))
##
## 4.  How many distinct pairs?  
##  
  k2 <- sum(x)
  if(k2==0){
    Ltrs <- rep(Letters[1], n)
    names(Ltrs) <- Lvls
    dimnames(LetMat)[[2]] <- Letters[1]
    return(list(Letters=Ltrs,
                LetterMatrix=LetMat))  
  }
##
## 4.  At last 2 levels are different: 
##     insert & absorb
##  
  distinct.pairs <- x2[x,,drop=FALSE]
  absorb <- function(A.){
#    Do the work in a recursive function:      
#    Delete any column for which the TRUE 
#    connections are a subset of another column
    k. <- dim(A.)[2]
    if(k.>1){ #i. <- 1; j. <- 2
      for(i. in 1:(k.-1))for(j. in (i.+1):k.){
        if(all(A.[A.[, j.], i.])){
#### drop a redundant column and recurse ###
          A. <- A.[, -j., drop=FALSE]
          return(absorb(A.))
        }
        else {
          if(all(A.[A.[, i.], j.])){
#### drop a redundant column and recurse ###
            A. <- A.[, -i., drop=FALSE]
            return(absorb(A.))
          }
        }          
      }
    }
#### end internal function absorb #######      
    A.
  }
# Now apply this function 
  for(i in 1:k2){ # i <- 1+i
#     Process the distinct differences one at a time       
#     Insert    i <- 1+i
#     Are (distinct) levels Td2[i, 1] and Td2[i,2]
#        connected in any columns of A?
    dpi <- distinct.pairs[i,]
    ijCols <- (LetMat[dpi[1],] & LetMat[dpi[2], ])
    if(any(ijCols)){
#     "Insert":  Break this connection 
      A1 <- LetMat[, ijCols, drop=FALSE]
      A1[dpi[1],] <- FALSE
      LetMat[dpi[2], ijCols] <- FALSE
      LetMat <- cbind(LetMat, A1)
#     Absorb   A. <- A
      LetMat <- absorb(LetMat)
    }
  }
##
## 5.  Sort the columns for visual appeal 
##  
  sortCols <- function(B){
    firstRow <- apply(B, 2, function(x)which(x)[1])
    B <- B[, order(firstRow)]
#     If ties, sort submatrices
    firstRow <- apply(B, 2, function(x)which(x)[1])
    reps <- (diff(firstRow)==0)
    if(any(reps)){
#     Break ties
      nrep <- table(which(reps))
      irep <- as.numeric(names(nrep))
      k <- dim(B)[1]
      for(i in irep){
        i. <- i:(i+nrep[as.character(i)])
        j. <- (firstRow[i]+1):k
        B[j., i.] <- sortCols(B[j., i., drop=FALSE])
      }
    }
#### end internal function sortCols #######      
    B
  }
  LetMat. <- sortCols(LetMat)
### Should the letters go in the reversed order?
  if(reversed) LetMat. <- LetMat.[ ,rev(1:ncol(LetMat.))]
# DON'T Sweep
    #...
##
## 6.  Create "Letters" for column names
##
  k.ltrs <- dim(LetMat.)[2]
  makeLtrs <- function(kl, ltrs=Letters){
    kL <- length(ltrs)
    if(kl<kL)return(ltrs[1:kl])
    ltrecurse <- c(paste(ltrs[kL], ltrs[-kL],
            sep=""), ltrs[kL])
    c(ltrs[-kL], makeLtrs(kl-kL+1,
                          ltrecurse))
  }
  Ltrs <- makeLtrs(k.ltrs, Letters)
  dimnames(LetMat.)[[2]] <- Ltrs
##
## 7.  Create simple summaries
##
  LetVec <- rep(NA, n)
  names(LetVec) <- Lvls
  for(i in 1:n)
    LetVec[i]<- paste(Ltrs[LetMat.[i, ]],
                    collapse="")
  nch.L <- nchar(Ltrs)
# To allow for multicharacter "Letters", create
# a vector of blanks with the right number
# of characters for each.  
  blk.L <- rep(NA, k.ltrs)
  for(i in 1:k.ltrs)
    blk.L[i] <- paste(rep(" ", nch.L[i]), collapse="")
# Now create monospacedLetters:    
  monoVec <- rep(NA, n)
  names(monoVec) <- Lvls
  for(j in 1:n){
    ch2 <- blk.L
    if(any(LetMat.[j,]))
      ch2[LetMat.[j,]] <- Ltrs[LetMat.[j,]]
    monoVec[j] <- paste(ch2, collapse="")
  }
##
## 8.  done
##
  InsertAbsorb <- list(Letters=LetVec,
        monospacedLetters=monoVec, 
        LetterMatrix=LetMat.)
  class(InsertAbsorb) <- "multcompLetters"
  InsertAbsorb  
}
