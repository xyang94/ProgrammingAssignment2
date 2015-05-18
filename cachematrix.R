## The functions below calculate the inverse of an input matrix which 
## uses the cache method to avoid repetitive computation of the matrix 
## inverse.

## This function defines a set of four functions to help to cache the 
## matrix inverse. It returns a list of functions.  

makeCacheMatrix <- function(m = matrix()) {
    M<-NULL
    set<-function(y)
    {
      m<<-y
      M<<-NULL
    }
    get<-function() m
    setinverse<-function(inv) M<<-inv
    getinverse<-function() M
    list(set=set, get=get, setinverse=setinverse,
         getinverse=getinverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix
## above. The cache will be checked first in order to avoid repetitive calculation.

cacheSolve <- function(x, ...) {
  M<-x$getinverse()
    if(!is.null(M)) {
       message("getting cached data")
       return(M)
     }
  data <- x$get()
  M <- solve(data, ...)
  x$setinverse(M)
  M
        ## Return a matrix that is the inverse of 'x'
}
