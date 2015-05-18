## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
