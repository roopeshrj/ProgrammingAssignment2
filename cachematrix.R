## The functions will create a special matrix whose inverse will be calculated using the solve function. 
## Moreover, It will first check if the inverse has alrdy been calculated. 
## If yes, it will obtain the inverse from the Cachesolve function and skip the steps.
## If no, the inverse will be calculated and cached. 

## The makeCacheMatrix creates a special matrix which follows the steps:
## 1) It first sets the values of the matrix at the setmatrix step.
## 2) It obtains the matrix at the getmatrix step.
## 3) It sets the inverse of the matrix at setinverse step using solve function.
## 4) It finally obatins the inverse in the getinverse step.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  setmatrix<-function() y{
    x<<-y
    i<-NULL
  }
  getmatrix<- function() x
    setinverse<-function(solve) i<<-solve
  getinverse<-function() i
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This functions first checks of the inverse has already been calculated.
##  If yes, It is retrived from the cache.
## If no, It is calculated and the value is set in cache via the setinverse function.

cacheSolve <- function(x, ...) {
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$getmatrix
  i<-solve(data,...)
  x$setinverse(i)
  i
}
