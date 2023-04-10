## The makeCacheMatrix function creates a special "matrix" object that 
## can cache its inverse.
## The cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
    }
  get<-function()x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
                    inver<-ginv(x)
                    inver%%x
  }
  list(set = set, get = get,
       setinv = setinv ,
       getinv = getinv)
}





cacheSolve <- function(x, ...) {
  }
 inv<-x$getinv()
 if(!is.null(inv)){
                  message("getting cached data")
                  return(inv)
 }
 data<-x$get()
 inv<-solve(data,...)
 x$setinv(inv)
 inv
}

B <- matrix(c(4,2,7,4),2,2)
B1 <- makeCacheMatrix(B)
cacheSolve(B1)
