## This function establishes a defined matrix in a cache to shorten the future computations.

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set<- function(){
    x<<-y
    inv<<- NULL
  }
  get <- function(){x}
  setInverse<- function(inverse) {inv<<- inverse}
  getInverse<- function(){inv}
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## Returns a matrix that is the inverse of X

cacheSolve <- function(x, ...) {
       inv<- x$getInverse()
       if(!is.null(inv)){
         message("Getting cached data")
         return(inv)
       }
       mat<-x$get()
       inv<-solve(mat,...)
       x$setInverse(inv)
       inv
}
