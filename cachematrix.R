## Combination of functions to cache the inverse of a matrix, in order to circumvent
## a potentially lengthy calculation.  If the inverse of a particular matrix is required 
## mutliple times, only the first instance will require the explicit calculation.  Subsequent
## requests for the inverse will be read from the cache.

## makeCacheMatrix creates a list of functions which return a reference matrix, an input 
## matrix, a function to calculate the inverse of the input matrix, and the reference inverse
## makeCacheMatrix takes a matrix as a passed parameter

makeCacheMatrix <- function(x = matrix()) {
  inv <-NULL
  set <- function(y) {
    x<<-y
    inv<<-NULL
  }
  get <-function() x
  setinv<-function(solve) inv<<-solve
  getinv<-function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve takes a matrix as a passed parameter.  The inverse of the matrix is calculated
## if it has not been previously calculated (checked by if() statement).  If the inverse
## has been previously calculated, the inverse is read off the list/cache instead of being
## explicitly calculated.  The check only works for repeated calls with the same list name,
## not repeated calls of different lists (from makeCacheMatrix) that were created from the
## same matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv
}
