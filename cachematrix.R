## This set of functions can calculate the inverse of a given
## matrix and cache its value so that it can be easily retrieved.   
## This helps save time in case inversion is to be computed 
## multiple times on the same matrix.

## The makeCacheMatrix function returns a list consisting of
## four functions to set/get the value of the matrix and to
## set/get the value of its inverse. By using the set function
## values are cached in a different environment.

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) xinv <<- inverse
  getinv <- function() xinv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The cacheSolve function checks if the inverse has already
## been calculated for the matrix that it takes as an input.
## If so, the inverse matrix is retrieved from cache and returned
## along with a message saying from where the value was taken,
## otherwise it is calculated, stored in cache and then returned. 

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
