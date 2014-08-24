## kevin wong

## makeCacheMatrix function creates an object which contains a matrix and its inverse
## makeCacheMatrix defines a getter/setter pair for each of its variables

## get: retrieves matrix of the object
## set: sets matrix of the object
## getInverse: retrieves inverse matrix of the object
## setInverse: sets inverse matrix of the object
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL ## default
  
  ## Sets matrix of object and resets the inverse matrix (because of new matrix)
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## retrieves matrix
  get <- function() x
  
  ## sets inverse matrix
  setInverse <- function(cSolve) inverse <<- cSolve
  
  ## returns inverse matrix
  getInverse <- function() inverse
  
  ## defines list of object functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Input: matrix x created by above makeCacheMatrix function
## Output: inverse of 'x' input
## Relies on getInverse getter to identify whether an inverse has been
## previously computed; if so, returns inverse from cache, else calculates with solve()
## and sets inverse with setInverse function
cacheSolve <- function(x, ...) { 
  ## Assumes input != NULL
  inverse <- x$getInverse()
  
  ## if inverse != null, we have cached data available
  if (!is.null(inverse)) {
    return(inverse)
  }
  
  ## otherwise, calculate inverse with solve()
  temp <- x$get()
  inverse <- solve(temp)
  x$setInverse(inverse)
  
  inverse
}
