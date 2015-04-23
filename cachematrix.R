## These functions are intended to calculate the inverse of a matrix with 
## cacheing.  This file introduces a new datatype, CacheMatrix, who's 
## constructor can be called with makeCacheMatrix(x).  This datatype behaves the
## same as a regular matrix, with the exception that it automatically caches
## its inverse.  All other operations should be unaffected.

## makeCacheMatrix is a constructor function to create the new datatype 
## "cacheMatrix".  This function requires as input the matrix 'x', and returns a
## cacheMatrix containing the elements of 'x'
makeCacheMatrix <- function(x = matrix()) {
  ## Return a 'cacheMatrix' object
  
  cachedInverse <- NULL
  
  # set function: Stores matrix 'x'
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  } 
  
  # get function: returns matrix 'x'
  get <- function() x
  
  # Sets inverse of matrix 'x'
  setInverse <- function(inverse) cachedInverse <<- inverse
  
  # Gets cached copy of inverse of matrix 'x'
  getInverse <- function() cachedInverse
  
  list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
  
}


## cacheSolve attempts to return the cached inverse of a matrix.  If no cache
## exists, the inverse is calculated and cached for the next call.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Attempt to get the cached inverse
  cachedInverse <- x$getInverse()
  if (!is.null(cachedInverse)) {
    
    # Cached inverse exists
    return(cachedInverse)
  }
  
  # Cached inverse does not exist: Solve for it
  cachedInverse <- solve(x$get())
  
  # Cache inverse and return it.
  x$setInverse(cachedInverse)
  cachedInverse
  
}
