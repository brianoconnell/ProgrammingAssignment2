## Creates a special "matrix" that can have its inverse cached.
## This can be used when the same matrix needs its inverse multiple times.

## Creates a list of functions that wrap functionality for caching the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Returns the cached inverse of the matrix or solves the inverse 
## if it has not already been solved and caches it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    return(inv)
  }
  
  ## Get the orginal matrix, solve the inverse and cache it.
  matrix <- x$get();
  inv <- solve(matrix)
  x$setInverse(inv)
  inv
}