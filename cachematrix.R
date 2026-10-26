## Creates a matrix object with functions to set and get the original
## matrix and to set and get its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(value) {
    inverse <<- value
  }
  
  getinverse <- function() inverse
  
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## Calculates the inverse of the special matrix object, or returns
## the cached inverse if it has already been calculated.

cacheSolve <- function(x, ...) {
  
  inverse <- x$getinverse()
  
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  
  inverse <- solve(data, ...)
  
  x$setinverse(inverse)
  
  inverse
}


