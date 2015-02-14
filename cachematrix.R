## Functions for calculating and caching the inverse of a square matrix

## makeCacheMatrix creates the object for caching the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  # function to set the value of the matrix   
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # function to get the value of the matrix 
  get <- function() x
  
  # function to set the inverse of the matrix 
  setinverse <- function(inverse) inv <<- inverse
  
  # function to get the inverse of the matrix 
  getinverse <- function() inv
  
  # return the functions for getting/setting the cached data 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve computes the inverse, or if it has already been calculated
# it is retrieved from the cache.

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x' 
  
  # Check if the inverse is already cached, return if it is  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # If not already cached, get the matrix and calculate the inverse  
  data <- x$get()
  inv <- solve(data, ...)
  # Store the inverse to the cache before returning
  x$setinverse(inv)
  inv
  
}
