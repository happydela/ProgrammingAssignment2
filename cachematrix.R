## Create a pair of functions that cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse property
  i <- NULL
  
  ## Method to set the matrix
  set <- function( y ) {
    x <<- y
    i <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() x
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) i <<- inverse
  
  ## Method to get the inverse of the matrix
  getInverse <- function() i
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Compute the inverse of the special "matrix" created by "makeCacheMatrix". 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Return the inverse if its already there
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse
  m <- solve(data,...)
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}
