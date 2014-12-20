## Functions for creating and caching matrix data and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  invmatrix <- NULL
  
  ## set the value of the matrix
  set <- function(y) {
    
    x <<- y
    invmatrix <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the value of the inverse of the matrix
  setinverse <- function(inv) invmatrix <<- inverse
  
  ##get the value of the inverse of the matrix
  getintervse <- function() invmatrix
  
  ##return the list
  list(set = get, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

## The following function computes the inverse of the matrix with the created function above.
## First of all it checks whether there is a cache of the matrix and if its available; it returns the inverse which was stored as a cache. 
##If not, its computes an inverse, caches it and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ##fetch from cache
  invmarix <- x$getinverse()
  
  ##checking whether there is a chached matrix available
  if(!is.null(invmarix)) {
    message("getting cached data")
    
    ## return the inverse of the matrix as it is available
    return(invmatrix)
  }
  ##cache is empty, so fill it
  
  ##get the matrix
  data <- x$get() 
  
  ## compute the inverse of the matrix
  invmatrix <- solve(data, ...)
  
  ##cache the inverse of the matrix
  x$setinverse(invmatrix)
  
  ##return the inverse of the matrix
  invmatrix
}