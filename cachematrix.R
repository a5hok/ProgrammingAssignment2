## Matrix inversion is usually a costly computation and their may be some benefit to caching
## the inverse of a matrix rather than compute it repeatedly. The writepair of functions below
## create a special matric that can cache the inverse of a matrix, once it has been computed.

## This function creates a special "matrix" object that can cache its inverse

## Creates a function which a default null matrix argument
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the value of the inverse of the matrix to NULL
  matrixinverse <- NULL                     
  
  ## This function sets the value of the matrix and clears the cached inverse value which should be re-computed
  set <- function(y) {                      
    x <<- y
    ## Clear the value of inverse of the matrix in case the matrix was changed.
    matrixinverse <<- NULL             
  }
  
  ## Returns the value of the matrix
  get <- function() x                           
  
  # Calculates the inverse of non-singular matrix via the solve function and caches the result
  setinverse <- function(inverse) matrixinverse <<- inverse
  
  # Gets the cached result of the inverse of the matrix     
  getinverse <- function() matrixinverse        
  
  ## Passes the value of the function makeCacheMatrix        
  list(set = set, get = get,                    
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above and caches it. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  # Retrieve the cached value of the inverse of the matrix
  matrixinverse <- x$getinverse()
  
  # If the inverse is not NULL, return the cached value.
  if(!is.null(matrixinverse)) {                 
    message("getting cached data - Inverse of the matrix")
    return(matrixinverse)
  }
  
  # If the inverse is NULL, calculate it, cache and return the result
  matrix <- x$get()                               
  matrixinverse <- solve(matrix, ...)
  x$setinverse(matrixinverse)
  matrixinverse
}