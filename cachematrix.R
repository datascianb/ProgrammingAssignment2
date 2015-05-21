## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  
  
  ## changes matrix stored stored main function  
  set <- function(y) {
    ## substitues matrix x in main function with matrix y 
    x <<- y
    
    #value of inv restored to null each time
    inv <<- NULL
  }
  
  ## return matrix x
  get <- function() x
  
  ## store inverse 
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  ##s stores all above functions 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  ## verifies value of inv 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## if inv not in memory, fetches inv of matrix
  data <- x$get()
  
  ##using generalized inverse of matrix install package MASS 
  inv <- ginv(data, ...)
  
  x$setinverse(inv)
  inv
}
