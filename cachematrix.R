## Description
### My functions will cache inverse of matrix
### Because of this function, when the Inverse data already calculated before,
### R don't calculate it again. Just get cache data
### It might be faster than normal calculation

##First function
###set the value of the matrix
###get the value of the matrix
###set the value of the Inverse
###get the value of the Inverse
makeCacheMatrix <- function(x = matrix()) {
  Inverse <- NULL
  set <- function(y){
    x <<- y
    Inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) Inverse <<- inv
  getInverse <- function() Inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##Second function 
### If the Inverse has been calculated before, the cached data is returned with the message
cacheSolve <- function(x, ...) {
  Inverse <- x$getInverse()
  if(!is.null(Inverse)){
    message("getting cached data")
    return(Inverse)
  }
  
  data <- x$get()
  Inverse <- solve(data, ...)
  x$setInverse(Inverse)
  return(Inverse)    
}

