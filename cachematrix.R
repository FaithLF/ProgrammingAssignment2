


## makeCacheMatrix creats a speical 'matrix' object that can cache its inverse
## inv holds the value of the matrix inverse, after being initialized as NULL
## then the set function assigns new values in the parent environment
## the get function returns the value of the matrix argument
## setinverse assigns the value of inv in the parent environment
## getinverse gets the value of inv when called
## the final line allows the function to be accessed by the $ operator  
 makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y ) {
x <<- y
inv <<- NULL
}
get <- function() x
setInverse <- function(inverse) inv <<- inverse
getInverse<- function() inv
list(set = set, get = get,
setInverse = setInverse,
getInverse = getInverse)
}
 


## This function computes the inverse of the "matrix" from MakeCacheMatrix above.
cacheSolve <- function(x, ...) {
  inv  <-  x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data  <-  x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}













