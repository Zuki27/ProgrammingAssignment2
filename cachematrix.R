## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function will create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  sinverse <- function(inverse)i <<- inverse
  ginverse <- function()i
  list(set = set,
       get = get,
       sinverse = sinverse,
       ginverse = ginverse)

}


## Write a short comment describing this function

# This function will compute the inverse of the special matrix that is returned by the makeCacheMatrix function. If the inverse has already been calculated then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$ginverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$sinverse(i)
  i
        
}
