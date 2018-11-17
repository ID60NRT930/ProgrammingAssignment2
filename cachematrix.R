## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invM <<- inverse
  getInverse <- function() invM
  list(set = set, get = get,
       setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the matrix from the previous function and checks if the inverse matrix has value or not.
## If the inverse matrix has no value, it gets the matrix from the previous function and solves the inverse matrix.
## If the inverse matrix has value, it returns the cached data of the matrix.

cacheSolve <- function(x, ...) {
  invM <- x$getInverse()
  if(!is.null(invM)) {
    message("getting cached data")
    return(invM)
  }
  
  data <- x$get()
  invM <- solve(data, ...)
  x$setInverse(invM)
  return(invM)
}