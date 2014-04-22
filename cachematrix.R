## The functions demonstrate the caching of the inverse of a matrix.
## Rather than computing the inverse repeatedly, the functions cache the
## inverse of a matrix

## The function creates a matrix object that can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setm <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getm <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(setm = setm, getm = getm,
       setinv = setinv,
       getinv = getinv)
}

## The function computes the inverse of the matrix 
## returned by the above makeCacheMatrix. If the inverse was already
## calculated (and the matrix has not changed), then then function retrieves
## the inverse from cache (memory).

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse of matrix")
    return(inv)
  }
  data <- x$getm()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

