## Put comments here that give an overall description of what your
## functions do

# A simple cache structure, providing getters and
# setters for both the original matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  # Set the matrix
  set <- function (y) {
    x <<- y
    i <<- NULL
  }
  # Get the matrix
  get <- function () x

  # Set the cached inverse
  setinverse <- function (inverse) i <<- inverse

  # Get the cached inverse
  getinverse <- function () i

  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# CacheSolve provides an optimized implementation of
# the solve method.  Passing a cache matrix object,
# we check if the solve value has been computed prior
# to making the solve call.  We always return the solve
# value.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  # Since we've already computed the inverse,
  # we can return the cached value
  if (!is.null(inverse)) {
    message('returning cached inverse')
    return(inverse)
  }
  # We do not have a computed inverse,
  # so we need to compute it, then store it
  # in the cache and return it.
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setinverse(inverse)
  inverse
}
