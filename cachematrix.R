# makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
# Returns a list containing functions to:
# - set the value of the matrix
# - get the value of the matrix
# - set the value of the inverse
# - get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the cached inverse when the matrix changes
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse is already calculated (and the matrix unchanged), retrieves it from the cache.
# Otherwise, computes the inverse using solve() and caches it.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
