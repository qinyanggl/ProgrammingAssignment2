## makeCacheMatrix will create a matrix object, then cacheSolve 
## will calculate the inverse of the matrix. If the matrix inverse 
## has already been calculated, it will find it in the cache 
## and return it, instead of calculating it again.

# makeCacheMatrix creates a list that contains functions to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function cacheSolve returns the inverse of the matrix.
## It first checks if the inverse has already been computed.
## If so, it gets the result and skips the computation. 
## If not, it computes the inverse, sets the value in the cache using
## setinverse function.

# This function cacheSolve assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
