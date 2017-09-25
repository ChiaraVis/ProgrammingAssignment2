# Matrix inversion may be a costly computation and so it is useful and efficient to cache the inverse 
# of a matrix, so that you do not have to compute it again and again. The following two functions serve 
# this purpose.

# makeCacheMatrix creates a list containing a function that:
# 1. sets the value of the matrix;
# 2. gets it;
# 3. sets the value of its inverse;
# 4. gets it.

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


# cacheSolve computes the inverse of the matrix returned by makeCacheMatrix. If the inverse has already 
# been calculated, and the matrix has not changed, it will take the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


