## There are two functions; the second computes an inverse of a matrix, 
## and the first one caches the computation, so that it can be retrieved
## later in case the matrix value stays the same.

## This function calculates the inverse of a particular matrix and caches that value.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function retrieves and returns the cached matrix inverse, if it has been cached for 
## that matrix value; if not, it comsputes the matrix inverse and returns it.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("retrieving cached matrix inverse")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  }
}
