## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      get <- function() x
      setinv <- function(invrs)    inv <<- invrs
      getinv <- function() m
      list(
        set = set, 
        get = get,
        setinv = setinv,
        getinv = getinv
      )
      
}


## Either gets inverse from makeCacheMatrix or calculates and stores

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getgetinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(inv)
      }
      inv <- solve(x$get(), 1, ...)
      x$setinv(inv)
      m
}
