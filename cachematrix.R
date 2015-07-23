## Two functions allow the calculation of the inverse of a matrix to be cached
## so that the result can be retrieved on future calls

## Stores a matrix as an object and makes a list of functions to access it

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      get <- function() x
      setinv <- function(invrs)    inv <<- invrs
      getinv <- function() inv
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
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      y <- createIdMatrix(dim(x$get)[1])
      inv <- solve(x$get(), y, ...)
      x$setinv(inv)
      inv
}

createIdMatrix <- function(d){
      ##print(d)
      mx <- matrix(0, d, d)
      for(i in 1:d){
            mx[i,i] <- 1
      }
      return(mx)
}