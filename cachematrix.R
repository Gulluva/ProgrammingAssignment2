## Two functions allow the calculation of the inverse of a matrix to be cached
## so that the result can be retrieved on future calls

## Stores a matrix as an object and makes a list of functions to access it

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      ## change a value in matrix (to change in case not invertible)
      setnum <- function(n, r, c){

            if (r<= as.integer(dim(x)[1]) & c <= as.integer(dim(x)[2])){
                  ##print(x)
                  x[r,c] <<- n
                  inv <<- NULL
                  ##print(x)
            }
      }
      
      get <- function() x
      setinv <- function(invrs)    inv <<- invrs
      getinv <- function() inv
      list(
        set = set, 
        get = get,
        setinv = setinv,
        getinv = getinv, 
        setnum = setnum
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
      y <- diag(nrow = dim(x$get()))
      inv <- solve(x$get(), y, ...)
      x$setinv(inv)
      inv
}

