## A calculator for the inverse of a matrix which uses caching to save time and computational power.

## Initializes a matrix object in order to find its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
      base <- NULL
      func <- function(y) {
            x <<- y
            base <<- NULL
      }
      func1 <- function() x
      setbase <- function(inverse) base <<-inverse
      
      getbase <- function() base
      list(func = func, func1 = func1, setbase = setbase, getbase = getbase)
        
}

## Calculates the inverse of the matrix. If the inverse is already known and saved on the cache, that result is retrieved instead

cacheSolve <- function(x, ...) {
      base <- x$getbase()
      data <- x$func1()
      base <- solve(data, ...)
      x$setbase(base)
      base
        
}	
