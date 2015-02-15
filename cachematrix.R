## makeCacheMatrix: Takes a matrix and returns a list of functions
##                  to get/set it's value and get/set the value of 
##                  a solve function call (inverse matrix when called
##                  with a single arg).

## makeCacheMatrix follows the same logic as in the
## vector sample provided in the materials with a fw changes.
## I modified the provided vector sample for the inverse matrix case
## and improved it so that it correctly handles changes
## in the ellipsis (...) arguments received in the call
## so that it doesn't return an invalid cached result when
## the ellipsis arguments change.

makeCacheMatrix <- function(x = matrix()) {
     ## i stores the inverse matrix of x (after it's set in cacheSolve below)
     i <- NULL
     ##cachedcallargs is used to cache the args list 
     cachedcallargs <- list()
     set <- function(y) {
          x <<- y
          i <<- NULL
          cachedccallargs <<- list()
     }
     get <- function() x
     ## these two functions set & get the inverse matrix from the cache
     setinversematrix <- function(inversematrix, ...) {
          i <<- inversematrix
          cachedcallargs <<- list(...)
     }
     getinversematrix <- function(...){ 
          if( identical( cachedcallargs, list(...)))
               return(i)
          else
               NULL
     }
     list(set = set, get = get,
          setinversematrix = setinversematrix,
          getinversematrix = getinversematrix)
}


## cacheSolve:      Expects a makeCacheMatrix list and returns
##                  the output of the solve function.
##                  (inverse matrix when called
##                  with a single arg).
##                  It fetches the result from the
##                  cache if available and if the (...) 
##                  args did not change.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     i <- x$getinversematrix(...)
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinversematrix(i,...)
     i
     
}
