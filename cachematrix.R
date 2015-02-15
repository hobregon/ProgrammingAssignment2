## makeCacheMatrix: Takes a matrix and returns a list of functions
##                  to get/set it's value and get/set the value of 
##                  a solve function call (inverse matrix when called
##                  with a single arg).

## makeCacheMatrix follows the same logic as in the
## vector sample provided in the materials with a few changes.
## I modified the provided vector sample for the inverse matrix case
## and improved it so that it correctly handles changes
## in the ellipsis (...) arguments received in the call
## so that it doesn't return an invalid cached result when
## the ellipsis arguments change.

makeCacheMatrix <- function(x = matrix()) {
     ## sr stores the result of a called solve function
     sr <- NULL
     ##cachedcallargs is used to cache the args list 
     cachedcallargs <- list()
     set <- function(y) {
          x <<- y
          sr <<- NULL
          cachedccallargs <<- list()
     }
     get <- function() x
     ## these two functions set & get the inverse matrix from the cache
     setsolveresult <- function(solveresult, ...) {
          sr <<- solveresult
          cachedcallargs <<- list(...)
     }
     getsolveresult <- function(...){ 
          if( identical( cachedcallargs, list(...)))
               return(sr)
          else
               NULL
     }
     list(set = set, get = get,
          setsolveresult = setsolveresult,
          getsolveresult = getsolveresult)
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
     i <- x$getsolveresult(...)
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setsolveresult(i,...)
     i
     
}
