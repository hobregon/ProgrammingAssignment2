## makeCacheMatrix: Takes a matrix and returns a list of functions
##                  to get/set it's value and get/set the value of 
##                  inverse matrix.

## cacheSolve:      Expects a makeCacheMatrix list and returns
##                  it's inverse. It fetches the inverse from the
##                  cache if available.

## makeCacheMatrix follows the same logic as in the
## vector sample provided in the materials
## I modified the sample simply for the inverse matrix case

makeCacheMatrix <- function(x = matrix()) {
     ## i stores the inverse matrix of x (after it's set in cacheSolve below)
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     ## these two functions set & get the inverse matrix from the cache
     setinversematrix <- function(inversematrix) i <<- inversematrix
     getinversematrix <- function() i
     list(set = set, get = get,
          setinversematrix = setinversematrix,
          getinversematrix = getinversematrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     i <- x$getinversematrix()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     ## Note that because of the ... optional arglist
     ## cacheSolve in fact could be used to cache any result
     ## of the R solve function, not only the case of a matrix inversion.
     ## This could create cunfusion for the user of this function.
     ## The same problem applies to the sample cacheMean function
     ## provided in the problem docs.
     i <- solve(data, ...)
     x$setinversematrix(i)
     i
     
}
