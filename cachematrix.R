## Below are two functions. The first function, makeCacheMatrix creates a R object
## that stores a matrix and its inverse. The second function, cacheSolve, computes
## the inverse of the matrix returned by the first function.

## The function below creates a matrix and stores its inverse

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL     ## initialize objects
      set <- function(y) {
             x <<- y    ##assign input value of x
             s <<- NULL ##assign value of Null to s
      }
      get <- function() x    ##defines getter for matrix x
      setsolve <- function(solve) s <<- solve  ##defines setter for solve s
      getsolve <- function() s  ##defines getter for solve s
      list(set = set, get = get,  ##create list
           setsolve = setsolve,
           getsolve = getsolve)

}


## This function solves for the inverse of the matrix by getting the 
##information in the first function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
               message("getting cached data")
               return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
