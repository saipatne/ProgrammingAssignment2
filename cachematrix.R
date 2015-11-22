
## This function creates a matrix vector "makeCacheMatrix" that can cache its inverse.
## using a list that has functions
## get- to get the matrix, set the matrix, getinv- get the inverse matrix 
## and setinv-sets the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
xinv <- NULL ## Inverse matrix
set <- function(y) {
  x <<- y
  xinv <<- NULL
}
get <- function() x 
setinv <- function(inverse) xinv <<- inverse
getinv <- function()xinv
list( set = set, get = get,
     setinv = setinv,
     getinv = getinv )
}

## The following function calculates the inverse of the matrix
## created with the above function. However, it first checks to see 
## if the inverse matrix has already been calculated. 
## If so, it gets inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix 
## and sets the inverse matrix in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  xinv<- x$getinv()
  if (!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
    data <- x$get() 
    xinv <- solve(data,...) 
    x$setinv(xinv)
    xinv
  }
  

