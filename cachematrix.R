##  The following programme contains two functions which create an object to stores a matrix and cache its inverse.

## makeCacheMatrix function is a list containing a function to 
##1.set the value of the matrix
##2.get the value of the matrix
##3.set the value of the inverse
##4.get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)  
}


## The cacheSolve function calculates the inverse of the matrix object created with the first function. 
## Before calculating the inverse, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it solves the inverse of the matrix and sets the value of the inverse in the cache 
## via the setinv function.

cacheSolve <- function(x, ...) {
      i <- x$getinv()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i

}
