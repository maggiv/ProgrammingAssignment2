makeCacheMatrix <- function(x = matrix()) 
  {
cmat <<- NULL
imat <<- NULL

set <- function(y)
{
  y <<- x
  cmat <<- y
}

get <- function(x)
{
  set(x)
  imat <<- solve(x)
}
get(x)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
  {
if (identical(x,cmat))
{
  message("retrieving from cache")
  imat
}
else
{
  solve(x)
}
}
