## Put comments here that give an overall description of what your
## functions do


# makeCacheMatrix creates a list containing a function to, set the value of the matrix, get the value of the matrix
# set the value of inverse of the matrix, and get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix())
{
  inv <- NULL
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# cacheSolve returns the inverse of the matrix. It checks if the inverse has already been calculated 
# and if so, it returns the already computated inverse
# If the inverse has not been calculated, it computes the inverse and sets the value in the cache with the
# "setinverse" function.

cacheSolve <- function(x, ...) 
{
  inv <- x$getinverse()
  if(!is.null(inv)) 
  {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}