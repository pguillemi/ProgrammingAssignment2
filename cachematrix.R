## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

### This function makes an object matrix, with functions to get and 
### set its values

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #i stands for inverse
  
  set <- function(y) { #this is the matrix set function
    x <<- y
    i <<- NULL
  }
  
  get <- function() x # this gets the matrix in the object
  
  setinv <- function(inv) i <<- inv
  
  getinv <- function() i
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


### this function either solves the inverse of the matrix or  retrieves its value
### if it exists in cache


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
  ## Return a matrix that is the inverse of 'x' or caches the existent 
  ## one that was created by makeCacheMatrix
}