## Author: Farago Janos
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { #initial value of the matrix
    x <<- y
    inv <<- NULL #the matrix inverse is NULL at the initialisation
  }
  get <- function() x #getter for the values of the matrix
  setinv <- function(inverse) inv <<- inverse #setter for the cached inverse
  getinv <- function() inv #getter for the cached inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) #return the inverse matrix of 'x' from cached data
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
