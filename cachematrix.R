## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
## 1)set the value of the vector
## 2) get the value of the vector
## 3)set the value of the mean
## 4)get the value of the mean


makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv_x <<- solve
  getinv <- function() inv_x
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## the function calculates the solve of the special matrix created with the 
## above function. 

cacheSolve <- function(x, ...) {
  inv_x <- x$getsolve()
  if(!is.null(inv_x)) {
    message("getting cached inverse matrix")
    return(inv_x)
  }
  matrix <- x$get()
  inv_x <- solve(matrix, ...)
  x$setsolve(inv_x)
  inv_x
}


