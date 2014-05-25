## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## set the value of the matrix and get the value of the matrix,
## set the value of the inverse matrix and get the value


makeCacheMatrix <- function(x = matrix()) {
  inverse_x <- NULL
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse_x <<- solve
  getinverse <- function() inverse_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## the function calculates the solve of the special matrix created with the 
## above function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## If the inverse has already has alreayd been calculated
        ## it should retrieve the inverse from the cache
  inverse_x <- x$getsolve()
  if(!is.null(inverse_x)) {
    message("getting cached inverse matrix")
    return(inverse_x)
  }
  matrix <- x$get()
  inverse_x <- solve(matrix, ...)
  x$setsolve(inverse_x)
  inverse_x
}
