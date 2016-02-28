## Cachematrix is a pair of functions (makeCacheMatrix and cacheSolve) 
## that cache the inverse of a matrix.
## The pair of functions create a special object that stores a matrix 
## and caches it matrix. The matrix must be a square invertible matrix.

## The makeCacheMatrix function creates a special Matrix which returns a 
## list of functions as follows:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  # sets and gets value of passed matrix 
  # computes, sets and gets value of Inverse of passed matrix
  #
  # Args:
  #   x: matrix() function
  #
  # Returns:
  #   list:set the value of the Matrix
  #        get the value of the Matrix
  #        set the value of the inverse Matrix
  #        get the value of the Inverse Matrix
  makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function (y) { 
      x <<- y
      m <<- NULL
    }
    get <-function() x 
    setmatrix <- function(solve) m<<- solve 
    getmatrix <- function() m
    # return the list of functions
    list (set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix) 
    
  }
  
  }


## the cacheSolve function calculates the inverse of the special "matrix"
## created with the makeCacheMatrix function. The cacheSolve function 
## first checks if the inverse of the matrix has already been calculated.
## If so, it gets the inverse of the matrix from the cache. If not, it 
## calculates the inverse of the matrix and sets the value of the 
## inverse in the setmatrix function

cacheSolve <- function(x, ...) {
  # gets inverse of matrix from cache or if inverse not in cache
  # calculates and caches the inverse matrix
  
  
  #
  # Args:
  #   x: special matrix() function
  #
  # Returns:
  #   m: matrix that is inverse of x

  m <- x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setmatrix(m)
  m      # Return a matrix that is the inverse of 'x'
}
