## Programming Assignment 2 of R Programming course on Coursera.org 
## by Tatiana Kurilo

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  
  check <- function(y) {          ## tests wheather the matrix is invertible
    if (ncol(y) != nrow(y)) {
      message: "The matrix must be square"
      NA
      return()
    }
    
    if (det(y) == 0) {
      message: "The matrix must be invertible"
      NA
      return()
    }
    TRUE
  }
  
  if (is.null(check(x))) {
    NA
    return() ## exits function if the matrix is not appropriate
  }              
  
  set <- function(newMatrix) {
    if (is.null(check(newMatrix))) {
      NA
      return()  ## exits function if the matrix is not appropriate
    }       
    x <<- newMatrix
    invMatrix <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setInvMatrix <- function(newInvMatrix){
    invMatrix <<- newInvMatrix
  }
  
  getInvMatrix <- function(){
    invMatrix
  }
  
  list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}

## This function computes the inverse of a special "matrix" returned by makeCacheMatrix function. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  inv <- x$getInvMatrix()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## if there is no cached data:
  matrix <- x$get()
  
  inv <- solve(matrix)
  
  x$setInvMatrix(inv)
  
  inv   
}