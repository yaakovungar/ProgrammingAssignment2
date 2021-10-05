## These two functions work in tandem to, first,create and store a matrix and, then,  
## calculate and store its inverse. The first function stores the matrix inverse and the 
## second function calculates the inverse only if it's not already stored in the first 
## function

## This function takes a matrix as it's argument and it creates four functions
## that are associated with the matrix.  One to set the matrix, one to return the matrix,
## one to set the inverse, and one to return the inverse. This function returns a list
## containing these four functions, which can then be utilized in the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMatrix <- function(y){
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function(){x}
  setInverse <- function(inverse){inv <<- inverse}
  getInverse <- function(){inv}
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, 
       getInverse = getInverse)
}


## This function takes the output of the makeCacheMatrix function as it's argument.
## It uses the getInverse function to see if the matrix inverse is already stored in the
## makeCacheMatrix function. If its stored there, it pulls the inverse from there and 
## returns it.  If it's not stored there, it calculates the inverse, caches it in the
## makeCacheMatrix function for later use, and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Getting Cached Data")
    return(inv)
  }
  mat <- x$getMatrix()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
