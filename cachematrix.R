##Create a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  set <- function(y) {
    x <<- y
    matrixInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) matrixInverse <<- inverse
  getInverse <- function() matrixInverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Computes the inverse of the matrix if
##i. The matrix has changed or
##ii. Matrix's inverse has not already been calculated.
##Else calculates inverse and caches(sets) it.
cacheSolve <- function(y, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrixInverse <- y$getInverse()
  if (!is.null(matrixInverse)) {
    message("Returned Cached Data as Inverse was already calculated")
    return(matrixInverse)
  }
  matrix <- y$get()
  matrixInverse <- solve(matrix, ...)
  y$setInverse(matrixInverse)
  matrixInverse
}