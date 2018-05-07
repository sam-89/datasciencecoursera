## functions that cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  
  ##set the matrix
  set <- function(matrix) {
    m <<- matrix
    inv <<- NULL
  }
  
  ## get the matrix
  
  get <- function() {
    m
  }
  
  ## set inverse of the matrix
  
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## get the invers of matrix
  getInverse <- function() {
    inv
  }
  
  ## get the value of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## return the inverse if its set already set
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## get matrix from objext
  data <- x$get()
  
  ## inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## set inverse to the object
  x$setInverse(m)
  
  ##return the matrix
  m
}
