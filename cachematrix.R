## function makeCacheMatrix() creates a special "matrix" object that can cache its inverse.
## There are several functions:
## 1. set -> set the value of the matrix
## 2. get -> get the value of the matrix
## 3. setinv -> set the inverse of the matrix
## 4. getinv -> get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  ## Return a matrix that is the inverse of 'x' if the inverse has already been calculated and cached.
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  ## Calculate the inverse of 'x' if the cached data doesn't exist.
  ## Package "MASS" required to compute inverse of matrix
  ## Run install.packages("MASS") and library(MASS) commands before running function
  data <- x$get()
  inv <- ginv(data)
  x$setinv(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}
