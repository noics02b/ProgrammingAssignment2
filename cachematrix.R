## Assignment #2 by Winston A. Sumalia

## The makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## subfunctions:
## a.) set  - redefine the elements of the matrix
## b.) get - passes on the elements of the matrix as defined in the makeCacheMatrix function
## c.) setinv - writes into cache the inverted matrix values, either directly through makeCacheMatrix function or as computed (solve) through the CacheSolve function.
## d.) getinv - passes on the elements of the inverted matrix as defined in makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x                   ##Passes on the matrix elements as defined
  setinv <- function(inv) m <<- inv     ##writes into cache the elements of the inverted matrix
  getinv <- function() m                ##Passes on the elements of the inverted matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function computes for the inverse returned from makeCacheMatrix. If the matrix inverse has already been calculated, then this function no longer recalculates and retrieves the inverse from cache instead.

cacheSolve <- function(x, ...) {
  m <- x$getinv()            ##check if there is already cached data and retrieves it.  Matrix inverse needs to be recalculated once input defined in set has been changed 
  if(!is.null(m)) {               
    message("getting cached data")
    return(m)
  }
  data <- x$get()            ##new input data detected; thus, being compiled for inverse matrix recalculation
  m <- solve(data, ...)      ## generic function that solves the inverse, in the form  a %*% x  = b
  x$setinv(m)                ##writes into cache the values of the inverse Matrix
  m
  
}
