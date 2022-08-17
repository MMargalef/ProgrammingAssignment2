##Two fuctions need to be created:
## Function 1: makeCacheMatrix will:
  ## set the value of the matrix -> set
  ## get the matrix -> get
  ## set the inverse of the matrix -> setinv
  ## get the inverse of the matrix -> getinv

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Function 2: cacheSolve will compute the inverse of the matrix created with makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
