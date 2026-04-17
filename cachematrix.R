## These functions enable the user to create a matrix whose inverse can be
## stored for later use upon calculation to enhance performance.

## Returns a representation of a matrix and its inverse with get/set functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

  get <- function() {x}
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  getInv <- function() {inv}
  setInv <- function(i) {inv <<- i}

  list(get = get, set = set, getInv = getInv, setInv = setInv)
}


## Gets the inverse of the matrix if already cached, otherwise computes and caches it

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(is.null(inv)) {
    inv <- solve(x$get(), ...)
    x$setInv(inv)
  } else {
    message('Getting cached data...')
  }
        ## Return a matrix that is the inverse of 'x'
  inv
}
