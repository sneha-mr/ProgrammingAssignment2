## Pair of functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## Set the value of the matrix and clear the cached value
  ## Use <<- to assign to global environment (accessible outside of this function)
  set <- function(y) {
    x <<- y
    m <<- null
  }
  ## Get the value of the matrix
  get <- function() x
  ## Return the list of functions
  list(set = set, get = get)
}


## Computes the inverse of the special matrix from above
## Inverse is already calculated AND matrix same: should retrieve the cached value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Retrieve the matrix
  m <- x$get()
  ## Check if the inverse is already cached
  if (!is.null(m$inverse)) {
    message("Retrieving cached inverse")
    return(m$inverse)
  }
  ## If the inverse is not cached, calculate it
  else {
    m$inverse <- solve(m) # solve can compute the inverse of a square matrix
    return(m$inverse)
  }
  
}
