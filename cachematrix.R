## The constructor function `makeCacheMatrix` for making matrix is defined for creating a matrix object and caching its inverse.
## Call `cacheSolve` to get either the cached inverse or compute and cache the inverse

## Creating a matrix object
## `set`: set internal matrix
## `get`: get internal matrix
## `setinv`: set inverse matrix
## `getinv`: get inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <- i
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Solve the inverse matrix for the matrix object, x
## Return cached version if cache exists
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}
