## This function creates a "matrix" object.
## It can cache its inverse.
makeCacheMatrix <- function(x = numeric()) {

  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  get <- function(x)
  setsolve <- function(solve) m <<- solve
  getsolve <- function(m)
  list(set = set, get = get,
       setsolve = setmean,
       getsolve = getmean)
}

## This function computes the inverse of the "matrix".
## If the inverse has already been calculated and has not changed,
## the cachesolve should retrieve the inverse.
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setsolve(m)
  m
}