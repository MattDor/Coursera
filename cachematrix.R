makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setinv <- function(solve) invMatrix <<- solve
  getinv <- function() invMatrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}

cacheSolve <- function(x, ...) {
  invMatrix <- x$getinv()
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  message("getting uncached data")
  data <- x$get()
  invMatrix <- solve(data,...)
  x$setinv(invMatrix)
  invMatrix
}
