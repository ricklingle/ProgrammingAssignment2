## this function creates a special matrix object that can cache
## its inverse

## created chache matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  minv <- function(solve) m <<- minv
  getminv <- function() m
  l<<-list(set = set, get = get,
       minv = minv,
       getminv = getminv)
}

## this function computes the inverse of a matrix returned by above
## makeCacheeMatrix.  If the inverse has already been calculated and
## matrix has not changed, then the cachesolve should retrieve the
## inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- l$getminv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- l$get()
  m <- solve(data, ...)
  l$minv(m)
  m
  }
