## The purpose of this program is to inverse a matrix, but to cache that process so that
## the programamatically costly call of solve() is not called more than it is needed to

## Creates a special "matrix" object that can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      if (is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)) {
        x <<- y
        i <<- NULL
      }
    }
    get <- function() x
    setInverse <- function(matrixInverse) i <<- matrixInverse
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes the inverse of the special "matrix" created by makeCacheMatrix().  If the inverse 
## has already been calculated (and the matrix has not changed), then this function will
## retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if (!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    mat <- x$get()
    i <- solve(mat)
    x$setInverse(i)
    i
}
