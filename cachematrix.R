## Make cache matrix provides a wrapper to R internal matrix that supports manual caching of 
## matrix inversion

## Provide a matrix to this wrapper class which can manually cache matrix inverse 
## and will invalidate cache when set is called

makeCacheMatrix <- function(matrixInternal = matrix()) {
  cachedInverse <- NULL
  set <- function(m) {
    matrixInternal <<- m
    cachedInverse <<- NULL
  }
  get <- function() {
    matrixInternal
  }
  setCachedInverse <- function (inverseOfMatrix) {
    cachedInverse <<- inverseOfMatrix
  }
  getCachedInverse <- function () {
    cachedInverse
  }
  list(get=get, set=set, setCachedInverse=setCachedInverse, getCachedInverse=getCachedInverse)
}

## provided a makeCacheMatrix, cacheSolve will return a cached inverse if it has been computed
## and if not will compute and cache using makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cachedInverse = x$getCachedInverse()
    if (!is.null(cachedInverse)) {
      return(cachedInverse)
    }
    inverse <- solve(x$get())
    x$setCachedInverse(inverse)
    inverse
}

#used for testing, creates a solvable n by n matrix
createSolveableMatrix <- function(n) {
  mat <- matrix(0, nrow=n, ncol=n)
  for (i in 1:n) {
    mat[i,i] = i
  }
  mat
}
