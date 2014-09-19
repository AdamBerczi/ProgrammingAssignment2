## The purpose for this is to create a pair of functions which enable us to cache the inverse of a matrix,
## so we don't need to caluclate it again

## The first function creates a special "matrix" object that can cache its own inverse.


makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
      set <- function(y) {
      	x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) i <<- solve
      getinverse <- function() i
      list(set = set, get = get,
      	setinverse = setinverse,
            getinverse = getinverse)
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
      if(!is.null(i)) {

		## If the inverse has already been calculated
		## (and the matrix has not changed),
		## then the cachesolve retrieves the inverse from the cache.

            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
	return(i)
}
