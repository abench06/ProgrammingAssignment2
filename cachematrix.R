

## the below functions create an object that stores a matrix and caches its inverse.

## This function creates a matrix object able to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the matrix object 'makeCacheMatrix'
## If the inverse has already been calculated (and the 
## matrix has not changed), then it  retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

####Testing#####
my_matrix <- makeCacheMatrix(matrix(1:3, 1, 1))

cacheSolve(my_matrix)
