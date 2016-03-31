##  A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

  inv <- NULL
  set <- function(y){
          x <<- y
          inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set,get = get,
        setInv = setInv, 
        getInv = getInv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if (!is.null(inv)){
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setInv(inv)
  inv
}
