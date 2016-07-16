## The following function calculates the inverse of the special "matrix" created
## with an function. However, it first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the invers of the matrix and sets the value of the 
## matrix in the cache via the setmean function.


# makeCacheMatrix  creates a list containing a function to do :
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The following function returns the inverse of the matrix . First it checks if
# the inverse has already been computed. If true, it gets the result and skips the
# computation part. If not, it computes the inverse, sets this value in the cache using
# setinverse function.

# This function always assumes that the matrix is invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Test run:
## > x = rbind(c(2, -2/3), c(-2/3, 2))
## > m = makeCacheMatrix(x)
## > m$get()
## [,1]       [,2]
## [1,]  2.0000000 -0.6666667
## [2,] -0.6666667  2.0000000

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 0.5625 0.1875
## [2,] 0.1875 0.5625

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 0.5625 0.1875
## [2,] 0.1875 0.5625



