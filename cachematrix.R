## makeCacheMatrix generates a matrix object, a list of get, set, getinverse, setinverse functions
## Caches the inverse upon first request, but deletes cache if matrix changes

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve returns the inverse of a matrix, the argument is a matrix object
## as generated from "makeCacheMatrix" (a list of functions)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  inv <- solve( x$get() )
  x$setinverse(inv)
  inv
}

  Example <- "
> mtx = makeCacheMatrix(matrix(Vectorize(runif)(9),3,3))
> mtx$get()
          [,1]      [,2]      [,3]
[1,] 0.1152455 0.1773401 0.6766049
[2,] 0.7328405 0.8989558 0.1334018
[3,] 0.7167812 0.8812230 0.6768583
> mtx$getinverse()
NULL
> cacheSolve(mtx)
> mtx$getinverse()
            [,1]       [,2]       [,3]
[1,] -36.4760048 -35.383509  43.436078
[2,]  29.7516042  30.239298 -35.700323
[3,]  -0.1070861  -1.898963   1.958726
> mtx$getinverse() %*% mtx$get()
              [,1]          [,2]          [,3]
[1,]  1.000000e+00 -7.105427e-15 -3.552714e-15
[2,]  3.552714e-15  1.000000e+00  0.000000e+00
[3,] -2.220446e-16  0.000000e+00  1.000000e+00

> mtx$get() %*% mtx$getinverse()
             [,1] [,2]          [,3]
[1,] 1.000000e+00    0 -6.938894e-18
[2,] 2.220446e-16    1  3.469447e-17
[3,] 0.000000e+00    0  1.000000e+00

"
