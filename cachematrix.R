## Assignment: Caching the Inverse of a Matrix 
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

## Write the following functions:
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix())  {
  invert <- NULL
  
  set <- function(new_mat) {
    x <<- new_mat
    invert <<- NULL
  }
  
  get <- function() x
  sets_inv <- function(inverse) invert <<- inverse
  gets_inv <- function() invert
  list(sets_inv = sets_inv, gets_inv = gets_inv, set = set, get = get)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { invert <- x$gets_inv()
if(!is.null(invert)) {
  message("getting cached matrix")
  return(inv)
}
matr <- x$get()
invert <- solve(matr, ...)
x$sets_inv(invert)
invert
       
}
##Hope this works! -LK