## Put comments here that give an overall description of what your
## functions do

## These functions calculate the inverse of a matrix by calculating the inverse
## and caching the result as well as returning the result.  When the
## calculation is done again, if the same matrix is provided as an input the
## results of the calculation already stored in the cache is returned rather
## than the result being re-run.  The first function places the raw data in a
## list with other functions; the second function takes this list either runs
## through the calculation or returns the already cached result as appropriate.

## Write a short comment describing this function

## This function takes a matrix and creates a list containing a function to 
## set the value of the matrix; get the value of the matrix; set the value of
## the inverse of the matrix; get the value of the inverse of the matrix.  The
## function also takes a null value, if a null matrix is input.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Write a short comment describing this function

## This function calculates the inverse matrix of the matrix prepared by the
## makeCacheMatrix function.  A check is first made to see if the inverse for
## that particular matrix has been calculated.  If the inverse has been calcu-
## lated, it is retrieved from the cache and the computation is skipped.  If 
## the inverse is not in the cache, the calculation is done, the result is put
## in the cache using the setmatrix function.  The inverse is also returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
