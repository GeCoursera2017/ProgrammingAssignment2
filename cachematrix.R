## Put comments here that give an overall description of what your
## functions do

## This function makeCacheMatrix creates a list containing a function to 
#  1.Set the value of the matrix and its inverse is initialized to be NULL
#  2.Get the value of the matrix
#  3.Set the value of the inverse
#  4.Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  matrixInv <- NULL  		#Matrix inverse is initialized to be NULL
  set <- function(y) {
    x <<- y
    matrixInv <<- NULL
  }				#Set the value of the matrix and its inverse is initialized to be NULL
  get <- function() x           #Get the value of the matrix 
  setinverse <- function(inverse) matrixInv <<- inverse #Set the value of the inverse
  getinverse <- function() matrixInv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}				#Get the value of the inverse


## This function, cacheSolve, calculates the inverse of matrix created
#with the above function. It first checks to see if the inverse has 
#already been calculated. If so, it gets the inverse from the cache and 
#skips the computation. Otherwise, it calculates the inverse of the data and 
#sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrixInv <- x$getinverse()
  if(!is.null(matrixInv)) {
    message("getting cached data")
    return(matrixInv)
  }
  data <- x$get()
  matrixInv <- solve(data, ...)
  x$setinverse(matrixInv)
  matrixInv
}
