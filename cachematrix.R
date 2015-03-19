## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	 c <- NULL
  
  set <- function(y) {
    x <<- y
    c <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) c <<- inverse
  getinveserse <- function () c
  list( set = set, get = get,
        setinverse = setinverse, getinverse = getinverse)
  }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
         c <- x$getinverse()
  if(!is.null(c)){
    message("getting cached data")
    return(c)
  }
  data <- x$get()
  c <- inverse(data, ...)
  x$datainverse(c)
  c
}
