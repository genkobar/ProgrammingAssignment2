## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a matrix as an input and returns a matrix
## object that can store the value of the inverse in a cached variable.
## The inverse variable “i” is set to NULL, and then general set/get functions
## are specified to set a new matrix or get the current one.
## If the set() function is used, the inverse variable “i” is reset to NULL.
## setinverse() and getinverse() are also specified to re

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set  <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list (set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
        )
}


## cachesolve(x) calls the getinverse() function of the input object
## to retrieve the value of the inverse stored there.
## Then it checks to see if it is empty; if not, it returns the value
## otherwise it proceeds to call solve() on the matrix data to calculate 
## the inverse. Then it stores the result in the object, and then prints it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
