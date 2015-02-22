## These two functions offer a framework for a matrix that can store the
## value of its inverse, so that it doesn’t have to be calculated over and over.
## The first function, makeCacheMatrix() provides a scaffolding, while the
## second function, cacheSolve(), takes such a scaffolding as an input and
## either calculates its inverse, stores it in the input and then returns the
## value or, if it has previously been calculated, simply returns the value
## of the inverse from the input.

## makeCacheMatrix() takes a matrix as an input and returns a matrix
## object that can store the value of the inverse in a cached variable.
## The inverse variable “i” is set to NULL, and then general set/get functions
## are specified to set a new matrix or get the current one.
## If the set() function is used, the inverse variable “i” is reset to NULL.
## setinverse() and getinverse() are also specified to re

makeCacheMatrix <- function(x = matrix()) {
  ## declare i for inverse
  i <- NULL 

  ## set(y) stores a new matrix in the matrix object
  ## and resets “i”
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


## cacheSolve(x) calls the getinverse() function of the input object
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
