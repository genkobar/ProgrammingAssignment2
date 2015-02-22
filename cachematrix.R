## These two functions offer a framework for a matrix object that can store the
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
  
  ## getter, returns matrix “x”
  get <- function() x

  ## inverse setter, stores value in “i”
  setinverse <- function(inverse) i <<- inverse

  ## inverse getter, returns “i”
  getinverse <- function() i

  ## This is the return value of the function makeCacheMatrix(),
  ## a list that contains the functions defined above.
  ## For example, with the command
  ##	x <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
  ## x$get() can be called to get the matrix and
  ## x$getinverse() can be called to get the inverse
  ## NB. the inverse will be NULL until cacheSolve is called with
  ## this return value as the input, or an inverse is manually set with
  ## x$setinverse().
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
## In short, it returns a matrix that is the inverse of ‘x’.

cacheSolve <- function(x, ...) {

  ## get current inverse value
  i <- x$getinverse()

  ## check if it is NULL
  if(!is.null(i)) {
    ## since it is not NULL, return this previously calculated value
    message("getting cached data")
    return(i) 
  }
  
  ## since we got to this point in the function, the inverse has
  ## not been calculated yet. 

  ## get matrix data
  data <- x$get()
  
  ## calculate inverse matrix
  i <- solve(data, ...)

  ## save result in matrix object
  x$setinverse(i)

  ## return inverse matrix
  i
}
