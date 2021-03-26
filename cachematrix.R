## makeCacheMatrix function will initialize the input value x and output value - inverse_matrix. 
## it will also store the value of input and output in parent environment.
## cacheSolve will take an input, verify if it is already cached by makeCacheMatrix function, 
## if not, it will calculate the inverse of the given matrix, store the results in 
## setinverse method defined in makeCacheMatrix and return the results.
## if object is already cached - it will return the output from the stored cache



## USAGE EXAMPLE
## x <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
## cacheSolve(x)
## cacheSolve(x)  --- you should see this result from cache object


## This function is used for caching the object and matrix value, usually getter 
## and setter methods
makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    set <- function(y) {
      x <<- y
      inverse_matrix <<- NULL
    }
    get <- function() {
      x
    }
    setinverse <- function(calculated_inverse) {
      inverse_matrix <<- calculated_inverse
    }
    getinverse <- function() {
      inverse_matrix
    }
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## This function will check if caching object already exists, if yes returns the 
## cached value using getinverse method, otherwise calculate new value, store in cache
## and return new value
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$getinverse()
    if(!is.null(inverse_matrix)) {
      message("getting cached data")
      return(inverse_matrix)
    }
    data <- x$get()
    calculated_inverse <- solve(data, ...)%%data
    x$setinverse(calculated_inverse)
    calculated_inverse
}
