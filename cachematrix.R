#Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.


#If you feed makeCacheMatrix a square matrix like the one commented out
#below it will create a special "Matrix" object which is really a list
#containing a function to:
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse
#4. get the value of the inverse

#squareMatrix <- matrix(c(9, 13, 5, 2, 1, 11, 7, 6, 3, 7, 4, 1, 6, 0, 7, 10), nrow = 4, ncol = 4)

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inverse <<- inverse
      getinverse <- function() inverse
      x <<- list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

#The following function calculates the inverse of the special "matrix" 
#created with the above function. However, it first checks to see if the 
#inverse has already been calculated. If so, it gets the inverse from the 
#cache and skips the computation. Otherwise, it calculates the inverse of 
#the data and sets the value of the inverse in the cache via the setinverse 
#function.

cacheSolve <- function(x, ...) {
      inverse <- x$getinverse()
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data, ...)
      x$setinverse(inverse)
      inverse
}
