# There are two functions in this file, makeCacheMatrix and cacheSolve
# The functions caches the inverse of a matrix instead of having to repeatedly compute it.
# 
# 
# makeCacheMatrix Description:-
# This function creates a list of functions to set and get the cached value


makeCacheMatrix <- function(originalMatrix = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    originalMatrix <<- y
    inverseMatrix <<- NULL
  }
  get <- function() originalMatrix
  setinverse <- function(inverse) inverseMatrix <<- inverse
  getinverse <- function() inverseMatrix
  
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}



# cacheSolve Description:-
# This function returns the inverted form of the received matrix
# If the inverse has already been computed, it will return the inverse stored in cache


cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getinverse()
  
  # If the inverse is already calculated, return it
  if (!is.null(inverseMatrix)) {
    message("getting cached matrix")
    return(inverseMatrix)
  }
  
  # If the inverse matrix has not been calculated
  tempData <- x$get()
  inverseMatrix <- solve(tempData, ...)
  
  # Cache the inverse matrix
  x$setinverse(inverseMatrix)
  
  # Return it matrix
  inverseMatrix
}

# Testing

x <- matrix(rnorm(25), nrow = 5)          
createMatrix <- makeCacheMatrix(x)                  
createMatrix$get()                                  
cacheSolve(createMatrix)                            
cacheSolve(createMatrix)                            
 