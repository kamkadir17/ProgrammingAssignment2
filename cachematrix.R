## The file contain two functions which can cache the original and inverse of a matrix.
## 

## Function makeCacheMatrix is a special matrix which stored both the original matrix and inverse of that matrix.
## Sample method to invoke the functions:
## 1. create or get some square matrix: a <- matrix(1:4, nrow = 2, ncol = 2)
## 2. Create a function reference variable for the given special matrix: mat <- makeCacheMatrix()
## 3. Set the original matrix to the special matrix: mat$set(a)
## 4. Calculate inverse of the matrix and cache:  cacheSolve(mat)
## 5. Calculate inverse of the matrix and cache:  cacheSolve(mat). This time, you should see "getting from cached" message

makeCacheMatrix <- function(x = matrix()) {
  #set the cached inverse variable to NULL
  inv <- NULL
  #set the initial matrix for this special matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #get the matrix itself
  get <- function() x
  
  #set the inverse matrix for the given matrix
  setInverse <- function(inverse) inv <<- inverse
  
  #get the inverse matrix
  getInverse <- function() inv
  
  # Return this "special" matrix
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function retrieves the inverse of the given matrix
## either from the cache if it is already available 
## or calculates it on stores in the cache for future use and as well returns the inverse to the caller

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Retrieve the inverse from the cache
  inv <- x$getInverse()
  
  ## If cache already has inverse, return it to the caller and end
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Proceed when inverse is not stored in the cache
  ## get the original matrix from the special matrix 
  mt <- x$get()
  
  ## find the inverse of the given matrix
  inv <- solve(mt)
  
  ## store the inverse onto the cache for future reference
  x$setInverse(inv)
  
  ## return the inverse matrix 
  inv
}
