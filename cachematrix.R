#Write the following functions:

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

#Computing the inverse of a square matrix can be done with the solve function in R. 
#For example, if X is a square invertible matrix, then solve(X) returns its inverse.
#For this assignment, assume that the matrix supplied is always invertible.




makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  
  
  set <- function(y) {     #Call this function to reassign the variable passed to makeCacheMatrix()
    x <<- y                #<<- Allows the assignment of variable x which is outside of this environment. 1UP
    m <<- NULL
  }
  get <- function() x      #Returns the matrix
  
  setInverse <- function(solve) x 
  getInverse <- solve(x)
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


cacheSolve <- function(x, ...){
  m <- x$makeCacheMatrix()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  #If not cached matrix, manually solve
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
  
}
