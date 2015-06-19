## This file contains two functions, Together they calculate the inverse of a 
## square matrix and save it on cache. The first one stores and retrieves the 
## matrix on cache, and the second one calculates the value of the inverse 
## matrix. 

## makeCacheMatrix is a function that stores and retrieves the matrix on cache
 
makeCacheMatrix <- function(x = matrix()) {
  invmatx <- NULL
  set <- function(y) { #set the matrix: function that changes the stored matrix
    x <<- y
    invmatx <<- NULL
  }
 
  get <- function() # get the matrix: function that returns the stored matrix
    x 
  
  setinvmatx <- function(invers_matrix=matrix()) #set the inverse of matrix 
    invmatx <<- invers_matrix
  
  getinvmatx <- function() #get the inverse of matrix
    invmatx
  
  list(set = set, get = get, #list of functions
       setinvmatx = setinvmatx,
       getinvmatx = getinvmatx)
}

## cacheSolve is a function that calculates the value of the inverse matrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.
 

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
 
  invmatx <- x$getinvmatx()
 
  if(!is.null(invmatx)) {   
    message("getting cached data")
    return(invmatx) #result 
  }
  matx <- x$get() 
  
  invmatx <- solve(matx, ...) 
 
  x$setinvmatx(invmatx)
  
  invmatx #result 
}

