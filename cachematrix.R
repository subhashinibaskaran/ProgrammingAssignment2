## makeCacheMatrix and cacheSolve functions are created for Programming 
## assignment 2 for R Programmimg Course. 

## makeCacheMatrix Function creates a special "matrix" object that 
## can cache its inverse. It has four functions defined in it the 
## set function, the get function, the setinverse function and the 
## getinverse function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function(){
    x
  } 
  setinverse <- function(inverse){
    inv <<- inverse
  } 
  getinverse <- function(){
    inv
  } 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve Function computes the inverse of the invertible 
## "matrix" returned by makeCacheMatrix above. If the inverse of the  
## matrix is not available in Cache, then this function calculates the   
## inverse and returns it. If the data is already available in the   
## cache this function will return the cached data

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting the cached inverse for the matrix")
    return(inv)
  }
  data <- x$get()
  message("Calculating the inverse of the matrix for the first time")
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## TESTCASES USED
## **************

## > var1 = makeCacheMatrix(matrix(rnorm(1:4), 2, 2))
## > cacheSolve(var1)
## Calculating the inverse of the matrix for the first time
## [,1]        [,2]
## [1,] 0.5687696 -0.08771193
## > cacheSolve(var1)
## getting the cached inverse for the matrix
## [,1]        [,2]
## [1,] 0.5687696 -0.08771193
## [2,] 0.9922200  0.60399715

## > var2 = makeCacheMatrix(matrix(1:4, 2, 2))
## > cacheSolve(var2)
## Calculating the inverse of the matrix for the first time
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## [2,] 0.9922200  0.60399715
## > cacheSolve(var2)
## getting the cached inverse for the matrix
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5