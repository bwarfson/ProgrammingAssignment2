## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Creates an opbect of type list that acts as a storage device.
## Stores the original matrix value and what will be the solve which is set to null
## This is essentially a getter setter to put it into OOP terms I am familier with
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL #set s = null 
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
## Accesses the makeCacheMatrix object (not the function). 
## Gets the value of the matrix used to create the object.  
## If solve has not been calculated then it calculates and stores it
## If it has been calculated it gets and returns the value
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

#m <- matrix(sample.int(100,size=9,replace=TRUE), nrow=3)
#m
#d <- makeCacheMatrix(m)
#cacheSolve(d)
#d$set(m)
#cacheSolve(d)
