## Put comments here that give an overall description of what your
## functions do
#makeCacheMatrix function takes matrix x as the argument.
#It then assigns null value to inv
#x and y are in different environments as x is an argument in the first function 
#and y is and argument of the second function.  The value of y is assigned to x.
#This sets the value of the matrix x.
#get<-function()x gets the value of the matrix x
#Next line of the code inverses the x matrix and assigns its value to inv in another environment. This caches the inverse value of matrix x.
#Next line of code gets the value of inv.
#The list is returned with set, get, setinverse and getinverse.
## Write a short comment describing this function
##

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function() inv <<- solve(x)
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function
#In this function the inverse of matrix x is assigned to inv.
#If there is a cached value is.null(inv) throws FALSE and hence !is.null(inv) throws TRUE.
#Thus the next code runs which returns the cached inverse value.
#If there is no cached value !is.null(inv) throws a FALSE which thus skips the next block of code.
#The next code to run hence is to get the value of matrix x and then it is inverses by solve function.
#The inversed value which is not from cache is then returned.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
