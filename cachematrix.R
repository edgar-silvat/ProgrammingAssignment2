## The makeCacheMatrix function creates a list of functions and saved in its enviroment the values of x and m.
## The correct way to use makeCacheMatrix is to assigne it to another variable (e.g. MyMatrix<-makeCacheMatrix)
## The cacheSolve looks if there is a previous value in the cache and return it, if not it calcultes the value.


## The function makeCacheMatrix returns a list of functions and assign the values for x and m variables and are
## saved in this enviroment for future access. The functions inside are only created, none of them is executed.

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) m <<- inverse
   getinverse <- function() m
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## cacheSolve function take as argument the list created by the makeCacheMatrix function.
## First it look in the makeCacheMatrix enviroment to see if there is already a value m
## saved in cache. If so, it returns the value from the cache. Otherwise, it calculates 
## the inverse of the matrix saved in the x variable from makeCacheMatrix enviroment
## and assigned the value to m, which is saved in the cache and finally returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getinverse()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setinverse(m)
   m
}
