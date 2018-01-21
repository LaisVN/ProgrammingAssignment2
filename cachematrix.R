
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# 1) Set the matrix value
# 2) Get the matrix value
# 3) Set the inverse matrix value
# 4) Get the inverse matrix value
makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL
         set <- function(y) {
                   x <<- y
                   inv <<- NULL
                  }
         get <- function() x
         setinverse <- function(inverse) inv <<- inverse
         getinverse <- function() inv 
         list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function
# This next function will check is the matrix inverse is computed and if itis, it will get the result and won't do the computation again.
# If it is not, it will compute the matrix inverse, and use setinverse to record value in cache
cacheSolve <- function(x, ...) {
         inv <- x$getinverse()
         if(!is.null(inv)) {
                   message ("Getting data in cache")
                   return (inv)
                  } 
         data <- x$get()
         inv <- solve(data)
         x$setinverse(inv)
         inv
}
