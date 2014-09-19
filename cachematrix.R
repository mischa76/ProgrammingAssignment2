## The following set of functions can be used to store a matrix,
## compute its inversion

## Function, `makeCacheMatrix` initializes a matrix and returns
## a list of functions, that can be used to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the matrix inversion
## 4.  get the value of the matrix inversion

makeCacheMatrix <- function(x = matrix()) {
       s <- NULL
     ## set matrix value, (re)setting cached inversion value to null
     set <- function(y) {
         x <<- y
         s <<- NULL
       }
     ## get matrix value
     get <- function() x
     ## set matrix inversion value
     setsolve <- function(solve) s <<- solve
     ## get matrix inversion value
     getsolve <- function() s
     ## return list of get/set-functions for setting matrix/inversion value
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}


## Function `cacheSolve` computes the inverse of the matrix object returned 
## by `makeCacheMatrix` above. If the inverse has already been calculated 
## (and the matrix has not changed), then `cacheSolve` retrieves the precomputed
## inverse from the cache.

cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        ## Lookup and return cahced value if available
         if(!is.null(s)) {
             message("getting cached data")
             return(s)
           }
        ## If not yet available then compute, cache and return inversion value
         data <- x$get()
         s <- solve(data, ...)
         x$setsolve(s)
         s
}
