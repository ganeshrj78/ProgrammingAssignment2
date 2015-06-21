## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Following two functions are written to cache the inverse of a matrix.
## 1. makeCacheMatrix and 
## 2. cacheSolve. 
## makeCacheMatrix contains get & set matrix as well as get & set Inverse of a Input matrix

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve retuns the inverse of the matrix. 
## First checks if the matrix is inversed. 
##    a) If inversed, then it returns the data as it is. 
##    b) If not inversed, then it inverses and sets the values in cache. 
 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
          return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv		
}


## Test Runs:
##> x <- matrix(1:4, 2,2)
##> x
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> solve(x)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> m <- makeCacheMatrix(x)
##> m$get()
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cacheSolve(m)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(m)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5