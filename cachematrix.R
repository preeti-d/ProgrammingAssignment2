## This function makeCacheMatrix requires an input argument of a matrix.
## The function computes the invert of the matrix. 
## Matrix conversion can be a costly operation.
## This function uses the "<<-" operator that can be used to cache the value of a variable.
## This function is is used to create a special object that stores a matrix and cahces its inverse.


## The function makeCacheMatrix creates a list that can be used to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
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


## The cacheSolve function can be used to return inverse of the matrix . Instead 
## of computing the inverse each time, the function first checks if the inverse
## already exists in the cache. Thus this saves time and improves performance. 
## This function uses solve to invert the matrix.
## NOTE: The function only works on SQUARE matrix.

##NOTE: The function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

## Sample run:
## > x = rbind(c(1, 2), c(2,1))
## > m = makeCacheMatrix(x)
## > m$get()
##      [,1] [,2]
## [1,]    1    2
## [2,]    2    1
## > m$getinverse()
## NULL
## > cacheSolve(m)
##            [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333
## > ## Retrieving from the cache in the second execution
## > cacheSolve(m)
## getting cached data.
##           [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333
 