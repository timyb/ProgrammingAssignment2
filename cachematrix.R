## This function creates a special "matrix" object that can be used to cache its 
## inverse


## It can cache the inverse of a matrix (if the matrix is already there)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    Set <- function (y) {
        x <<- y
        inv <<- NULL
    }
    Get <- function() x
    SetInverse <- function(Inverse) inv <<- Inverse
    GetInverse <- function() inv
    list (Set=Set,Get=Get,SetInverse=SetInverse,GetInverse=GetInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix, and retrieve the inverse from cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$GetInverse()
        if(!is.null(inv)){
           message('Getting your inversed matrix!')
           return(inv)
        }
        data <- x$Get()
        inv <- solve(data,...)
        x$SetInverse(inv)
        inv
}
