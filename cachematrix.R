## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- x
        mInverse <- NULL
        set <- function(y){
                m <<- y
                mInverse <<- NULL
        }
        get <- function() m
        
        setInverse <- function(inv) mInverse <<- inv
        getInverse <- function() mInverse
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        y <- x$getInverse()
        if(!is.null(y)){
                return(y)
        }
        z <- x$get()
        y <- solve(z, ...)
        x$setInverse(y)
        y
}
