## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # m is the actual matrix
        m <- x
        
        # mInverse will hold the inverse of the matrix
        mInverse <- NULL
        
        # set will set a new matrix to m 
        set <- function(y){
                m <<- y
                mInverse <<- NULL
        }
        
        # get will return the matrix
        get <- function() m
        
        # set Inverse will attribute the inverse to mInverse
        setInverse <- function(inv) mInverse <<- inv
        
        # get will return the inverse of the matrix
        getInverse <- function() mInverse
        
        # list of the function that manipulate the data
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # y will hold the inverse of the matrix
        y <- x$getInverse()
        
        # if the inverse exist so it's no nessery to calculate it
        if(!is.null(y)){
                return(y)
        }
        
        # z will hold the value of the matrix 
        z <- x$get()
        
        # y will hold the value of the inverse of z
        y <- solve(z, ...)
        
        # caching the inverse
        x$setInverse(y)
        
        # returning the inverse
        y
}
