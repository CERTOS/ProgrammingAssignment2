##THIS CODE IS FOR COURSERA PROGRAMMING ASSIGNMENT 2 FOR COURSERA DATA SCIENCE: R PROGRAMMING.
## The code here is partially written for the coursera course requirment 

## the function here cache the inverse of a matrix
##the makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
##the cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y) {
                   x <<- y              
                   inv <<- NULL
                    }
        get <- function() x 
        setinverse <- function(inverse) inv <<- inverse 
        getinverse <- function() inv 
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
       
}


## Write a short comment describing this function

## The function evaluates the inverse of the special "matrix" which was returned by makeCacheMatrix mentioned above.
## In case the inverse already been calculated,
## then in that case the cacheSolve will extract the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                 message("getting cached data")
                 return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
