## This R function will cache the inverse of a Matrix 
## rather than compute it repeatedly

## This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function (y){
            x <<- y
            i <<- NULL
        }
        get <- function()x
        setinverse <- function(inverse) i <<- inverse 
        getinverse <- function() i
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}
##check if there is an existing calculated inverse; 
##if not, calculate the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return (i)
    }
    data <- x$get()
    i <-solve(data, ...)
    x$setinverse(i)
    i
}
