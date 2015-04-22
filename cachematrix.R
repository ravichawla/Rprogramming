##This function creates a special "matrix" object that can cache its inverse.

## This function will be divided into 2 parts one will compute the inverse and other
## will cache it

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y){
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x
                setinv <- function(solve) inv <<- solve
                getinv <- function() inv
                        list(get=get, set=set,
                             getinv=getinv, setinv=setinv)
}



## This function will create list of 4 vectors and save them in a particular environment
## These will be retrieved by the next function.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                message("Please Wait a minute, am getting the data for you...")
                return(inv)                
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        return(inv)
        ## Return a matrix that is the inverse of 'x'
}
