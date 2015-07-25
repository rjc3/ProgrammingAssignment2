## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
        matrx <- NULL
        set <- function(y) {
                x <<- y
                matrx <<- NULL
        }
        get <- function() x
        setinv <- function(solve) matrx <<- solve
        getinv <- function() matrx
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        matrx <- x$getinv()
        if(!is.null(matrx)) {
                message("getting cached data")
                return(matrx)
        }
        data <- x$get()
        matrx <- solve(data, ...)
        x$setinv(matrx)
        matrx
        ## Return a matrix that is the inverse of 'x'
}
