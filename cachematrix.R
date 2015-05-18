## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    imat <- NULL
    set <- function(y) {
        x <<- y
        imat <<- NULL
    }
    get <- function() x
    setimat <- function(inverse) imat <<- inverse
    getimat <- function() imat
    list(set = set, get = get, setimat = setimat, getimat = getimat) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    imat <- x$getimat()
    if(!is.null(imat)){
        message("getting cached data")
        return(imat)
    } 
    data <- x$get()
    imat <- solve(data)
    x$setimat(imat)
    imat
}
