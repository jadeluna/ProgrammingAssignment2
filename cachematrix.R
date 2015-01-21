## Put comments here that give an overall description of what your
## functions do

## the below function will create a special 'matrix' object that can cache its inverse
## create an inversable matrix

makeCacheMatrix <- function(x = matrix()) {
    j <- NULL                                             ##j will store the cached matrix inverse
    set <- function(y){                                   ##set for the matrix     
        x <<- y
        j <<- NULL
    }
    get <- function() x                                   ##get for the matrix 
    setinv <- function(inverse) j <<- inverse             ##set for the inverse
    getinv <- function() j                                ##get for the inverse    
    list(set= set, get = get,                             ##return a list with our new functions
         setinv = setinv, 
         getinv = getinv)
}

## the below function will compute the inverse, and if already calculated, will returned the cached

cacheSolve <- function(x, ...) {
        j <- x$getinv()
        if (!is.null(j)){                                 ##if inverse is already calculated, return it
            message("getting cached data")
            return(j)
        }
        data <- x$get()                                   ##inverse not calculated, so we calculate it
        j <- solve(data,...)
        x$setinv(j)                                       ##cache the inverse
        j                                                 ##call the inverse 
}
