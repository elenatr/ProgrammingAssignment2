## These two functions cache the inverse of a matrix.

## The first function with an argument "x"(which is a matrix) contains 4 functions in it:
## 1) sets new matrix in different environment (cache)
## 2) gets the matrix from the cache
## 3) sets a function that inverse the matrix in the cache
## 4) gets the inverse of the matrix from the cache

makeCacheMatrix <- function(x = matrix()) {  
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## The second function does the following:
## checks, whether the inverse of the matrix is in the cache
##    if it's true, the function returns the inverse of the matrix from the cache
##                   (with a message "getting cached data")
##    if it's false, the function gets the matrix from the cache, inverses it,
##                    and prints the inverse of the matrix 

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
