## The two functions provide the ability to create a matrix object that allows
## the user to cache its inverse. This can save computational power if a user
## needs to use the inverse of matrix and only calculate it when necessary.

## This function is used to create a special 'matrix' object. This object is a
## list of functions that set and get the matrix, and set and get the inverse 
## of the matrix.
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function is used when the inverse of the matrix is needed. This function
## checks whether the inverse has already been calculated and stored within the
## previous function. If so, a message stating that the data is cached, and the 
## inverse is displayed. If not, the inverse is calculate (via solve function),
## stored within 'makeCacheMatrix' and displayed 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        originalMatrix <- x$get()
        s <- solve(originalMatrix, ...)
        x$setsolve(s)
        s        
}