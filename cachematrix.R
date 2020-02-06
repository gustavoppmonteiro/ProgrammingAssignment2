## Programming Assignment 2
##      makeCacheMatrix: 
##      This function creates a special "matrix" object that can cache its inverse.
##      cacheSolve:
##      This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##      If the inverse has already been calculated (and the matrix has not changed), 
##      then cacheSolve should retrieve the inverse from the cache.

## Being 100% honest, i did't understood everything that I wrote.
## Basically I copied the example they gave, changing the function from 'mean' to 'solve'.
## I also used the word 'invertida' to name the inverse matrix - it's how we call it in portuguese.


## This function reates a list containing a function to

##      set the value of the matrix
##      get the value of the matrix
##      set the value of the inverse of the matrix
##      get the value of the the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        invertida <- NULL
        set <- function(y = matrix()) {
                x <<- y
                invertida <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) invertida <<- solve
        getsolve <- function() invertida
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



## This function calculates the inverse of the matrix 'invertida' created with the above function.
## If the inverse has already been calculated it returns the inverse in the cache.

cacheSolve <- function(x, ...) {
        invertida <- x$getsolve()
        if(!is.null(invertida)) {
                message("getting cached data")
                return(invertida)
                
        }
        data <- x$get()
        invertida <- solve(data, ...)
        x$setsolve(invertida)
        invertida
}

## the end
