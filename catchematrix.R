## Put comments here that give an overall description of what your
## functions do?

## We understand that Matrix inversion is usually a costly computation and
## there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## So Below are a pair of functions that we are going to use to find the solution for such opportunity.


## Write a short comment describing this function?

## This function creates a special "matrix" object that can cache its inverse.

-makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
        +    inv <- NULL                     ## initialize inv as NULL; will hold value of matrix inverse
        +    set <- function(y)}{            ## define the set function to assign new
                +        x <<- y             ## value of matrix
                +        inv <<- NULL        ## if there is a new matrix, reset inv to NULL
        }


+ get <- function() x                          ## define the get fucntion
+    setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv
+    getinverse <- function() inv                     ## gets the value of inv
+    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## this is list standard order to the functions with the $ operator
+

        }



## Write a short comment describing this function?
## This function computes the inverse of the special "matrix" created by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then it should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        +    inv <- x$getinverse()
        +    if(!is.null(inv)) {  ## is.null verification.
                +        message("getting cached data")
                +        return(inv) ## inspection of inv by using return function.
                +  }
        +    data <- x$get()
        +    inv <- solve(data, ...)
        +    x$setinverse(inv)
        +    inv
}

