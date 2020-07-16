## We want to create a code that can compute the inverse of a matrix or 
## retrieve the inverse if it has already been computed.

## The following function creates a special matrix object
## that can store its inverse if it has been already computed.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set_matrix <- function(y){
        x <<- y
        i <<- NULL
    }
    get_matrix <- function () x
    set_inverse <- function(inverse) i <<- inverse
    get_inverse <- function() i
    list(set_matrix = set_matrix , get_matrix = get_matrix,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## This function computes the inverse of a matrix defined by the function
## above and computes its inverse if it has not been already computed; 
## otherwise, it will retrieve it form the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inverse()
    if (!is.null(inv)) {
        message("Inverse already computed")
        return(inv)
    }
    my_matrix <- x$get_matrix()
    inv <- solve(my_matrix , ...)
    x$set_inverse(inv)
    inv
}

