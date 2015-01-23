## The following functions are assigned to calculate and to cache the inverse
## of a square matrix assuming that the matrix can be changed only by calling
## makeCacheMatrix function and the matrix supplied is always invertible.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set_matrix <- function (y) {
        x   <<-y
        inv <<- NULL
    }
    get_matrix <- function() x
    set_inversed_matrix <-function(inversed_matrix) inv <<- inversed_matrix
    get_inversed_matrix <-function() inv
    list(set_matrix=set_matrix, get_matrix = get_matrix, 
        set_inversed_matrix=set_inversed_matrix,
        get_inversed_matrix=get_inversed_matrix)
}


##  This function computes the inverse of the special "matrix" returned 
##  by makeCacheMatrix above. If the inverse has already been calculated 
##  (and the matrix has not changed), then this function retrieve 
##  the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inversed_matrix()
    if(!is.null(inv)) {
        message("geting cached data")
        return(inv)
    }
    data <- x$get_matrix()
    inv <- solve(data)
    x$set_inversed_matrix(inv)
    inv
}
