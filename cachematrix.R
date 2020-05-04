rm(list = ls())
## Functions that cache the inverse of matrices

## Creating an object that can cache a matrix
makeCacheMatrix <- function(original_matrix = matrix()) {
        inverted_matrix <- NULL
        set <- function(y) {
                original_matrix <<- y
                inverted_matrix <<- NULL
        }
        get <- function() original_matrix
        # Inversing matrix with solve function
        set_inverse <- function(solve) inverted_matrix <<- solve
        get_inverse <- function() inverted_matrix
        
        list(
                set = set, 
                get = get,
                set_inverse = set_inverse,
                get_inverse = get_inverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(cache_matrix, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverted_matrix <- cache_matrix$get_inverse()
        #Is there a cached inverted matrix available?
        if(!is.null(inverted_matrix)) {
                message("Getting cached inverted matrix")
                return(inverted_matrix)
        }
        
        # Creating inverted matrix in case of an absent cached inverted matrix
        matrix_to_inverse <- cache_matrix$get()
        inverted_matrix <- solve(matrix_to_inverse, ...)
        cache_matrix$set_inverse(inverted_matrix)
        inverted_matrix
        
}

