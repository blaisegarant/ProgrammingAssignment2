## The following functions permits to store matrices and their inverted matrix
## into cache rather than calculating it every time. 

## makeCacheMatrix: function used to set the matrix and store all the 
## pertinent information into cache. If the matrix in changed, the 
## corresponding inverse is dump from memory.
## When calculating the inverse of a matrix, that inverse is returned.

makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    set <- function(new_matrix){
        x <- new_matrix
        inverse_matrix <-NULL
    }
    get <- function(){
        x
    }
    set_inverse <- function(){
        inverse_matrix <- solve(x)
        inverse_matrix
    }
    get_inverse <- function(){
        inverse_matrix
    }
    list(set = set, get = get, set_inverse = set_inverse, 
         get_inverse = get_inverse)
}


## cacheSolve: function to retrive the inverse of a matrix. Expects a 
## CacheMatrix and returns it's inverse either the cached one or calculates it
## if not.

cacheSolve <- function(x, ...) {
    inverse <- x$get_inverse()
    if(is.null(inverse)){
        return(x$set_inverse())
    }
    inverse
}
