## Caching the Inverse of a Matrix

## makeCacheMatrix() creates a special "matrix" object that can cache its inverse

## cacheSolve() computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

makeCacheMatrix <- function(original.matrix = matrix()) {
        
        # Checking for correct input
        if (!is.matrix(original.matrix)) {
                stop("Please give a matrix")
        }
        
        inverted.matrix <- NULL
        
        set <- function(y) {
                original.matrix <<- y
                inverted.matrix <<- NULL
        }
        
        # Getting & setting cached inverse matrix value
        get <- function() original.matrix
        # Inversing the matrix using build in solve() function
        set.inverse <- function(solve) inverted.matrix <<- solve
        get.inverse <- function() inverted.matrix
        
        list(
                set = set, 
                get = get,
                set.inverse = set.inverse,
                get.inverse = get.inverse)
        
}


## Computes the inverse of the cacheable matrix returned by makeCacheMatrix()
## If the inverse has already been calculated, cacheSolve() returns the cached inverse

cacheSolve <- function(cacheable.matrix, ...) {
        inverted.matrix <- cacheable.matrix$get.inverse()
        # Checks whether cached matrix is available
        if(!is.null(inverted.matrix)) {
                message("Getting cached inverse matrix")
                return(inverted.matrix)
        }
        # Creates inverted matrix if cached matrix is unavailable.
        matrix.to.inverse <- cacheable.matrix$get()
        inverted.matrix <- solve(matrix.to.inverse)
        cacheable.matrix$set.inverse(inverted.matrix)
        inverted.matrix
        
}