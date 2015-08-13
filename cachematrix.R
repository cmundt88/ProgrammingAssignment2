## This assignment involves a function containing child functions which
## pass variables using lexical scoping such that the parent function
## can 'see' the variables being manipulated by the children

## makeCacheMatrix takes a square matrix, but is really a function of
## functions such that cacheSolve can access the functions within

## The makeCacheMatrix function allows the caller to get the matrix, set it, 
## as well as set the inverse and get it.  It will also store the inverse of 
## the last calculated inverse such that it can be skipped by cacheSolve

makeCacheMatrix <- function(mat1 = matrix()) {
    inv <- NULL
    
    set <- function(mat2) {
        mat1 <<- mat2  ## pass to parent function
        inv <<- NULL  ## pass to parent function
    }
    
    get <- function() mat1
    
    setinverse <- function(pass_inv) inv <<- pass_inv  ## pass to parent function
    
    getinverse <- function() inv
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## accesses the MakeCacheMatrix function and it's children to see if there
## is a cached solution of the inverse.  If there isn't it calculates and
## populates the cache

cacheSolve <- function(mat1, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inv <- mat1$getinverse()  ## calls child of MakeCacheMatrix
    
    ##  check to see if there is a cached value in inv variable
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- mat1$get()  ## calls child of MakeCacheMatrix
    inv <- solve(data, ...)  ## calculates inverse when not cached
    mat1$setinverse(inv)  ## calls child of MakeCacheMatrix to set the inverse
    inv
}
