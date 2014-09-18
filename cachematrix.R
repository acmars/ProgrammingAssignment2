##---cacheMatrix.R---

## This script contains two functions: makeCacheMatrix and cacheSolve
## makeCacheMatrix is designed to create a matrix that can cache the
## inverse of a matrix, using the solve function.
## cacheSolve is designed to compute the inverse of the matrix created
## in the makeCacheMatrix function, using the cached solution if it 
## had already been calculated by the previous function to save time.

## makeCacheMatrix --- A function that creates a matrix to cache the inverse of 
##                     that matrix.

makeCacheMatrix <- function(x = matrix()) {
        m=NULL
        set=function(y){
                x<<-y
                m<<-NULL
        }
        get=function() x
        setmatrix=function(solve) m<<-solve
        getmatrix=function()m
        list(set=set,get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)

}


## cacheSolve --- A function to return the inverse of a matrix, either by
##                calculating it, or returning the previously cached inverse.

cacheSolve <- function(x, ...) {
        m=x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        ## Return a matrix that is the inverse of 'x'
        data=x$get()
        m=solve(data,...)
        x$setmatrix()
}
