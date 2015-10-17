## These functions create a matrix object and a caching solve function on it that calculates the inverse matrix.
## Caching solve function does not recalculate inverse matrix unless the matrix itself changes.

## makeCachMatrix creates a matrix object that can store its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inverseX<-NULL
        set<- function(y)
        {
                x<<-y
                inverseX<<-NULL
        }
        get<- function() x
        getInverse<- function() inverseX
        setInverse<- function(solveX) inverseX<<-solveX
        list(set=set,get=get,getInverse=getInverse, setInverse=setInverse)
}


## cacheSolve functions operates on object returned by makeCacheMatrix object (argument x)
## It returns cached matrix or calculates it if it wasn't calculated before

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseX<-x$getInverse()
        if(!is.null(inverseX))
        {
                message("returning cached data")
                return(inverseX)
        }
        data<-x$get()
        inverseX<-solve(data,...)
        x$setInverse(inverseX)
        inverseX
}


