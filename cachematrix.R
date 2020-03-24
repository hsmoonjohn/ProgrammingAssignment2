## makeCacheMatrix creates a special "Matrix", which is actually a list containing four functions
## analogous to the Caching the Mean of a Vector example. set the value of the matrix, get the value of the matrix
## set the matrix of the inverse, and get the matrix of the inverse
## cacheSolve calculates inverse by solve function, but if it is Cached, it gives you the cached value.

## I just followed the example, which is given by vector example. However, I do not know wheter set function is necessary.
##Haeseong Moon
makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<- NULL
        }
        get<-function() x
        setinv <-function(inverse) inv<<-inverse
        getinv <-function() inv
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve calculates inverse by solve function, but if it is Cached, it gives you the cached value.
## with some message

cacheSolve <- function(x, ...) {
        inv<-x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
