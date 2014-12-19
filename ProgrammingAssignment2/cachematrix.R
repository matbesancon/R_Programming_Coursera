## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#Setting the default value in the cache

makeCacheMatrix <- function(x = matrix()) {
        #Set value of the matrix
        inv<-NULL
        set<-function(m){
                x<<-y
                inv<<-NULL
        }
        get<- function()x
        setinv<-function(inv) inv<<-inv
        getinv<-function() inv
        list(set=set,get=get,
             setinv = setinv
             getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        if (!if.null(inv)){
                message("Getting cached data")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setinv(inv)
        inv
}
