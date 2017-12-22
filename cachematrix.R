## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Function to create special matrix 
makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        
        get<-function() x
        setInv<-function(inverse) inv<<-inverse
        getInv<-function() inv
        list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## Write a short comment describing this function
## Compute inverse of matrix if computation/value have not already been carried out/exists
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getInv()
        
        ## Test if inverse of matrix exists
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        ## If hasn't been calculated, run solve() function
        data<-x$get()
        inv<-solve(data,...)
        x$setInv(inv)
        
        inv
}
