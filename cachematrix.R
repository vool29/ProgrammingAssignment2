##  Below are two functions that are used to create a special object that 
## stores a matrix and cache's its inverse.

## makeCacheMatrix is a function used to create a special vector which is
## really a list containing a function to set value of matrix, get value of matrix,
## set value of inverse and get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
inv<-matrix()

set<-function(y) {
x<<-y
inv<<-matrix()
}

get<-function() x

setinv<-function(mat) {
inv<<-mat
}

getinv<-function() inv

list(set=set, get=get, setinv=setinv, getinv=getinv)


}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
inv<-x$getinv()

if(!is.na(inv))
{
message("getting cached data")
return(inv)
}

data<-x$get()
inv<-solve(data,...)
x$setinv(inv)
inv

}
