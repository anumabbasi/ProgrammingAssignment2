## The functions below allow for us to cache the inverse of a matrix so that
## it does not need to repeatedly be calculated. However,if the matrix has changed,
## the function will compute the inverse of the new matrix.

## the makeCacheMatrix function returns a list with functions that will be utilizedd
## in the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set <- function(y){
      x<<-y
      inv<- NULL
    }
    get<-function()
      x
    setinv<-function(inverse) 
      inv<<-inverse
    getinv<-function() 
      inv
    list(set=set,get=get,setinv=setinv,getinv=getinv)
  
  }
  

## The cacheSolve function allows user to return the previously calculated
## inverse of the matrix stored, otherwise it calculates the new matrix inverse.

cacheSolve <- function(x) {
    inv <- x$getinv()
      if (!is.null(inv)){
        message("data previously calculated, retrieving..")
        return(inv)
      }
  inverse.data <- x$get()
  inv <- solve(inverse.data)
  x$setinv(inv)
  inv
}
