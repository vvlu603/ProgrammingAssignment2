##This program contains 2 functions (makeCacheMatrix and cacheSolve)
##These functions cache the inverse of a matrix. 

## makeCacheMatrix is a function that returns a list of functions. 
## it stores a matrix and a cached value of the inverse. 
## set will set the value of the matrix (to y)
## get will retrieve the value of the matrix (thus passing empty arguments)
## setInverse will solve for the inverse (thus setting the inverse to the calculated answer)
## getInverse will retrieve the value of the inverse (thus passing empty arguments)

makeCacheMatrix <- function(x = matrix()) {
#Initially set inverse to null (nothing is cached)
  inv<-NULL
#Writing the set function: setting the matrix value to input (y), and inverse to null.  
#We set the inverse to null because we are setting a new matrix (thus we calculate anew)
  set <-function(y){
    mat<<-y
    inv<<-NULL
  }
#Writing the get function (basically returning the stored matrix)
  get<-function(){
    mat
  }
#Using solve function within the setInverse function to solve the inverse of matrix.
#Here, we are storing the solved inverse
  setInverse<-function(solve){
    inv<<-solve
  }
#Writing the getInverse function to retrieve the inverse cached value. 
  getInverse <-function(){
    inv
  }
#returning a list with all of the functions. 
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve computes the inverse of the matrix returned from makeCacheMatrix. 
## The condition is written such that if the inverse has already been calculated, 
## then cachesolve should just retrieve the inverse from the cache (thus saving time)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #Here, we want to retrieve the cached inverse value from the matrix 
  inv<-x$getInverse()
  #If the cached inverse value exists, then return it
  if(!is.null(inv)){
    message("retrieving cached data")
    return(inv)
  }
  #else, get the matrix and store it as "data". Solve for the inverse. 
  data <-x$get()
  inv<-solve(data,...)
  #store the calculated inverse as a cache. 
  x$setInverse(inv)
  #Return the inverse
  inv
}
