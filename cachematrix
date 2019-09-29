## Chaching the Inverse of Matrix

## makeCacheMatrix:This function creates a special "matrix"object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<- function(y){
             x<<-y
            inv<<-NULL
  
  }
  get<-function()x
  setinverse<-function(inverse)inv<<-inverse
  getinverse<-function()inv
  list(set=set,get=get,
    setinverse=setinverse,
    getinverse=getinverse)
}

## cacheSolve:This function computes the inverse of the special "matrix"returned by makeCacheMatrix above.
## If the inverse has already been calculated(and the matrix has not changed), then the cacheSolve should retrive the invrse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null (inv))  {
   message("getting cached data")
  return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinverse(inv)
  inv
}

-----------------------------------------------
> ## Test：Caching the Inverse of a Matrix
> my_matrix<-makeCacheMatrix(matrix(1:4,2,2))
> my_matrix$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> cacheSolve(my_matrix)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> 
