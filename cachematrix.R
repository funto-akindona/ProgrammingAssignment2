## These two functions will compute and store the inversion of an invertible matrix.

## makeCacheMatrix creates the matrix and caches the inverse of this matrix. 
##Matrix dimensions must be entered.

makeCacheMatrix <- function(x=matrix(),nrow,ncol) {
  
  i<-NULL
  set <- function (y=matrix(),a,b){ ##new dimensions must be passed with set()
    x<<-y
    i<<-NULL
    nrow<<-a
    ncol<<-b
  }
  get<-function() matrix(x,nrow,ncol) #returns x in matrix form
  setinverse<-function(inverse) i<<-inverse
  getinverse<-function() i
  
  list(set=set, get=get, 
       getinverse=getinverse, setinverse=setinverse)
}


##cacheSolve retrieves from cache or computes the inverse of given matrix

cacheSolve <- function(x, ...) {
  i<-x$getinverse() 
  
  if(!is.null(i)){      
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}