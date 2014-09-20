## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##My Cache Matrix Function Goes Here


makeMyCacheMatrix<-function(x = matrix()) {
  
  i<-NULL
  
  ##Setting the matrix
  set<-function(matrix) {
    m<<-matrix
    i<<-NULL
  }

  ##Getting the matrix & returning the matrix
  get<-function() {
    m
  }
  
  ##Setting the inverse
  setInverse<-function(inverse) {
    i<<-inverse
  }
  
  ##Getting the inverse & returning the inverse
  getInverse<-function() {
    i
  }
  
  ##Returning the list of methods
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function

##My Cache Solve Function Goes Here

cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()

  ##Returning the inverse
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }

  ##Getting the object matrix
  data <- x$get()
  
  m <- solve(data) %*% data
  
  #Setting the object inverse
  x$setInverse(m)
  
  ##Returning the matrix
  m
}
