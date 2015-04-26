#Creates an objec to store cahed matrixes.
#Adds sfunctionality to that object to set values get values set inverse and get inverse


#Function to cache matrix inverses
makeCacheMatrix <- function(x = numeric()) {
  inverse <- NULL #Sets the inverse to NULLL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  setInverse <- function(y) inverse <<- y
  getInverse <- function() inverse
  get <- function() x
  list(set=set,getInverse=getInverse, get= get,setInverse=setInverse)
}

#Solves caching by checking if exists in the object
#If does not exists the it wil apply solve function
cacheSolve <- function(x = numeric()) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  x$setInverse(solve(x$get()))
  x$getInverse()
}