##First functions create a matrix and manager getters, setters and the inverse table
##Second function returns the inverse of a matrix. If the matrix is cached then
##it returns the cached value

## Creates a matrix and manage getters, setters of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <-NULL
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  
  get <-function() x
  setInversTable <- function(inversTable) m<<-inversTable
  getInversTable <- function() m
  list(set=set, get=get,
       setInversTable = setInversTable,
       getInversTable = getInversTable)
}


## Write a short comment describing this function
##This function returns the inverse of matrix
##if the value is cached then it returns the 
##the value from memory (cached)

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

  m <- x$getInversTable()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data <-x$get()
  m <- solve(data,...)
  x$setInversTable(m)
  m
}
