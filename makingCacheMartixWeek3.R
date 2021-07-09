makeCacheMatrix <- function(m= matrix()){
  inv <- NULL
  set <- function(n){
    m <<- n
    inv <<- NULL
  }
  get <- function() {m}
  setInverse <- function(inverse) (inv <<- inverse)
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


cachesolve <- function (m, ...){
  inv <- m$getInverse()
  if(!is.null(inv)){
    message("getting inverse chached data")
    return(inv)
  }
  mat <- m$get()
  inv <- solve(mat, ...)
  m$setInvert(inv)
  inv
}

