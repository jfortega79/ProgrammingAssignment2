## The functions first calculate the inverse of the Matrix "X" and then storage it in the memory.
## Then the other functions first tries to recover that value, and recalculates the inverse if it is not storaged in the memory


## This function creates a matrix called "Inversa" that is the inverse of "X"
## The function storages "Inversa" in the memory

makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
  set <- function(y) {
    x <<- y
    inversa <<- NULL
  }
  get <- function() x
  setinversa <- function(solve) inversa <<- solve
  getinversa <- function() inversa
  list(set = set, get = get,
       setinversa = setinversa,
       getinversa = getinversa)
    }


## This function first tries to retrieve "Inversa" (the inverse of "X") from the memory.
## If the inverse is not available, the function calculates the inverse of "X".

cacheSolve <- function(x, ...) {
  inversa <- x$getinversa()
  if(!is.null(inversa)) {
    message("getting cached data")
    return(inversa)
  }
  data <- x$get()
  inversa <- solve(data, ...)
  x$setinversa(inversa)
  inversa
  
}
