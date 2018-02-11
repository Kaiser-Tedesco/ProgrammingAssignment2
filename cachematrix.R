## These functions solve for the inverse of a matrix or stores them in memory for subsequent access.
## They use lexical scoping to retrieve objects & functions from environments that operate as a cache.

## makeCacheMatrix creates an R object storing a matrix & its inverse. It builds 4 functions & returns 
## these in a list to parent enviro. The object it creates holds a copy of the makeCacheMatrix enviro.

makeCacheMatrix <- function(x = matrix()) {           # initialize x as an empty matrix
        I <- NULL                                     # initialize I 
        set <- function(y) {                          # Set Behavior: Repeats first 2 steps 
            x <<- y                                   #    in parent enviro. for later changes of
            I <<- NULL                                #     value w/o reinitializing. Uses <<- to 
        }                                             #     change values in parent enviro.
        get <- function() x                           # Get Behavior: goes to get x in parent enviro.
        setinv <- function(solve) I <<- solve         # Inverse setting Behav.: assigns input arg to 
                                                      #                  I in par. enviro.
        getinv <- function() I                        # Get Inv Behav.: goes to get I in parent enviro.
        list(set = set, get = get,                    # Assigns ea. func. as ele. of a list w/ names
            setinv = setinv,                          # ...name of set is "set",... etc
            getinv = getinv)
}


## This function acts on an object created by makeCacheMatrix. It populates or retrieves the inverse of
## a matrix stored in the object created by makeCacheMatrix().
cacheSolve <- function(x, ...) {

  I <- x$getinv()                                # 1: Calls getinv() of makeCacheMatrix & assigns to I 
  if(!is.null(I)) {                              # 2: Checks if I is null or if inv exists in cache
    message("getting cached matrix inverse")     # 2a: if I exists, returns inverse from input object
    return(I)
  }
  data <- x$get()                                # 3: If new matrix => gets the matrix
  I <- solve(data, ...)                          # 4: ...solves for the inverse
  x$setinv(I)                                    # 5: ...sets the inverse w/i input object
  I                                              # 6: ...returns the value of the inverse
}
