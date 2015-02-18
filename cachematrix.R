##  Calculate matrix inversion and save it 

##  ---- Create special matrix for cache memory 
makeCacheMatrix <- function(x = matrix()) {
  
    ## create matrix with same dimensions as input matrix
    inv <- list()
    length(inv) <- length(x)
    dim(inv) <-  dim(x)
    
    det <-   NULL 
    set <- function(y) {
        x <<- y
        inv <- list()
        length(inv) <- length(x)
        dim(inv) <-  dim(x)
        det <-   NULL 
    }
    
    ## function that sets input matrix (x) determinant 
    setdet <- function(determ) det <<- determ
    ## function that gets input matrix determinant
    getdet <- function() det
    
    ## function that gets the input matrix
    get <- function() x
    ## function that sets the inverse of input matrix
    setinv <- function(inverse) inv <<- inverse
    ## funtion that gets the inverse of input matrix
    getinv <- function() inv
    ## define special matrix
    list(set = set, get = get, 
         setdet = setdet, getdet = getdet,
         setinv = setinv, getinv = getinv)
}


## ---
cacheSolve <- function(x, ...) {
    ## get inverse and determinant of special matrix
    inv <- x$getinv()
    det <- x$getdet()
    
    ## if determinant exists, get inverse from cached data
    if(!is.null(det)) {
       message("getting cached data")    
       # return cached inverse matrix
       return(inv)
    }
    ## if determinant is NULL, get input matrix
    data <- x$get()
    ## invert input matrix
    inv <- solve(data, ...)
    ## calculate input matrix determinant
    det <- det(data)
    ##save into cached data the inverse matrix just calculated
    x$setinv(inv)
    ## and the determinant just calculated
    x$setdet(det)
    ##return inverse matrix just calculated
    inv
}
