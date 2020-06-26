## this pair of functions creat a matrix, stores the inverse (i) of that matrix, and then checks if there is any inverse stored in the memory, if not the case, calculates such a inverse and then stores it
## makeCachematrix allows to retrieve the inverse (i) of a fiven x matrix 

makeCacheMatrix <- function(x = matrix()) {   ##NB que esto significa que x es por 
##default una matrix vacia
        i <- NULL
        set <- function(y) {     ##creo la funcionalidad set en caso de que quiera modificar externamente el input x
                x <<- y
                i <<- NULL
        }
        
        get <- function () x    ##creo la funcionalidad get
        setinverse <- function(inverse) i <<- inverse   ##creo la funcionalidad setinverse
        getinverse <- function() i
        list(set = set, get = get,    ## le asigno nombres para poder llamarlas con el operador $
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calls functions within the great function makeCacheMatrix. if a inverse is found withind makeCacheMatrix, cacheSolve shall return it, otherwise it will calculate such an inverse and store it within makeCachematrix using the functions there contained for that purpose

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        else {
                data <- x$get()
                i <- solve(data, ...)
                x$setinverse(i)
                i
        
        ## Return a matrix that is the inverse of 'x'
} }
