makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        get <- function() x
        setSolvedMatrix <- function(solve) invMatrix <<- solve
        getSolvedMatrix <- function() invMatrix
        list(set = set, get = get,
             setSolvedMatrix = setSolvedMatrix, getSolvedMatrix = getSolvedMatrix)
}

cacheSolveMatrix <- function(x, ...) {
        invMatrix <- x$getSolvedMatrix()
        if(!is.null(invMatrix) && identical(x, x$get())) {
                message("getting cached data")
                return(invMatrix)
        }
        data <- x$get()
        invMatrix <- solve(data, ...)
        x$setSolvedMatrix(invMatrix)
        invMatrix
}