## These functions follow the example shown in the Assignment page,
## "Caching the Mean of a Vector". The variables were renamed properly.
## The only modification needed was to use the solve() function instead
## of the mean() function on line 28.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
        
}

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cache data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

## Sample run:
## > x <- rbind(c(2,7,9,0),c(1,5,3,8),c(3,1,7,6),c(2,1,2,9))
## > x
## [,1] [,2] [,3] [,4]
## [1,]    2    7    9    0
## [2,]    1    5    3    8
## [3,]    3    1    7    6
## [4,]    2    1    2    9
## > m <- makeCacheMatrix(x)
## > cacheSolve(m)
## [,1]       [,2]       [,3]       [,4]
## [1,]  0.8512397 -1.3636364 -1.0578512  1.9173554
## [2,]  0.2396694 -0.1363636 -0.3512397  0.3553719
## [3,] -0.2644628  0.4090909  0.5082645 -0.7024793
## [4,] -0.1570248  0.2272727  0.1611570 -0.1983471
## > cacheSolve(m)
## getting cache data
## [,1]       [,2]       [,3]       [,4]
## [1,]  0.8512397 -1.3636364 -1.0578512  1.9173554
## [2,]  0.2396694 -0.1363636 -0.3512397  0.3553719
## [3,] -0.2644628  0.4090909  0.5082645 -0.7024793
## [4,] -0.1570248  0.2272727  0.1611570 -0.1983471
## > 