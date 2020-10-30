## This function stores the inverted matrix in cache so that we
## avoid constant computation of its inverse, which is particularly
## useful for bigger datasets.

makeCacheMatrix <- function(x = matrix()) {
  
  # initialize variable
  cached_matrix <- NULL
  
  # set the value of the inverted matrix, in this case, nothing is there
  set <- function(y) {
    x <<- y
    cached_matrix <<- NULL
  }
  
  # get the original matrix for computation
  get <- function() x
  
  # saves the inverted matrix into cache
  setinv <- function(solve) cached_matrix <<- solve
  
  # gets inverted matrix from cache on request
  getinv <- function() cached_matrix
  
  # create list of results from function stack
  list(set = set, get = get,
       setinverse = setinv,
       getinverse = getinv)

}





## This function inverts the original matrix stored in cache, thus
## preserving the original matrix in its raw state. If inversion has
## been computed before, get from cache; if not, compute inversion and
## store in cache.

cacheSolve <- function(x, ...) {
  
  # attempt to get inverted matrix from cache
  cached_matrix <- x$getinv()
  
  # if cached matrix is not empty (inverted), return it
  if(!is.null(cached_matrix)) {
        message("getting inverted matrix from cache")
        return(cached_matrix)
  }
  
  # get original matrix, invert, store in cached_matrix, and return it
  data <- x$get()
  cached_matrix <- solve(data, ...)
  x$setinv(cached_matrix)
  cached_matrix

}




# ----------------------
# TESTING THE CODE:
# Create mock up matrix:
# ----------------------

#> a1 <- c(3, 2, 5) 
#> a2 <- c(2, 3, 2) 
#> a3 <- c(5, 2, 4)
#> test <- rbind(a1,a2,a3)


### OUTPUT:
###     [,1] [,2] [,3]
### a1    3    2    5
### a2    2    3    2
### a3    5    2    4



# ----------------------
# Run solve (validation code)
# ----------------------

#> solve(test)

### OUtPUT:
###               a1          a2         a3
### [1,] -0.29629630 -0.07407407  0.4074074
### [2,] -0.07407407  0.48148148 -0.1481481
### [3,]  0.40740741 -0.14814815 -0.1851852


# ----------------------
# Run code (assignment code)
# ----------------------

#> cacheSolve(makeCacheMatrix(test))


### OUTPUT:
##               a1          a2         a3
## [1,] -0.29629630 -0.07407407  0.4074074
## [2,] -0.07407407  0.48148148 -0.1481481
## [3,]  0.40740741 -0.14814815 -0.1851852




# ----------------------
# Check if correct:
# ----------------------

#> identical(solve(test), cacheSolve(makeCacheMatrix(test)))

### OUTPUT:
### [1] TRUE


# ----------------------
# CHECKS OUT !
# ----------------------



