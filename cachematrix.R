> makeCacheMatrix <- function(x = matrix()) {
+     m <- NULL
+     set <- function(y) {
+         x <<- y
+         m <<- NULL
+     }
+     
+     get <-function()x
+     setmatrix<- function(solve) m<<- solve
+     getmatrix<- function() m
+     list(set=set, get=get,
+          setmatrix = setmatrix,
+          getmatrix = getmatrix)
+     
+ }
> ## This function produces the inverse of the matrix above, but, first 
> ## checks to see if it has already been produced. If so it will skip
> ## the calculation and 'get' the inverse from the cache. If not, it
> ## will solve and set the solution into the cache via 'x$setmatrix(x)'.
> 
> cacheSolve <- function(x=matrix(), ...) {
+     m <- x$getmatrix()
+     if(!is.null(m))
+     {
+         message("getting cached data")
+         return(m) ## grabs cached data and returns
+     }
+     data <- x$get()
+     m <- solve(data,...)
+     x$setmatrix(m)
+     m
+ }
> ## Return the matrix which is inverse of x.
