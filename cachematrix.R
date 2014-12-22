## a special kind of data structure is created by makeCacheMatrix which can cache a calculated value (in this case the inverse)

## creates a data structure with 4 access functions: set, get, setCache & getCache
## the data stored is enforced to be in the shape of a matrix
makeCacheMatrix <- function(data = matrix()) {
  # empty the cache when creating a cache matrix
  cache <- NULL
  # the set function is used to change the data contained in the cache matrix
  # this also has to reset the cached value since the data changed
  set <- function(y) {
    data <<- y
    cache <<- NULL
  }
  # the get function simply returns the contained data
  get <- function() data
  # the set and get cache function store a value in the cache and retrieve it
  # the <<- operator saves to the cache variable up the environment path
  setCache <- function(value) cache <<- value
  getCache <- function() cache
  # returned is a list containig all 3 functions but no direct references to the cache and data fields
  list(set = set,
       get = get,
       setCache = setCache,
       getCache = getCache
       )
}


## use a cacheMatrix to calculate the inverse of a matrix only if it has not been done before
cacheSolve <- function(x, ...) {
  # get the cached value
  m <- x$getCache()
  # if it is not null then printa message and return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # else retrieve the data
  data <- x$get()
  # and calculate the inverse
  m <- solve(data, ...)
  # and store it in the cache
  x$setCache(m)
  # before returning it
  m
}