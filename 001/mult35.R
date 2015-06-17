findMult <- function(a,b,start=1,end=1000) {
  out <- NULL
  counter <- 0
  for (x in start:(end-1))  if ( (x%%a == 0) || (x%%b == 0) ) {
    counter <- counter + 1
    out[counter] <- x
  }
  out
}

findMultSum <- function(a,b,end=1000) {
  get_largestMultOf_x <- function(x) {
    last_x_numbers <- (end-x):(end-1)
    last_x_numbers[which(last_x_numbers%%x==0)]
  }
  
  largestMultOf_a <-  get_largestMultOf_x(a)
  largestMultOf_b <-  get_largestMultOf_x(b)
  largestMultOf_ab <- get_largestMultOf_x(a*b)

  answer <- .5 * ((largestMultOf_a+a) * (largestMultOf_a/a) +
                  (largestMultOf_b+b) * (largestMultOf_b/b) -
                  (largestMultOf_ab+a*b) * (largestMultOf_ab/(a*b)))
  answer
}

#sum(findMult(3,5,end=150000))
#findMultSum(3,5,150000)

findMultSum(3,5,1000)
