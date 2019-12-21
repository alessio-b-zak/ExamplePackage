

#' Merges two sorted lists
#'
#' @param x a sorted list
#' @param y a sorted list
#'
#' @return a sorted list containing elements of both lists
#' @export
#'
#' @examples
#' merge(c(1,2), c(3,4))
merge <- function(x, y) {
  x_len <- length(x)
  y_len <- length(y)
  combined_length <- x_len + y_len
  merged_list <- numeric(combined_length)
  x_i <- 1
  y_i <- 1
  for(i in 1:combined_length) {
    if (y_i > y_len || ((x_i <= x_len) && (x[x_i] <= y[y_i]))) {
      merged_list[i] <- x[x_i]
      x_i <- x_i + 1
    }
    else {
      merged_list[i] <- y[y_i]
      y_i <- y_i + 1
    }
  }
  return(merged_list)
}

#' Sorts a list recursively using the mergesort algorithm
#'
#' @param x A list
#'
#' @return A sorted list
#' @export
#'
#' @examples
#' mergesort(c(1,3,2,4,2))
mergesort <- function(x) {
  n <- length(x)
  if (n > 1) {
    split_point <- floor(n/2)
    lower_list_merged <- mergesort(x[1:split_point])
    upper_list_merged <- mergesort(x[(split_point+1):n])
    return(merge(lower_list_merged, upper_list_merged))
  }
  else {
    return(x)
  }
}

#' Recursively sorts a list by selecting a random pivot
#'
#' @param x  A list
#'
#' @return Returns a sorted version of `x`
#' @export
#'
#' @examples
#' quicksort(c(1,3,2,4,2))
#' quicksort(5,3,2,4,2))
quicksort <- function(x) {
  n <- length(x)

  if(n >1) {
    pivot <- sample.int(n, 1)
    pivot_element <- x[pivot]

    x <- x[-pivot]
    upper_list <- x[x>pivot_element]

    lower_list <- x[x<=pivot_element]
    sorted_lower <- quicksort(lower_list)
    sorted_upper <- quicksort(upper_list)

    final_list <- c(sorted_lower, pivot_element, sorted_upper)
  }
  else {
    final_list <- x
  }

  return(final_list)
}
