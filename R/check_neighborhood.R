check_neighborhood <- function(line, rejected, y) {
  is_neighbor     <- abs(line[1] - rejected$time) <= 1/frequency(y)
  better_neighbor <- any(is_neighbor) && all(line[4] < rejected[is_neighbor, "pvalue"])

  return(better_neighbor)
}
