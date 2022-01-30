# Extract Variance from formula
get_variance <- function(component, terms) {
comp_pos <- which(grepl(paste0(component, ".*"), terms))
  if (length(comp_pos) > 0) {
    comp <- terms[comp_pos]
    comp <- as.numeric(gsub("(.*\\()|(\\))", "", level))
  } else {
    comp <- NULL
  }
  return(comp)
}
