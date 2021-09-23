# Create packages bibliography
create_packages_bib <- function(packages, bibpath){
  knitr::write_bib(packages, bibpath)
  return(bibpath)
}

# Combine bibliography files
combine_bibs <- function(packages_bib, references_bib, outpath){
  fullbib <- lapply(c(packages_bib, references_bib), readLines)
  write(unlist(fullbib), file=outpath)
  return(outpath)
}