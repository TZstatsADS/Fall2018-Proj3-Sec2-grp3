extract_feat <- function(mat){
  feat <- c(mat)[-5] - mat[2,2]
  return(feat)
}


