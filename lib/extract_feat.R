extract_feat_8 <- function(mat){
  feat <- c(mat)[-5] - mat[2,2]
  return(feat)
}

extract_feat_24 <- function(mat){
  feat <- c(mat)[-13] - mat[3,3]
  return(feat)
}


