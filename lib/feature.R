#############################################################
### Construct features and responses for training images###
#############################################################

### Authors: Chengliang Tang/Tian Zheng
### Project 3

source("../lib/extract_feat.R")

feature <- function(LR_dir, HR_dir, n_points=1000){
  
  ### Construct process features for training images (LR/HR pairs)
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + number of points sampled from each LR image
  ### Output: an .RData file contains processed features and responses for the images
  
  ### load libraries
  library("EBImage")
  set.seed(2018)
  n_files <- length(list.files(LR_dir))
  
  ### store feature and responses
  featMat <- array(NA, c(n_files * n_points, 8, 3))
  # featMat <- array(NA, c(n_files * n_points, 24, 3))
  labMat <- array(NA, c(n_files * n_points, 4, 3))
  
  
  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    
    ### step 1. sample n_points from imgLR
    width_LR <- dim(imgLR)[1]
    height_LR <- dim(imgLR)[2]
    
    # pad after -center
    padded_image_LR <- array(0, c(width_LR+2, height_LR+2, 3))
    padded_image_LR[2:(width_LR + 1), 2:(height_LR+1), ] = imgLR[,,]
    # padded_image_LR <- array(0, c(width_LR+4, height_LR+4, 3))
    # padded_image_LR[3:(width_LR + 2), 3:(height_LR + 2), ] = imgLR[,,]
    
    samples <- sample.int(height_LR * width_LR, n_points)
    row_LR <- floor((samples - 1)/height_LR + 1)
    col_LR <- (samples - 1) %% height_LR + 1
    row_HR <- 2*row_LR
    col_HR <- 2*col_LR
    img_HR <- imageData(imgHR)
   
    ### step 2. for each sampled point in imgLR,
    
    ### step 2.1. save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    
    for(j in 1:n_points) {
      featMat[(i-1)*n_points+j, , ] <- apply(padded_image_LR[row_LR[j]:(row_LR[j]+2), col_LR[j]:(col_LR[j]+2), ], 3, extract_feat_8)

      ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat

      labMat[(i-1)*n_points+j, ,] = sweep(img_HR[(row_HR[j]-1):row_HR[j], (col_HR[j]-1):col_HR[j],], 3,
                                          padded_image_LR[row_LR[j]+1, col_LR[j]+1,])
    }
    featMat[((i-1)*n_points):(i*n_points), , ][row_LR == 1, c(1,4,6),] <- 0
    featMat[((i-1)*n_points):(i*n_points), , ][row_LR == width_LR, c(3,5,8),] <- 0
    featMat[((i-1)*n_points):(i*n_points), , ][col_LR == 1, c(1,2,3),] <- 0
    featMat[((i-1)*n_points):(i*n_points), , ][col_LR == height_LR, c(6,7,8),] <- 0
    
    # for(j in 1:n_points) {
    #   featMat[(i-1)*n_points+j, , ] <- apply(padded_image_LR[row_LR[j]:(row_LR[j]+4), col_LR[j]:(col_LR[j]+4), ], 3, extract_feat_24)
    #   
    #   ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat
    #   
    #   labMat[(i-1)*n_points+j, ,] = sweep(img_HR[(row_HR[j]-1):row_HR[j], (col_HR[j]-1):col_HR[j],], 3,
    #                                       padded_image_LR[row_LR[j]+1, col_LR[j]+1,])
    # }
    # featMat[((i-1)*n_points):(i*n_points), , ][row_LR == 1, c(1,6,11,15,20),] <- 0
    # featMat[((i-1)*n_points):(i*n_points), , ][row_LR == width_LR, c(5,10,14,19,24),] <- 0
    # featMat[((i-1)*n_points):(i*n_points), , ][col_LR == 1, c(1,2,3,4,5),] <- 0
    # featMat[((i-1)*n_points):(i*n_points), , ][col_LR == height_LR, c(20,21,22,23,24),] <- 0
    
    ### step 3. repeat above for three channels
  }
  return(list(feature = featMat, label = labMat))
}



