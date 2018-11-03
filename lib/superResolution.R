########################
### Super-resolution ###
########################

### Author: Chengliang Tang
### Project 3

source("../lib/extract_feat.R")

superResolution <- function(LR_dir, HR_dir, modelList){
  
  ### Construct high-resolution images from low-resolution images with trained predictor
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + a list for predictors
  
  ### load libraries
  library("EBImage")
  n_files <- length(list.files(LR_dir))
  
  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img", "_", sprintf("%04d", i), ".jpg"))
    pathHR <- paste0(HR_dir,  "img", "_", sprintf("%04d", i), ".jpg")
    featMat <- array(NA, c(dim(imgLR)[1] * dim(imgLR)[2], 8, 3))
    
    ### step 1. for each pixel and each channel in imgLR:
    ###           save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    
    width_LR <- dim(imgLR)[1]
    height_LR <- dim(imgLR)[2]
    padded_image_LR <- array(0, c(width_LR+2, height_LR+2, 3))
    padded_image_LR[2:(width_LR + 1), 2:(height_LR+1), ] = imgLR[,,]
    for(j in 1:width_LR * height_LR){
      row_LR <- floor((j - 1)/height_LR + 1)
      col_LR <- (j - 1) %% height_LR + 1
      featMat[j, , ] <- apply(padded_image_LR[row_LR:(row_LR+2), col_LR:(col_LR+2), ], 3, extract_feat)
    }
    
    ### step 2. apply the modelList over featMat
    predMat <- test(modelList, featMat)
    ### step 3. recover high-resolution from predMat and save in HR_dir
    height_HR <- 2*height_LR
    width_HR <- 2*width_LR
    img_HR_pred <- array(predMat, dim=c(width_HR, height_HR, 3))
    img_center <- imageData(imgLR[rep(1:width_LR, times = rep(2, width_LR)),,])
    img_center <- img_center[, rep(1:height_LR, times = rep(2, height_LR)),]
    img_pred <- img_HR_pred + img_center
    
    img_display <- Image(img_pred)
    # display(img_display)
    # writeImage(img_display, paste0(HR_dir,  "img_pred", "_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(pathHR)
    mse_i <- sum((imgHR - img_HR_pred )^2)/(3*height_HR*width_HR)
    if (i==1){
      mse <- mse_i
      psnr <- 20*log(255) - 10*log(mse_i)
    } else {
      mse <- c(mse, mse_i)
      psnr <- c(psnr, 20*log(255) - 10*log(mse_i))
    }
  }
  return(mean(mse), mean(psnr))
}





