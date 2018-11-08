source("../lib/extract_feat.R")

xgb_superResolution <- function(LR_dir, HR_dir, modelList){
  
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
    # featMat <- array(NA, c(dim(imgLR)[1] * dim(imgLR)[2], 24, 3))
    
    ### step 1. for each pixel and each channel in imgLR:
    ###           save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    # start_time <- Sys.time()
    
    width_LR <- dim(imgLR)[1]
    height_LR <- dim(imgLR)[2]
    padded_image_LR <- array(0, c(width_LR+2, height_LR+2, 3))
    padded_image_LR[2:(width_LR + 1), 2:(height_LR+1), ] <- imgLR[,,]
    # padded_image_LR <- array(0, c(width_LR+4, height_LR+4, 3))
    # padded_image_LR[3:(width_LR + 2), 3:(height_LR+2), ] = imgLR[,,]
    row_LR <- floor((1:(width_LR * height_LR) - 1)/height_LR + 1)
    col_LR <- (1:(width_LR * height_LR) - 1) %% height_LR + 1
    
    for(j in 1:(width_LR * height_LR)){
      featMat[j, , ] <- apply(padded_image_LR[row_LR[j]:(row_LR[j]+2), col_LR[j]:(col_LR[j]+2), ], 3, extract_feat_8)
    }
    featMat[1:height_LR, c(1,4,6),] <- 0
    featMat[(width_LR * height_LR-height_LR+1):(width_LR * height_LR), c(3,5,8),] <- 0 
    featMat[(1:width_LR-1)*height_LR+1, c(1,2,3),] <- 0 
    featMat[(1:width_LR)*height_LR, c(6,7,8),] <- 0 
    
    # for(j in 1:(width_LR * height_LR)){
    #   featMat[j, , ] <- apply(padded_image_LR[row_LR[j]:(row_LR[j]+4), col_LR[j]:(col_LR[j]+4), ], 3, extract_feat_24)
    # }
    # featMat[1:height_LR, c(1,6,11,15,20),] <- 0
    # featMat[(width_LR * height_LR-height_LR+1):(width_LR * height_LR), c(5,10,14,19,24),] <- 0 
    # featMat[(1:width_LR-1)*height_LR+1, c(1,2,3,4,5),] <- 0 
    # featMat[(1:width_LR)*height_LR, c(20,21,22,23,24),] <- 0 
    
    # mid_time <- Sys.time()
    # print(mid_time-start_time)
    
    ### step 2. apply the modelList over featMat
    predMat <- xgb_test(modelList, featMat)
    
    # time2 <- Sys.time()
    # print(time2-mid_time)
    
    ### step 3. recover high-resolution from predMat and save in HR_dir
    height_HR <- 2*height_LR
    width_HR <- 2*width_LR
    pred_Mat <- array(predMat, dim=c(width_LR*height_LR, 4, 3))
    img_HR_pred <- array(NA, c(width_HR, height_HR, 3))
    for(j in 1:(width_LR * height_LR)) {
      img_HR_pred[(2*row_LR[j]-1):(2*row_LR[j]), (2*col_LR[j]-1):(2*col_LR[j]),] <- pred_Mat[j,,]
    }
    img_center <- imageData(imgLR[rep(1:width_LR, times = rep(2, width_LR)),,])
    img_center <- img_center[, rep(1:height_LR, times = rep(2, height_LR)),]
    img_pred <- img_HR_pred + img_center
    
    img_display <- Image(img_pred, colormode = 'Color')
    # display(img_display)
    writeImage(img_display, paste0("../data/test_set/SR-I/",  "img_pred", "_", sprintf("%04d", i), ".jpg"))
    imgHR <- imageData(readImage(pathHR))
    mse_i <- mean((imgHR - img_pred)^2)
    mse_c <- mean((imgHR - img_center)^2)
    if (i==1){
      mse <- mse_i
      psnr <- 20*log10(1) - 10*log10(mse_i)
      psnr_center <- 20*log10(1) - 10*log10(mse_c)
    } else {
      mse <- c(mse, mse_i)
      psnr <- c(psnr, 20*log10(1) - 10*log10(mse_i))
      psnr_center <- c(psnr_center, 20*log10(1) - 10*log10(mse_c))
    }
  }
  return(list(mse = mean(mse), psnr = mean(psnr), psnr_center=mean(psnr_center)))
}
