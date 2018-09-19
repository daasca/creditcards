library(kknn)
apple_sauce = read.table("C:\\Users\\daasca\\Documents\\credit_card_data.txt", header=F, stringsAsFactors = F)

head(apple_sauce)
orange_juice = function(X){
  pred<- rep(0,(nrow(apple_sauce))) 
  
  for (i in 1:nrow(apple_sauce)){
    knn_model=kknn(V11~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10,apple_sauce[-i,],apple_sauce[i,],k=X, scale = T) 
    pred[i] <- as.integer(fitted(knn_model)+0.5) }
  acc = sum(pred == apple_sauce[,11]) / nrow(apple_sauce)
  return(acc)}

test_vec <- rep(0,15) 
for (X in 1:15){
  test_vec[X] = orange_juice(X) }

plat_n <- as.matrix(test_vec * 100) 
plat_n 
salad_fing <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
plot(salad_fing,plat_n)
max(plat_n)
