#OCR model loading
library(h2o)
require(imager)

h2o.init()
pathM = "C:\\Users\\ZotovAV\\Desktop\\prj\\OCR\\model_dl"
model.ocr = h2o.loadModel(pathM)




im <- load.image("data/Rplot.jpeg")
im = grayscale(im)
tt = im %>% as.data.frame() %>% dcast(x~y)
tt = tt[,-1] %>% as.matrix()
tt = ifelse(tt<=0.9*max(tt),0,256)

# im2 <- im
# im2[,, 1, 1] <- tt 
# ttbkp <- tt
tt[1:dim(tt)[1],] <- tt[dim(tt)[1]:1,]
tt = t(tt)
tt[,1:dim(tt)[2]] <- tt[,dim(tt)[2]:1]
image(tt,col=grey.colors(255))
tt = as.vector(matrix(as.matrix(tt[,-1]),ncol = 1))
tt = as.data.frame(tt) %>% t() 
nm = NULL
for (i in c(1:NCOL(tt))){
  nm = c(nm,paste0("pixel",i))
}
colnames(tt) = nm
# tt = train[4,]

tt = as.h2o(tt)


pred.dl<-h2o.predict(object=model.ocr, newdata=tt)
pred.dl.df<-as.data.frame(pred.dl)
summary(pred.dl,exact_quantiles=TRUE)
