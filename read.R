require(imager)
require(plotly)

fun = function (im, thr = "auto", approx = TRUE) 
{
  if (is.character(thr)) {
    if (nPix(im) > 1000 & approx) {
      v <- im[round(seq(1, nPix(im), l = 1000))]
    }
    else {
      v <- im
    }
    if (thr == "auto") {
      thr <- cut.kmeans(c(v))
    }
    else {
      regexp.num <- "\\d+(\\.\\d*)?|\\.\\d+"
      qt <- stringr::str_match(thr, regexp.num)[, 1] %>% 
        as.numeric
      thr <- quantile(v, qt/100)
    }
  }
  a <- im > thr
  b <- im <= thr
  im[a] <- 0
  im[b] <- 256
  im
}
dir = "C:/Users/ZotovAV/Desktop/prj/OCR"
setwd(dir)
#system.file gives the full path for a file that ships with a R package
#if you already have the full path to the file you want to load just run:
#im <- load.image("/somedirectory/myfile.png")
im <- load.image("data/raw_pic.jpg")
im = grayscale(im)
plot(im) #Parrots!
threshold(im,"10%") %>% plot


#Chain operations using the pipe operator (from magrittr)
#DELETING GRID!!!
#пороговое удаление шума
sig = 2.7
tmp.ng = deriche(im,sig,order=2,axis="x")
plot(tmp.ng)
im = threshold(tmp.ng,"5") 
# mclosing_square(im,2) %>% plot()
# tmp.ng = as.data.frame(tmp.ng )
# tmp.ng$value[tmp.ng$value>0.0023] = 0
plot(im)



# deriche(tmp.ng,order=2,axis="y")
# vv = im - tmp.ng
# plot(tmp.ng)
# vanvliet(im,sigma=2,order=1,axis="y") %>% plot()
#   vanvliet(sigma=2,order=2,axis="x") %>% 
#   plot()
# 
# 
# layout(matrix(1:2,1,2))
# grayscale(im) %>% get_gradient(axes="xy") %>% l_ply(plot)


