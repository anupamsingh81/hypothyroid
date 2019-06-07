drive_upload('thyrosim.R')
drive_upload('shashi.pdf')

drive_upload('nirmala.pdf')

drive_upload('zafar.pdf')

###########3
simcor <- function (n, xmean, xsd, ymean, ysd, correlation) {
  x <- rnorm(n)
  y <- rnorm(n)
  z <- correlation * scale(x)[,1] + sqrt(1 - correlation^2) *
    scale(resid(lm(y ~ x)))[,1]
  xresult <- xmean + xsd * scale(x)[,1]
  yresult <- ymean + ysd * z
  data.frame(x=xresult,y=yresult)
}
r <- simcor(n = 50, xmean = 12, ymean = 30, xsd = 3, ysd = 4, correlation = 0.56)
cor(r$x,r$y)
#generate correlation between two vectors n simulate
########
##generate correlation with prespecified vector
simcorone = function (x, ymean=0, ysd=1, correlation=0) {
  n <- length(x)
  y <- rnorm(n)
  z <- correlation * scale(x)[,1] + sqrt(1 - correlation^2) *
    scale(resid(lm(y ~ x)))[,1]
  yresult <- ymean + ysd * z
  yresult
}
z = simcorone(r$x,ymean=90,ysd =12,correlation=0.8)