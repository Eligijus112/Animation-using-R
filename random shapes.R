library(ggplot2)
library(animation)
library(dplyr)

gen.coef <- function(type){
  m <- numeric(1)
  if(type=="normal") m <- rnorm(1)
  if(type=="exp")    m <- rexp(1)
  if(type=="unif")   m <- runif(1)
 return(m)
}

generate.lines <- function(type1, type2){
  j <- 1
  a <- gen.coef(type1)
  b <- gen.coef(type2)
  #colour <- sample(1:length(colours()), 1)
  int1 <- sample(0:1, 1)
  int2 <- sample(0:1, 2)
  int3 <- sample(0:1, 3)
  p <- qplot(0, ylim=c(-3*a, 3*a), xlim=c(-3*b, 3*b), type='n',axes=FALSE,ann=FALSE, geom="blank")
  a <- p + geom_abline(intercept=a, slope=b, col=rgb(int1, int2, int3, alpha=0.2), lwd=2)  +
                                                                    theme(legend.position = "none", 
                                                                    axis.text.x=element_blank(),
                                                                    axis.text.y=element_blank(),
                                                                    axis.ticks=element_blank(),
                                                                    axis.title.x=element_blank(),
                                                                    panel.grid.major = element_blank()) 
   print(a) 
}

lines.animate <- function(nr, type1, type2){
  nr <- 1
  while(nr <= 50){
  generate.lines(type1, type2)
  nr <- nr +1
  }
}

path.to.convert <- paste0(shortPathName(
  "C:\\Program Files (x86)\\ImageMagick-6.9.0-Q16\\"), "convert.exe")
ani.options(convert=path.to.convert)

saveGIF({
  ani.options(nmax = 40)
  lines.animate(50, "normal", "normal")
}, interval = 0.20, movie.name = "shapes ggplot.gif", ani.width = 700, ani.height = 700)

#######################################
#######################################
############ Heart shape ##############
#######################################
#######################################

x.f <- function(t){
  y <- 16*(sin(t))^3
  return(y)
}

y.f <- function(t){
  y <-  13*cos(t) - 5*cos(2*t) - 2*cos(3*t) - cos(4*t)
}

gen.heart.data <- function(acc, scale){
  z <- seq(from=-10, to=10, length.out=acc)  
  heart.data <- matrix(ncol=2, nrow=acc) %>% as.data.frame()
  heart.data[, 1] <- x.f(z)/scale
  heart.data[, 2] <- y.f(z)/scale
  colnames(heart.data) <- c("x", "y")
  return(heart.data)
}

heart <- gen.heart.data(1000, 1)
qplot(x, y, data=heart)

animate.heart <- function(soft, pulse.start, pulse.end){
  pulse <- c(seq(from=pulse.start, to=pulse.end, length.out=soft), seq(from=pulse.end, to=pulse.start, length.out=soft))
  for(j in 1:length(pulse)){
    heart <- gen.heart.data(10000, pulse[j])
    a <- qplot(x, y, data=heart, xlim=c(-20, 20), ylim=c(-20, 20), col="red", lwd=3) + theme(legend.position = "none", 
                                                                                          axis.text.x=element_blank(),
                                                                                          axis.text.y=element_blank(),
                                                                                          axis.ticks=element_blank(),
                                                                                          axis.title.x=element_blank(),
                                                                                          axis.title.y=element_blank(),
                                                                                          panel.grid.minor=element_blank(),
                                                                                          panel.grid.major=element_blank())
    print(a)
  }
}

## The same using ggplot

animate.heart2 <- function(soft, pulse.start, pulse.end){
  pulse <- c(seq(from=pulse.start, to=pulse.end, length.out=soft), seq(from=pulse.end, to=pulse.start, length.out=soft))
  for(j in 1:length(pulse)){
    heart <- gen.heart.data(10000, pulse[j])
    #colnames(heart) <- c("x", "y")
    ids <- factor(c("1.1", "1.2", "1.3", "1.4"))
    
    values <- data.frame(
      id = ids,
      value = c(1, 2, 3, 4)
    )
    
    positions <- data.frame(
      id = rep(ids, each = 2500),
      x = heart[, 1],
      y = heart[, 2]
    )
    
    datapoly <- merge(values, positions, by=c("id"))
    
    a <- ggplot(datapoly, aes(x=x, y=y)) + geom_polygon(aes(fill="red", group=id))
    #a <- ggplot(data=heart, aes(x=x, y=y, col="red", lwd=2)) + geom_polygon(data=heart, fill="red") 
    a <- a + xlim(c(-20, 20)) + ylim(c(-20, 20)) + theme(legend.position = "none", 
                                                         axis.text.x=element_blank(),
                                                         axis.text.y=element_blank(),
                                                         axis.ticks=element_blank(),
                                                         axis.title.x=element_blank(),
                                                         axis.title.y=element_blank(),
                                                         panel.grid.minor=element_blank(),
                                                         panel.grid.major=element_blank())
    
    print(a)
  }
}


saveGIF({
  ani.options(nmax = 40)
  animate.heart2(15, 1, 1.2)
}, interval = 0.07, movie.name = "heart final.gif", ani.width = 700, ani.height = 700)

#########################################################
#########################################################
################### Additional info #####################
#########################################################
#########################################################

age <- 1:10
y.low <- rnorm(length(age), 150, 25) + 10*age
y.high <- rnorm(length(age), 250, 25) + 10*age

plot(age,y.high,type = 'n', ylim = c(100, 400),
     ylab = 'Y Range', xlab = 'Age (years)')
lines(age, y.low, col = 'grey')
lines(age, y.high, col = 'grey')

polygon(c(age, rev(age)), c(y.high, rev(y.low)),
        col = "grey30", border = NA)

## Additional info

ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3"))

values <- data.frame(
  id = ids,
  value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5)
)

positions <- data.frame(
  id = rep(ids, each = 2000),
  x = heart[, 1],
  y = heart[, 2]
)

datapoly <- merge(values, positions, by=c("id"))

p <- ggplot(datapoly, aes(x=x, y=y)) + geom_polygon(aes(fill="red", group=id))
print(p)
