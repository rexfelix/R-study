x <- seq(-3,3,by=0.01)
f <- function(x){
  return(1/(1 + exp(-x)))
}
sig <- data_frame(xi=x,yi=f(x))

sig %>% ggplot(aes(xi,yi))+geom_line()

a <- rep(1:2, times=3,each=3)
b <- 1:9
sort(a)
rev(a)
table(a)
unique(a)
b[2]
b[-1]
b[2:4]
b[-(4:7)]
b[c(1,3,5)]
b[b==3]
b[b>5]
b[b%in%c(1,2,3)]
c <- c('a'=1,'b'=2,'c'=3)
c
c['a']
for (aa in a) {
  if (aa==1){
    print(paste('one',collapse = ' '))
    } else{
      print(paste('two',collapse = ' '))
    }
}
z=1
while(z<50){
  print(z)
  z=z+sum(z)
}
s <- c(1,3,NA,NULL,6)
is.na(s)
sum(is.na(s))
mean(is.na(s))
sum(is.null(s))
d <- matrix(a, nrow=3, byrow = TRUE)
d
df <- data.frame(x=1:3,y=c('a','b','c'))
df
df[1,2]
df[1:2,1:2]
df[-1,]
df[[2]]
df[2]
df[,2]
df[2,]
view(df)
nrow(df)
dim(df)
df$y
ls()
