## 2-D Histogram and increasing transparency to avoid overplotting ##

ss = 100000
x = runif(ss,-1,1)


y = ifelse(runif(ss,-1,1) < (-0.25),
           runif(ss,-1,1),
           x)

#Plot reveals nothing
plot(x,y)

#Nothing showing up in the plot, but these variables have high correlation??
cor(x,y)

require(ggplot2)



df = data.frame(cbind(x,y))

#Overplotting, can't see the signal between x and y
ggplot(data=df,aes(x=x,y=y)) + geom_point() + theme_bw()

#Increase transparency to prevent this
ggplot(data=df,aes(x=x,y=y)) + geom_point(alpha=0.01) + theme_bw()

#2-D Histogram approach

ggplot(df, aes(x=x, y=y) ) +
  geom_bin2d(bins = 50) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()


#Another example
ss = 10000
x = sample(seq(1,5,1),replace=T,size=ss)
y = ifelse(runif(ss)<0.1,sample(seq(1,5,1),replace=T,size=ss),
           ifelse(runif(ss)<0.5,x+1,x-1))

df = data.frame(cbind(x,y))

#Overplotting, can't see any pattern
ggplot(data=df,aes(x=x,y=y)) + geom_point() + theme_bw()

#Hexogram chart
ggplot(data=df,aes(x=x,y=y)) + geom_hex(bins=20) + theme_bw()

