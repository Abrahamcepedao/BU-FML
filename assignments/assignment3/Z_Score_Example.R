##Example of using z-scores for non-normally distributed data

tmp = rbeta(10000,1,5)
hist(tmp)

t2 = scale(tmp)
hist(t2)


df = data.frame(cbind(tmp,t2))
colnames(df) = c("Original","Z")

plot(df$Original,df$Z)

require(ggplot2)

sum(abs(df$Z) > 1) / nrow(df)
#expected 32 % for normal

#Why might a z-score be useful? 


#To take disparate measures and put them on the same units


