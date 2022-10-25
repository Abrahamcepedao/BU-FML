#value for t distribution
qt(0.975, df=29)

#cor.test()
#cor.test(data$explanatoryvariable, data$responsevariable,alternative=[alternative], method=[method], conf.level=[confidence level])

#linear model
#lm(data$responsevariable ~ data$explanatory)


# Calculating probability from F-statistics
# Use pf() function to calculate the area to the left of a given F-statistic
#> pf([F statistic], df1=[degree of freedom of the numerator], df2=[degree of freedom of the denominator])
# Calculating F-statistics from probability
# Use qf() function to calculate F-statistic with the specifies area to theleft
#> qf([probability], df1=[degree of freedom of the numerator], df2=[degree of freedom of the denominator])
#> 
#>  # getting the p-value out of F-value
#> pf(18.51, df1=1, df2=2)
#[1] 0.9499929  # (the area to the left)
#> pf(18.51, df1=1, df2=2, lower.tail = F) # (area to the right)
#[1] 0.05000706
# getting the F value
#> qf(0.975, df1=1, df2=29, lower.tail = T)
#> 
#> 
#> # Calculating probability from T-statistics
#> qt(0.975, df=11, lower.tail = T)
#> pt(2.04523, df=29)
#> 
#> 
#> 
#> 

x12234y12324
y = c(1,2,3,2,4)
x = c(1,2,2,3,4)

cor(x, y)

