#ANOVA table for statistical testing of multiple linear regression model
states <- read.csv("Datasets/states.csv")

attach(states)


data <-data.frame(poverty, metro_res, white, hs_grad, female_house)

# Data is about poverty in 50 states plus district of Colombia. 
# 
# The variables are percentage living in poverty in each state, 
# percentage of residents living in a metropolitan area, percentage white, 
# percentage of high school graduates, and percentage of female head of householders. 

## put correlations on the upper panels,
## with size proportional to the correlations.


panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  
  if(missing(cex.cor)) 
    cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}


pairs(data, upper.panel = panel.cor)

# See read the five ways to visualize pairwise comparisions
# https://www.r-bloggers.com/five-ways-to-visualize-your-pairwise-comparisons/


### GGplot version
require(GGally)
ggpairs(data) + theme_bw()


m <- lm(poverty~ female_house + white + hs_grad + metro_res)


m2 <- lm(poverty ~ hs_grad + metro_res)


# Summary function can calculate almost everything that you need. 
summary(m)
summary(m2)



# Using anova table 
anova_table <-anova(m2)
anova_table

#Residual sum of squares
SSE <-anova_table$`Sum Sq`[3]
SSE

#Total sum of squares
SST <-anova_table$`Sum Sq`[1]+anova_table$`Sum Sq`[2]+anova_table$`Sum Sq`[3]
SST

#Manual R2
R2 <- (anova_table$`Sum Sq`[1]+anova_table$`Sum Sq`[2]) / SST


# adjusted R^2
# adjusted_R2 <-  1 - (SSE/SST )  * (( n - 1 ) / ( n - k - 1) )
# n number of samples
# k number of independent varaibles 

adjusted_R2 <-  1 - (SSE/SST )  * (( length(poverty) - 1 ) / ( length(poverty) - 2 - 1) )



# A second model - head of households that are female only
m2 <- lm(poverty~female_house)
anova_table2<-anova(m2)
anova_table2

totalss_anova2 <-anova_table2$`Sum Sq`[1]+anova_table2$`Sum Sq`[2]
totalss_anova2

# A third  model - Simple Linear Regression only considering racial breakdown
m3 <- lm(poverty~white)
anova_table3<-anova(m3)
anova_table3

totalss_anova3 <-anova_table3$`Sum Sq`[1]+anova_table3$`Sum Sq`[2]
totalss_anova3
