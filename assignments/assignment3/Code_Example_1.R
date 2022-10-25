####   LECTURE 1 CODE EXAMPLES #####

#This file introduces basic data types and logical operators in R

#numeric data types: integer, double
348 


348.50


#Character
"my string"

#logical
TRUE
FALSE

#Arithmetic Operators work as in other programming languages
42 + 1 * 2 ^ 4

#Order of operations is preserved
( (42 + 1) * 2 )^4

# Logical Operators and comparison

# | is element-wise OR, & is element-wise AND

c(TRUE,FALSE,TRUE) | c(FALSE,TRUE,TRUE)
c(TRUE,FALSE,TRUE) & c(FALSE,TRUE,TRUE)

#vs. single element OR ||, and AND && - only uses first element

c(TRUE,FALSE,TRUE) || c(FALSE,TRUE,TRUE)
c(TRUE,FALSE,TRUE) && c(FALSE,TRUE,TRUE)


# Greater than and less than as you'd expect
1 + 7 > 7

## && and || just compare single truth values
1 + 7 >= 7 && 6 > 7
1 + 7 >= 7 || 6 > 7


###Testing for Equality
3 == 3

##Testing for Inequality
3 != 3



