### VECTORS AND VARIABLES ####
#This file introduces saving values to variables and storing multiple values in a vector

###Variable assignment is <- OR =

##See here for more information on the difference: 
#https://stackoverflow.com/questions/6140694/is-there-a-technical-difference-between-and

#Create a numeric variable
my_number = 483
my_number <- 483


###Default to double
typeof(my_number)

###Convert from double to integer
my_int = as.integer(my_number)

###typeof(my_int)
typeof(my_int)

#These different data types are not important when using R
# Since R will convert integers to doubles when needed automatically
# The only time these are important is when passing data to 
# an external routine - written in C for example

###Vector data structure
my_vec <- c(1,2,67,-8)

typeof(my_vec)


#Can hold different variables
my_vec <- c(1,2,67,-8,"goat")

typeof(my_vec)



#Get some properties
my_vec <- c(1,2,67,-8)

str(my_vec)

###INDICES START AT 1 IN R

###Slicing and Dicing

#This returns the first element of the vector
my_vec[1]

#Vector indices start at 1 in R so this returns an empty element
my_vec[0]

#You can also pass in multiple indices as a vector 
c(2:4)
my_vec[c(2,3)]
my_vec[c(2:4)]


#What does an out of bounds element return?
my_vec[5]

#This syntax assigns new elements to specified locations of the vector 
#Note what happens to indices without an element
my_vec[5] = 41.2
my_vec[7] = 33


my_vec
my_vec[6]

##Keep every element of the vector except the 6th entry
my_vec = my_vec[-6]
my_vec

#Everything except the first two entries
my_vec = my_vec[c(-1,-2)]
my_vec

#Can save the indices as a variable before using for slicing
indices = c(2:4)

indices

my_vec[indices]

#What if we remove the character string?
my_vec <- c(1,2,67,-8,"goat")
my_vec = my_vec[1:4]

#Still characters unless converted
typeof(my_vec)

#Conversion to numeric
my_vec = as.numeric(my_vec)
typeof(my_vec)

#Can't convert non-numeric characters to numeric
my_vec <- c(1,2,67,-8,"goat")
my_vec = as.numeric(my_vec)
my_vec
