setwd("/Users/anthony/Desktop/UniTrento/Courses/Statistical methods/labs") #set working directory to path

help(mean)

getwd() #check which working directory you're working in

3/2
3%/%2 #integer division

log(2.7, base = 3)
exp(1)

x <- 6 #variable assignment (assigning value to a part of memory on computer)
typeof(x)

xx <- "ABCD"
typeof(xx)

x2 <- TRUE
typeof(x2)

! FALSE #! negates whatever comes after

y <- c(1,2,3,4) #vector is defined by c
t(y) #t is used for vector transposition
y[1] #indexing starts at 1 (not 0 like python)

## Matrices
#matrices can only hold data of the same type so it will coerce everything to the type of the first element contained
a <- cbind(c(1,2,3), c(4,5,6))
dim(a)
a
b <- rbind(c(1,2,3, "a"), c("a", "b", "c", 1))
b

## strings
names <- c("A", "B", "C")
paste(names, sep = " ", collapse = ",") #similar to the .join string method in python

## LISTS are collection of objects

## Data Frames are tabular data structures that allow for mixed types
L3 <- LETTERS[1:3]
fac <- sample(L3, 10, replace = T)
df <- data.frame(x = 1, y = 1:10, fac = fac) #much easier than the pd.DataFrame method in python which requires you to pass in a dictionary with dict keys as colnames and values as observations associated to that keyword
str(df) #check the structure of the df

#add packages to environments 
library(pacman)
p_load("hflights") #automatically downloads the package if it is not found. Skips the install.packages("hflights") step
p_load(bbmle)
p_unload(bbmle)

#allows us to see the content of a package (i.e., the functions contained in the package) with syntax package_name::

MASS::Pima.tr
View(Mass::Pima.tr) #opens the interactive data viewer to visualize data in tabular form
View(hflights)