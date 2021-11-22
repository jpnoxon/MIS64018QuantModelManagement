#Assignment 6

library(lpSolveAPI)

#Question 1

#find longest path

#Objective Function:
# Max: 5x12+ 3x13+ 4x24+ 2x25+ 3x35+ 1x46+ 4x47+ 6x57+ 2x58+ 5x69+ 4x79+ 7x89
# Constraints:
# starting node:
# 5x12 + 3x13 = 1
# intermediate nodes;
# 5x12 - 4x24 - 2x25 = 0 
# 3x13 - 3x35 = 0
# 4x24 - 1x46 - 4x47 = 0
# 2x25 + 3x25 - 6x57 - 2x58 = 0
# 1x46 - 5x69 = 0
# 4x47 + 6x57 - 4x79 = 0
# 2x58 - 7x89 = 0
# finishing node:
# 5x69 + 4x79 + 7x89 = 1

q1 <- make.lp(0,12)

set.objfn(q1, c(5,3,4,2,3,1,4,6,2,5,4,7))

lp.control(q1, sense = "max")

#starting node
add.constraint(q1, c(1,1,0,0,0,0,0,0,0,0,0,0), "=",1)
#intermediate nodes
add.constraint(q1, c(1,0,-1,-1,0,0,0,0,0,0,0,0), "=",0)
add.constraint(q1, c(0,1,0,0,-1,0,0,0,0,0,0,0), "=",0)
add.constraint(q1, c(0,0,1,0,0,-1,-1,0,0,0,0,0), "=",0)
add.constraint(q1, c(0,0,0,1,1,0,0,-1,-1,0,0,0), "=",0)
add.constraint(q1, c(0,0,0,0,0,1,0,0,0,-1,0,0), "=",0)
add.constraint(q1, c(0,0,0,0,0,0,0,1,1,0,-1,0), "=",0)
add.constraint(q1, c(0,0,0,0,0,0,0,0,1,0,0,-1), "=",0)
#finishing node
add.constraint(q1, c(0,0,0,0,0,0,0,0,0,1,1,1), "=",1)

set.bounds(q1, lower = c(0,0,0,0,0,0,0,0,0,0,0,0), columns = 1:12)

solve(q1)

get.objective(q1)
get.variables(q1)

#longest path is x12 -> x25 -> x57 -> x79

# Question 2

# Setting the stock price to a variable and formulating objective function
# to find 
# software companies
S1 <- as.integer(40) # rate .05 dividend 2
S1
S2 <- as.integer(50) # rate .1 + dividend 1.5
S2
S3 <- as.integer(80) # (80*.03) + 80 + 3.5
S3
# hardware companies
H1 <- as.integer(60) # (60*.04) + 60 + 3
H1
H2 <- as.integer(45) # (45*.07) + 45 + 2
H2
H3 <- as.integer(60) # (60*.15) + 60 + 1
H3
# consulting companies
C1 <- as.integer(30) # (30*.22) + 30 + 1.8
C1
C2 <- as.integer(25) # (25*.25) + 25 + 0
C2

x1 <- (.05*2) + (.05*S1)
x1
x2 <- (.1*1.5) + (.1*S2)
x2
x3 <- .03*3.5 + (.03*S3)
x3
x4 <- .04*3 + (.04*H1)
x4
x5 <-.07*2 + (.07*H2)
x5
x6 <-.15*1 + (.15*H3)
x6
x7 <-.22*1.8 + (.22*C1)
x7
x8 <-.25*0 + (.25*C2)
x8


2500000/1000
100000/1000
2500*.4

# Objective Function:
# Max x1+ x2+ x3+ x4+ x5+ x6+ x7+ x8
# Constraints
# minimum investment constraint for all x
# s1*x1 >= 100
# S2*x2 >= 100
# S3*x3 >= 100
# H1*x4 >= 100
# H2*x5 >= 100
# H3*x6 >= 100
# C1*x7 >= 100
# C2*x8 >= 100

# proportionality constraints
# s1*x1 + S2*x2 + S3*x3 <= 1000
# H1*x4 + H2*x5 + H3*x6 <= 1000
# C1*x7 + C2*x8 <=1000
# money constraint
# x1+x2+x3+x4+x5+x6+x7+x8 = 2500
m <- as.integer(2500)
typeof(m)
pc <- as.integer(1000)
pc
mi <- as.integer(100)
mi

q2 <- make.lp(0,8)


set.objfn(q2, c(x1,x2,x3,x4,x5,x6,x7,x8))

lp.control(q2, sense = "max")

q2

#minimum investment constraint
add.constraint(q2, c(S1,0,0,0,0,0,0,0), ">=",100)
add.constraint(q2, c(0,S2,0,0,0,0,0,0), ">=",100)
add.constraint(q2, c(0,0,S3,0,0,0,0,0), ">=",100)
add.constraint(q2, c(0,0,0,H1,0,0,0,0), ">=",100)
add.constraint(q2, c(0,0,0,0,H2,0,0,0), ">=",100)
add.constraint(q2, c(0,0,0,0,0,H3,0,0), ">=",100)
add.constraint(q2, c(0,0,0,0,0,0,C1,0), ">=",100)
add.constraint(q2, c(0,0,0,0,0,0,0,C2), ">=",100)
#proportionality constraints (.4*2500 = 1000)
add.constraint(q2, c(1,1,1,0,0,0,0,0), "<=",1000)
add.constraint(q2, c(0,0,0,1,1,1,0,0), "<=", 1000)
add.constraint(q2, c(0,0,0,0,0,0,1,1), "<=", 1000)
#money constraint
add.constraint(q2, c(1,1,1,1,1,1,1,1), "=", m)


solve(q2)

get.objective(q2)
get.variables(q2)

#without the 1000 share increment, the stocks invested in would be top heavy
mix1 <- 496.25/2500
mix2 <- 996.11/2500
mix3 <- 996/2500
mix1+mix2+mix3
#S2, H3 & C1 would would be 99.534% of total investment


q3 <- make.lp(0,8)


set.objfn(q3, c(x1,x2,x3,x4,x5,x6,x7,x8))

lp.control(q3, sense = "max")

q3

#minimum investment constraint
add.constraint(q3, c(S1,0,0,0,0,0,0,0), ">=",mi)
add.constraint(q3, c(0,S2,0,0,0,0,0,0), ">=",mi)
add.constraint(q3, c(0,0,S3,0,0,0,0,0), ">=",mi)
add.constraint(q3, c(0,0,0,H1,0,0,0,0), ">=",mi)
add.constraint(q3, c(0,0,0,0,H2,0,0,0), ">=",mi)
add.constraint(q3, c(0,0,0,0,0,H3,0,0), ">=",mi)
add.constraint(q3, c(0,0,0,0,0,0,C1,0), ">=",mi)
add.constraint(q3, c(0,0,0,0,0,0,0,C2), ">=",mi)
#proportionality constraints (.4*2500 = 1000)
add.constraint(q3, c(1,1,1,0,0,0,0,0), "<=", pc)
add.constraint(q3, c(0,0,0,1,1,1,0,0), "<=", pc)
add.constraint(q3, c(0,0,0,0,0,0,1,1), "<=", pc)
#money constraint
add.constraint(q3, c(1,1,1,1,1,1,1,1), "=", m)


solve(q3)

get.objective(q3)
get.variables(q3)

#solve for integers
2.1*2+5.15*496+2.505*2 > 2.1*3+5.15*496+2.505*1
#S1=2 & S3=2 is better than S1=3 & S3=1
2.52*2 + 3.29*2 + 9.15*996 > 2.52*1 + 3.29*3 + 9.15*996
#H1=1 & H2=3 is better than H1=2 & H3=2

#optimal solution using integers:
(2.1*2+5.15*496+2.505*2 + 2.52*1 + 3.29*3 + 9.15*996 + 6.996*996 + 6.25*4)
#18682.42
18683.01 - 18682.42
#The 1000 share constraint (Using integer values), the optimal solution is 
#.59(*1000) less