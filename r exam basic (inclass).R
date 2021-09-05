Q1
ans=>  
  a=seq(20,50)
a
b=mean(20:60)
b
c=seq(51,91)
sum(c)

#############################################################################

Q2
ans=>
  marks = c(70, 95, 80, 74)
barplot(marks,
        main = "Comparing  of marks of 5 subjects",
        xlab = "Marks",
        ylab = "Subjects",
        names.arg = c("English", "Science", "Maths", "History"),
        col = "yellow",
        horiz = FALSE)
########################################################################################
Q3
ans=>
  Employees = data.frame(Name=c("Anastasia S","Dima R","Katherine S", "JAMES A","LAURA MARTIN"),
                         Gender=c("M","M","F","F","M"),
                         Age=c(23,22,25,26,32),
                         Designation=c("Clerk","Manager","Exective","CEO","ASSISTANT"),
                         SSN=c("123-34-2346","123-44-779","556-24-433","123-98-987","679-77-576")
  )
print("Summary of the employees")                      
print(summary(Employees))



####################################################################
Q4
ans=>
  hetero_list = list(
    'name' = 'mayur', 
    'profession' = 'GYM trainer',
    'rank' = seq(1:5),
    'average' = round(runif(5,10,25),2),
    'flag' = TRUE
  )

print(hetero_list)
#####################################################################
Q5
ans=>
  mtx=matrix(1:12,3,4)
print(" matrix")
print(mtx)
dimarry = as.vector(mtx)
print("1 dimensional array:")
print(dimarry)
###############################################################
Q6
ans=>
  list_data = list(c("Red","Green","Black"), matrix(c(1,3,5,7,9,11), nrow = 2),
                   list("Python", "PHP", "Java"))
print("List")
print(list_data)
print("Add a new element at the end of the list")
list_data[4] = "New element:4"
print("New list")
print(list_data)





##################################################################################

Q7
ans=>
  list1=list(1,2,3)
list1
list2=list("Red","Green","Black")
list2
lst3=c(list1,list2)
print(lst3)
##################################3
Q8
ans=>
  exam_data = data.frame(
    name = c('Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas'),
    score = c(12.5, 9.0, 16.5, 12.0, 9.0, 20.0, 14.5, 13.5, 8.0, 19.0),
    attempts = c(1, 3, 2, 3, 2, 3, 1, 1, 2, 1),
    qualify = c('yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes')
  )
print("Original dataframe:")
print(exam_data)
new_list = split(exam_data, seq(nrow(exam_data)))
print("dataframe rows to a list")
print(new_list)
#########################################
exam = data.frame(
  name = c('Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas'),
  score = c(12.5, 9.0, 16.5, 12.0, 9.0, 20.0, 14.5, 13.5, 8.0, 19.0),
  attempts = c(1, 3, 2, 3, 2, 3, 1, 1, 2, 1),
  qualify = c('yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes')
)
print("Exam dataframe")
print(exam)
new_list = split(exam, seq(nrow(exam)))
print("list by rows")
print(new_list)

###################################
Q9
ans=>
  d = data.frame(x1=rnorm(5),
                 x2=rnorm(5),
                 x3=rnorm(5))
print("Original dataframe")   
print(d)
result = cor(d) 
print("Correlation matrix")
print(result)
######################################################
Q10
ans=>
  x =  matrix(1:9, 3)
print("Original  matrix")
print(x)
rotate = t(apply(x, 2, rev))
print("Rotate the matrix 90 degree clockwise:")
print(rotate)


####################################################################
Q11
ans=>
  View(mtcars)
# functions to check Nulls/0
checkNull=function(x) return(any(is.na(x)))
checkZero=function(x) return(any(x==0))

# EDA
colnames(mtcars)[apply(mtcars,2,checkNull)]
colnames(mtcars)[apply(mtcars,2,checkZero)]
#################################################

checkNull=function(x) return(any(is.na(x)))
checkZero=function(x) return(any(x==0))

# EDA
# EDA
colnames(mtcars)[apply(mtcars,2,checkNull)]
colnames(mtcars)[apply(mtcars,2,checkZero)]