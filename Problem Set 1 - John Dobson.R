# John Dobson

rm(list = ls())

#1.

A=matrix(c(1,-4,9,2,3,4,1,1,-2,2,4,8,8,-3,2,2),nrow=4,ncol=4)
A

#2. creating the b vector
b=c(1,1,1,1)
b

#3. solve(A) yields/displays inverse matrix
solve(A)
View(solve(A))

#4. solving augmented matrix A with b will yield x vector
x<-solve(A,b)
View(x)

#5.Triangular numbers are a series of numbers obtained by a continued sum of the natural numbers
#[(20*21)/2]=210
n=1:20
t<-n*(n+1)/2
t

#6.# assigned each element a corresponding letter using name function. t to demonstrate function worked
names(t)<-letters[1:20]
t

#7. vowels a,e,i,and o are elements 1,5,9,and 15
# tried creating a vowels vector c("a","e","i","o","u") and find
#which elements in t==vowels, but was missing intuition for which
vowels=c("a","e","i","o","u")
t[vowels]

#8. here a remainder means the element in t is not divisible by 3
# using (t%%3)==0 will yield true when remainder is 0 and false otherwise
(t%%3)==0
