#Quadratic DA for two groups(setosa, vesicolor)
#   on two variables(�ɹ�ħ����,�ɹ�ħ��) 
library(MASS)
library(MVN)

setwd("c:/R�� �Բ��ϴ� �ٺ����ڷ�м�/R_code_data")
setosa_versi<-read.table("setosaversicolor.txt", header=T)
attach(setosa_versi)



# MVN tests based on the Skewness and Kurtosis Statistics
par(mfrow=c(1,2))
setosa=setosa_versi[1:50, 1:2]
versicolor=setosa_versi[51:100, 1:2]
setosa
versicolor
result_setosa = mardiaTest(setosa, qqplot = TRUE)
result_versicolor = mardiaTest(versicolor, qqplot = TRUE)
result_setosa
result_versicolor


# Quadratic DA with 3 groups applying 
 # resubstitution prediction and equal prior probabilities.
QDA=qda(����~�ɹ�ħ����+�ɹ�ħ��, data=setosa_versi, prior=c(1,1)/2)
QDA
qcluster=predict(QDA, setosa_versi)$class
qct=table(����, qcluster)
qct
# Total percent correct
mean(����==qcluster)
