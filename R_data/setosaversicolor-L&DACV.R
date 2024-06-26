# Linear and Quadratic DA for two groups(setosa, vesicolor)
#   on two variables(�ɹ�ħ����,�ɹ�ħ��) 

setwd("c:/R�� �Բ��ϴ� �ٺ����ڷ�м�/R_code_data")
setosa_versi<-read.table("setosaversicolor.txt", header=T)
attach(setosa_versi)
plot(setosa_versi[, 1:2], pch=unclass(����), col=1:2)
library(MASS)
# Density function of Variables
ldahist(data= �ɹ�ħ����, g=����, type="density")
ldahist(data= �ɹ�ħ��, g=����, type="density")

# Linear DA
LDA=lda(����~�ɹ�ħ����+�ɹ�ħ��, data=setosa_versi)
LDA
lcluster=predict(LDA, setosa_versi)$class
lct=table(����, lcluster)
lct
# Total percent correct
mean(����==lcluster)

# Linear Discriminant Analysis with Jacknifed Prediction
LDACV=lda(����~�ɹ�ħ����+�ɹ�ħ��, data=setosa_versi, CV=TRUE)
LDACV
names(LDACV)
cltcv=table(����, LDACV$class)
cltcv


# Quadratic DA with 3 groups applying 
 # resubstitution prediction and equal prior probabilities.
QDA=qda(����~�ɹ�ħ����+�ɹ�ħ��, data=setosa_versi, prior=c(1,1)/2)
QDA
qcluster=predict(QDA, setosa_versi)$class
qct=table(����, qcluster)
qct
# Total percent correct
mean(����==qcluster)


