setwd("c:/R�� �Բ��ϴ� �ٺ����ڷ�м�/R_code_data")
Data1.1.5<-read.table("irisflower.txt", header=T)
X<-Data1.1.5[, -1]
# Box Plot
par(mfrow=c(2,2)) 
boxplot(�ɹ�ħ����~group, data=X, xlab="�ײ� ����", ylab="X1: �ɹ�ħ ����")
boxplot(�ɹ�ħ��~group, data=X, xlab="�ײ� ����", ylab="X2: �ɹ�ħ ��")
boxplot(���ٱ���~group, data=X, xlab="�ײ� ����", ylab="X3: ���� ����")
boxplot(������~group, data=X, xlab="�ײ� ����", ylab="X4: ���� ��")

# Multiple Scatter Plot
plot(X[,1:4], pch=unclass(X[,5]), col=1:3)

