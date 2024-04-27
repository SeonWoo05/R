# Nine Jobs's Dissimilarity
setwd("c:/R�� �Բ��ϴ� �ٺ����ڷ�м�/R_code_data")
Data7.3.1<-read.table("jobs.txt", header=T)
D<-as.matrix(Data7.3.1)
����=colnames(D)

# Nonlinear Metric MDS
win.graph()
library(MASS)
con<-sammon(D, k=2, magic=0.3)
con
x<-con$points[,1]
y<-con$points[,2]
lim1<-c(-max(abs(x)), max(abs(x)))
lim2<-c(-max(abs(y)), max(abs(y)))

plot(x,y, xlab="Dim1", ylab="Dim2", xlim=lim1, ylim=lim2)
text(x,y,����, cex=0.8, pos=3)
abline(v=0, h=0)