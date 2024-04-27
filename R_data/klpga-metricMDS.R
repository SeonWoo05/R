setwd("c:/R�� �Բ��ϴ� �ٺ����ڷ�м�/R_code_data")
Data1.3.2<-read.table("klpga.txt", header=T)
X<-scale(Data1.3.2, scale=T)
X<-as.matrix(Data1.3.2)
����<-rownames(X) 

# ǥ��ȭ ��Ŭ����Ÿ�
D <- as.matrix(dist(X, method="euclidean"))

win.graph()
# Metric MDS
con<-cmdscale(D, k=2, eig=T)
con
x<-con$points[,1]
y<-con$points[,2]
lim1<-c(-max(abs(x)), max(abs(x)))
lim2<-c(-max(abs(y)), max(abs(y)))
plot(x,y, xlab="Dim1", ylab="Dim2", xlim=lim1, ylim=lim2)
text(x,y, ����, cex=0.8, pos=1)
abline(v=0, h=0)