#[���� 7.2.5] Bank data�� �跮�� MDS  
# Dissimilarity Matrix from Binary Data
setwd("c:/R�� �Բ��ϴ� �ٺ����ڷ�м�/R_code_data")
Data7.2.2<-read.table("bankbinary.txt", header=T)
X<-Data7.2.2[, -1]
����<-Data7.2.2[, 1]
n<-nrow(X)
p<-ncol(X)

# Dissimilarity Matrix from Binary Data
m <-as.matrix(dist(X, method="euclidean", diag=T))
D<-round(m^2, 3)/p

# Metric MDS
win.graph()
con<-cmdscale(D, k=2, eig=T)
con
x<-con$points[,1]
y<-con$points[,2]
lim<-c(-max(abs(con$points)), max(abs(con$points)))
plot(x,y, xlab="Dim1", ylab="Dim2", xlim=lim, ylim=lim)
text(x,y,����, cex=0.8, pos=1)
abline(v=0, h=0)