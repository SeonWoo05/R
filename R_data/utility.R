# AMCA : Ward Linkage for US Public Utilities
setwd("c:/R�� �Բ��ϴ� �ٺ����ڷ�м�/R_code_data")
Data5.3.1<-read.table("utility.txt", header=T)
X<-Data5.3.1[,-1]
X<-as.matrix(Data5.3.1[,-1])
ȸ��=Data5.3.1[,1]

n<-nrow(X)
xbar<-t(X)%*%matrix(1,n,1)/n # ��պ���
I<-diag(n)
J<-matrix(1,n,n)
H<-I-1/n*J                  # �߽�ȭ���
Y<-H%*%X                 # �߽�ȭ �ڷ����
S<-t(Y)%*%Y/(n-1)          # ���л���� 
D<-diag(1/sqrt(diag(S)))     # ǥ����������� ��
Z<-Y%*%D                # ǥ��ȭ�ڷ����
colnames(Z)<-colnames(X)



# ǥ��ȭ ��Ŭ����Ÿ�
ds <- as.matrix(dist(Z, method="euclidean"))
ds <- as.dist(ds)
round(ds, 3)
wards=hclust(ds, method="ward")
plot(wards, labels=ȸ��, hang=-1,  main=" (a) Ward Linkage : Standardized Euclidean Distance")


# ���Ҷ��񽺰Ÿ�
library(biotools)
dm<-D2.dist(X, S)
round(sqrt(dm), 3)
wardm=hclust(ds, method="ward")
plot(wardm, labels=ȸ��, hang=-1,main="(b) Ward Linkage : Mahalanobis Distance")