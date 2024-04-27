# K-Means & K-Medoids(Partitioning Around Medoids)CA for Economic Views
setwd("c:/R�� �Բ��ϴ� �ٺ����ڷ�м�/R_code_data")
Data1.3.5<-read.table("economicview.txt", header=T)
X<-Data1.3.5[,-1]
X<-as.matrix(Data1.3.5[,-1])
Z<-scale(X)
���=Data1.3.5[,1]


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

library("NbClust")
ncluster<-NbClust(Z, distance="euclidean", min.nc = 2, max.nc = 8,
 method = "kmeans", index = "alllong", alphaBeale = 0.1)

# K-means Method
kmeans <- kmeans(Z, 4) # 4 cluster solution
cluster=data.frame(���,cluster=kmeans$cluster)
C1=cluster[(cluster[,2]==1),]
C2=cluster[(cluster[,2]==2),]
C3=cluster[(cluster[,2]==3),]
C4=cluster[(cluster[,2]==4),]
C1;C2;C3;C4

# Get cluster means 
aggregate(X, by=list(kmeans$cluster),FUN=mean)

# K-medoids Method
library(cluster)
kmedoids <- pam(Z, 4, metric="euclidean") # 4 cluster solution
cluster=data.frame(���,cluster=kmedoids$cluster)
C1=cluster[(cluster[,2]==1),]
C2=cluster[(cluster[,2]==2),]
C3=cluster[(cluster[,2]==3),]
C4=cluster[(cluster[,2]==4),]
C1;C2;C3;C4
# Get cluster means 
aggregate(X,by=list(kmedoids$cluster),FUN=mean) 