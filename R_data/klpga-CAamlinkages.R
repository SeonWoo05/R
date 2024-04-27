# AMCA : AM Linkages
setwd("c:/R�� �Բ��ϴ� �ٺ����ڷ�м�/R_code_data")
Data1.3.2<-read.table("klpga.txt", header=T)
X<-Data1.3.2
X<-as.matrix(Data1.3.2)
����<-rownames(X) 

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
#���Ͽ����
sinle=hclust(ds, method="single")
plot(sinle, labels=����, hang=-1, main="(a) Sinle Linkage")
#���������
complete=hclust(ds, method="complete")
plot(complete, labels=����, hang=-1, main="(b) Complete Linkage")
#��տ����
average=hclust(ds, method="average")
plot(average, labels=����, hang=-1, main="(c) Average Linkage")
#�͵忬���
ward=hclust(ds, method="ward")
plot(ward, labels=����, hang=-1, main="(d) Ward Linkage")
