setwd("c:/R�� �Բ��ϴ� �ٺ����ڷ�м�/R_code_data")
Data1.1.1<-read.table("3subjects.txt", header=T)
X<-Data1.1.1
X
X<-as.matrix(X)              # �ڷ����
n<-nrow(X)
xbar<-t(X)%*%matrix(1,n,1)/n # ��պ���
I<-diag(n)
J<-matrix(1,n,n)
H<-I-1/n*J                # �߽�ȭ���
Y<-H%*%X                  # �߽�ȭ �ڷ����
S<-t(Y)%*%Y/(n-1)         # ���л���� 
D<-diag(1/sqrt(diag(S)))  # ǥ����������� ��
Z<-H%*%X%*%D              # ǥ��ȭ�ڷ����
colnames(Z)<-colnames(X)
R<-t(Z)%*%Z/(n-1)         # ������
R_S<-D%*%S%*%D           # �����İ� ���л������ ����� ����
detS<-det(S)              # �Ϲ�ȭ�л�� �Ѻл�
detR<-det(R)
trS<-sum(diag(S))
trR<-sum(diag(R))
# ��� ���
X; xbar; Y; Z; S; R; detS; trS; detR; trR