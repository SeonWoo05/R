setwd("c:/R�� �Բ��ϴ� �ٺ����ڷ�м�/R_code_data")
Data<-read.table("censustract.txt", header=T)
X<-Data
X
round(cor(X), 3)
# PCA based on the SD using princomp( )
pca.R<-princomp(X, cor=T)
summary(pca.R, loadings=T) # ������, �ּ��а��
round(pca.R$scores, 3)  # �ּ�������
screeplot(pca.R, type="lines") # ��ũ���׸�

# �ּ��� ��ĵ�
biplot(pca.R, scale=0, xlab="1st PC",ylab="2nd PC",
                main="PC Biplot for Census Tract Data ")   
abline(v=0, h=0)