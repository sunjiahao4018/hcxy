dis<-function(data,data.score){
distance<-10000000000000
for(l in seq(nrow(data))){
distance_temp<-(data[l,'A']-as.numeric(data.score[1,'mean.A']))^2+
		(data[l,'B']-as.numeric(data.score[1,'mean.B']))^2+
		(data[l,'C']-as.numeric(data.score[1,'mean.C']))^2+
		(data[l,'D']-as.numeric(data.score[1,'mean.D']))^2+
		(data[l,'E']-as.numeric(data.score[1,'mean.E']))^2+

		(data[l,'YL']-as.numeric(data.score[1,'YL']))^2+

		(data[l,'AA']-as.numeric(data.score[1,'AA']))^2+
		(data[l,'A.1']-as.numeric(data.score[1,'A']))^2+
		(data[l,'AB']-as.numeric(data.score[1,'AB']))^2+
		(data[l,'B.2']-as.numeric(data.score[1,'B']))^2+
		(data[l,'BB.BC']-as.numeric(data.score[1,'BB']))^2+
		(data[l,'C.2']-as.numeric(data.score[1,'C']))^2+
		(data[l,'CC']-as.numeric(data.score[1,'CC']))^2+
		(data[l,'DD']-as.numeric(data.score[1,'DD']))^2+
		(data[l,'D.2']-as.numeric(data.score[1,'D']))^2+	
		(data[l,'DE.EE']-as.numeric(data.score[1,'DE']))^2+
		(data[l,'E.2']-as.numeric(data.score[1,'E']))^2+
		(data[l,'EF']-as.numeric(data.score[1,'EF']))^2+
		(data[l,'F']-as.numeric(data.score[1,'F']))^2

  if(distance_temp<distance) {
		count<-l
		distance<-distance_temp
		
	}
}
return(count)
}


#data.score<-last[1,]##一行打分结果数据

YZ<-function(data.score){
data.score$YL<-data.score$YL+0.1 #氧量区间（0.2）变中间点数值

for(j in names(data.score)[25:37]){##可能会改，风门区间（2）变中间点数值
data.score[,j]<-as.numeric(data.score[,j])+1
}
k<-c(108,117,122,131,140,150,161,167,175,185,196,207,219,229)
d<-c(451,536,609,690,797,860,956)
condition<-paste(k[as.numeric(data.score$V1)],"-",k[as.numeric(data.score$V1)+1],",",d[as.numeric(data.score$V2)],"-",d[as.numeric(data.score$V2)+1],f,sep='')

res<-NA
if(length(intersect(condition,dir("D:/合成兴业/珲春工况/")))==1){
data<-read.csv(paste("D:/合成兴业/珲春工况/",condition,sep=""))####

if(nrow(data)>=20){
scale1<-function(x) (x-min(x))/(max(x)-min(x)+0.0000001)#归一化
M1<-scale1(data$A);M2<-scale1(data$B)
M3<-scale1(data$C);M4<-scale1(data$D)
M5<-scale1(data$E);

F1<-scale1(data$AA);F2<-scale1(data$A.1)
F3<-scale1(data$AB);F4<-scale1(data$B.2)
F5<-scale1(data$BB.BC);F6<-scale1(data$C.2)
F7<-scale1(data$CC);F8<-scale1(data$DD)
F9<-scale1(data$D.2);F10<-scale1(data$DE.EE)
F11<-scale1(data$E.2);F12<-scale1(data$EF)
F13<-scale1(data$F)

d1<-cbind(M1,M2,M3,M4,M5)
d2<-cbind(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13)

#d1做主成分
pr1<-princomp(d1)
for(i in seq(ncol(d1))){#确定选取几个主成分
sum.zcf1<-sum(pr1$sdev[seq(i)]^2)/sum(pr1$sdev^2)
if(sum.zcf1>Threshold){
count1=i
break
}}
#做主成分
ZCF1<-rep(0,nrow(d1))
for(i in seq(count1)) {
 temp<-rep(0,nrow(d1))
  for(j in 1:ncol(d1)) temp<-temp+d1[,j]*pr1$loading[j,i]
  ZCF1<-cbind(ZCF1,temp)
}
ZCF1<-ZCF1[,-1]
##
pr2<-princomp(d2)
for(i in seq(ncol(d2))){#确定选取几个主成分
sum.zcf2<-sum(pr2$sdev[seq(i)]^2)/sum(pr2$sdev^2)
if(sum.zcf2>Threshold){
count2=i
break
}}
#做主成分
ZCF2<-rep(0,nrow(d2))
for(i in seq(count2)) {
 temp<-rep(0,nrow(d2))
  for(j in 1:ncol(d2)) temp<-temp+d2[,j]*pr2$loading[j,i]
  ZCF2<-cbind(ZCF2,temp)
}
ZCF2<-ZCF2[,-1]

##
ZCF<-cbind(ZCF1,ZCF2,scale1(data$YL))
 ###构建综合指标Y,越大越好

Y1 <- (data[, "nox进口A"]+data[,"NOX.B"])/2
Y2 <- data[, "再热器温度"]
Y3 <- data[, "排烟温度"]

Y <- scale1(Y2)*0.5 + ( 1 - scale1(Y1))*0.4  + (1 - scale1(Y3))*0.1
###建立函数dis，返回与data.score最近的工况数据序号
lm.sol<-lm(Y~ZCF)
res<-fitted(lm.sol)[dis(data,data.score)]
}
}
return(res)

}

###
files.l<-dir("C:/珲春寻优结果")
for(f in files.l){
data<-read.csv(paste("C:/珲春寻优结果/",f,sep=""))
for(z in seq(nrow(data))){
 data.score<-data[z,]
 data$huigui[z]<-YZ(data.score)
}
write.csv(data,paste("D:/交叉验证",f,sep=''))
}
data
dir.create("C:/123/")




