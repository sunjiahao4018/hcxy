files<-dir("D:/合成兴业/珲春/回归数据")
#f1<-function(data_new,Threshold,Druck,Weight.wwt,Weight.nox){#参数为寻优指标和工况和主成分阀值，data.new需带列名
#condition<-files[1]      #返回寻优结果
Weight.wwt<-0.5
Weight.nox<-0.4
Weight.rt<-0.1
Threshold<-0.9
# s.1<-c()
# s.2<-c()
R<-c()
can<-c()

for(condition in files){
result1=NULL

#
data<-read.csv(paste("D:/合成兴业/珲春/回归数据/",condition,sep=''))###
#


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
Y <- scale1(Y2)*Weight.wwt +(1-scale1(Y1))*Weight.nox + (1-scale1(Y3))*Weight.rt

lm.sol<-lm(Y~ZCF)
coeff.sd<-summary(lm.sol)$coeff[, "Std. Error"]#回归方程各个系数的标准差
COR<-cor(lm.sol$residuals,scale1(Y))#残差与Y的相关系数
R_Squared<-(summary(lm.sol))$r.squared
#if(R_Squared>=0.6){
 fit<-fitted(lm.sol)
 y_hat<-rev(sort(fit))[1:5]#最优的5个值
 y_hat_back<-y_hat*(max(Y)-min(Y))+min(Y)
residuals.sd<-1.96*sd(lm.sol$residuals)
residuals.sd_back<-residuals.sd*(max(Y)-min(Y))
hat<-cbind(data[as.numeric(names(y_hat)),],y_hat_back,y_hat) 
#得出5个最优中与第一个最优距离在2倍残差标准差之内的数据
result<-hat[which(hat$y_hat>=y_hat[1]-sd(lm.sol$residuals) & hat$y_hat<=y_hat[1]+sd(lm.sol$residuals)),]
##计算与data_new距离最近的最优


r1<-result$y_hat_back-residuals.sd_back
r2<-result$y_hat_back+residuals.sd_back

number<-rep(nrow(data),nrow(result))
##
# s.1<-c(s.1,sum.zcf1)
# s.2<-c(s.2,sum.zcf2)
R<-c(R,R_Squared)
can<-c(can,sd(lm.sol$residuals))
##

pinshu<-c()
for(j in 1:nrow(result)){
best<-result[j,]
data.pinshu<-data
for(i in names(data)[c(20:24,30:42)]){
data.pinshu<-subset(data.pinshu,data.pinshu[,i]>=best[,i]-1 & data.pinshu[,i]<=best[,i]+1)
pinshu[j]<-nrow(data.pinshu)
}
}
sd.res<-1.96*sd(lm.sol$residuals)
result1<-cbind(number,pinshu,result,r1,r2,sd.res)
names(result1)<-c("工况数据量","寻优结果频数",names(result1)[-(1:2)])
write.csv(result1,paste("D:/合成兴业/珲春/回归结果(0.5WWT-0.4NOX-0.1RT)/",condition,sep=''),row.names=F)
}

plot(R,main="各个工况R_Square")
plot(can,main="各个工况残差标准差")

###测试
data<-read.csv("D:/合成兴业/珲春/珲春原始数据/2016.9.1-10.11.csv")
dim(data)
data<-na.omit(data)

condition<-function(data.new){
result<-'回归'
k<-c(108,117,122,131,140,150,161,167,175,185,196,207,219,229)
d<-c(451,536,609,690,797,860,956)


for(j in 1:(length(k)-1)){
if(data.new['总煤量']>=k[j]&data.new['总煤量']<k[j+1])  result<-paste(result,paste(k[j],k[j+1],sep='-'),sep='')
}
for(i in 1:(length(d)-1)){
if(data.new['主汽流量']>=d[i]&data.new['主汽流量']<d[i+1])  result<-paste(result,',',paste(d[i],d[i+1],sep='-'),sep='')
}

result<-paste(result,unlist(data.new['X.1']),sep='')

return(result)
}

#
test<-c()
last<-c()
for(i in 15000:30000){
data.new<-data[i,]
cond<-condition(data.new)
if(length(intersect(paste(cond,".csv",sep=''),dir("D:/合成兴业/珲春/回归结果WWT")))==1){
result<-read.csv(paste("D:/合成兴业/珲春/回归结果WWT/",cond,".csv",sep=''))

res.scale<-read.csv(paste("D:/合成兴业/珲春/回归数据/",cond,".csv",sep=''))
nox.min<-min((res.scale[,"nox进口A"]+res.scale[,"NOX.B"])/2)
nox.max<-max((res.scale[,"nox进口A"]+res.scale[,"NOX.B"])/2)
wwt.min<-min(res.scale[,"再热器温度"])
wwt.max<-max(res.scale[,"再热器温度"])
rt.min<-min(res.scale[,"排烟温度"])
rt.max<-max(res.scale[,"排烟温度"])
##计算与data_new距离最近的最优
best<-result[1,]
distance<-10000000000000
 #data_new<-t(as.matrix(data_new))

for(l in seq(nrow(result))){
distance_temp<-(result[l,'A']-as.numeric(data.new[1,'A']))^2+
		(result[l,'B']-as.numeric(data.new[1,'B']))^2+
		(result[l,'C']-as.numeric(data.new[1,'C']))^2+
		(result[l,'D']-as.numeric(data.new[1,'D']))^2+
		(result[l,'E']-as.numeric(data.new[1,'E']))^2+

		(result[l,'AA']-as.numeric(data.new[1,'AA']))^2+
		(result[l,'A.1']-as.numeric(data.new[1,'A.1']))^2+
		(result[l,'AB']-as.numeric(data.new[1,'AB']))^2+
		(result[l,'B.2']-as.numeric(data.new[1,'B.2']))^2+
		(result[l,'BB.BC']-as.numeric(data.new[1,'BB.BC']))^2+
		(result[l,'C.2']-as.numeric(data.new[1,'C.2']))^2+
		(result[l,'CC']-as.numeric(data.new[1,'CC']))^2+
		(result[l,'DD']-as.numeric(data.new[1,'DD']))^2+
		(result[l,'D.2']-as.numeric(data.new[1,'D.2']))^2+	
		(result[l,'DE.EE']-as.numeric(data.new[1,'DE.EE']))^2+
		(result[l,'E.2']-as.numeric(data.new[1,'E.2']))^2+
		(result[l,'EF']-as.numeric(data.new[1,'EF']))^2+
		(result[l,'F']-as.numeric(data.new[1,'F']))^2

  if(distance_temp<distance) {
		best<-result[l,]
		distance<-distance_temp
		
	}
}
test<-rbind(test,cbind(data.new,nox.min,nox.max,wwt.min,wwt.max,rt.min,rt.max))
last<-rbind(last,best)
}
}
dim(test)
dim(last)

write.csv(last,"D:/合成兴业/珲春/寻优结果RT.csv")
write.csv(test,"D:/合成兴业/珲春/测试数据RT.csv")

####
test.nox<-((test[,'nox进口A']+test[,'NOX.B'])/2-test[,'nox.min'])/(test[,'nox.max']-test[,'nox.min'])
test.wwt<-(test[,'再热器温度']-test[,'wwt.min'])/(test[,'wwt.max']-test[,'wwt.min'])
test.rt<-(test[,'排烟温度']-test[,'rt.min'])/(test[,'rt.max']-test[,'rt.min'])
y.test<-0*test.wwt+0*(1-test.nox)+1*(1-test.rt)


plot(test.nox,ylab='NOX',type='l',col='green',ylim=c(-1.2,2))
lines(1-last$y_hat,type='l',col='red')
lines((1-last$y_hat)+last$sd.res,type='l',col='blue')
lines((1-last$y_hat)-last$sd.res,type='l',col='blue')
legend('bottom',legend=c("寻优结果","置信区间上（下）限","测试数据"),col=c('red','blue','green'),lty=1)
title("测试数据及寻优结果的综合指标（NOX）对比")




