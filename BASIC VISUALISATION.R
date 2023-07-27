
#READING THE EXCEL SHEET

setwd("X:/")
getwd()
data=read.csv("Data Dissertation.csv");data
#View(data)

#--------------------------------

#BASIC VISUALISATION

table(data$Count)
table(data$Stream)

library(ggplot2)
theme_new=theme(
  
  plot.title =element_text(size=16,
                           hjust=0.5,face="bold"),
  
  plot.subtitle =element_text(size=14,
                              hjust=0.5,face="bold"),
  
  legend.title=element_text(size=10,hjust=0.5,
                            face="bold.italic"),
  
  axis.title=element_text(face="bold"),
  
  axis.text=element_text(face="bold")
  
)
z=data$Count
z=c(3 ,1, 1, 2, 4, 1, 2, 1, 1, 1, 2, 1, 1, 3, 2, 0, 1,
    1, 1, 4, 1, 3, 0, 1, 2, 2, 0, 2, 2, 2, 1, 4, 1, 2,
    1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 2, 1, 1
)
ggplot(NULL,aes(z))+
  geom_bar(col=5,fill=4)+theme_dark()+
  theme_new+
  labs(title="Plot of No.of Seminars attended vs no. of students",
       subtitle="Column Diagram",
       x="\nNo. of seminars attended",
       y="No. of students\n",
       col="Index")

x=0:20
y=c(34,5,8,15,16,15,8,6,7,0,3,1,0,0,0,1,0,0,0,0,1)
sum(y)
A=cbind(x,y);A

#GRAPHICAL VISUALISATION
#TYPE : COUNT DATA

plot(table(data$Count),type="h",lwd=15,col=5)

barplot(y,names.arg=x,ylim=c(0,35),xlim=c(0,25),
        col="#98AFC7",border="#98AFC7",
        main="Bar-Plot\nAttendance in Seminars",
        sub="Period : 2018-2021",
        xlab="No. of seminars attended",
        ylab="No. of Students")


#METHOD OF MOMENTS ESTIMATES

count=rep(x,times=y);count
table(count)
#MEAN AND VARIANCE OF THE COLLECTED SAMPLE
sample_mean=mean(count);sample_mean

sample_variance=var(count);sample_variance

library("VGAM")
#ESTIMATES  _ MME _ZIP _PIE ,LAMBDA

lambda_MME=sample_mean+(sample_variance/sample_mean)+1
lambda_MME

pie_MME=(sample_variance-sample_mean)/
  (sample_mean^2+
  (sample_variance-sample_mean));

#AIC_MME
AIC_MME_ZIP=-2*sum(dzipois(count,lambda_MME,
              pie_MME,log=T))+4;AIC_MME_ZIP


#GENERATING SAMPLES FROM THE ZIP DISTRIBUTION
#PARAMETERS ARE THE ESTIMATED BY METHOD OF MOMENTS

#library("VGAM")

sample_1=rzipois(100,lambda_MME,pstr0 = pie_MME)

table(sample_1)


#MAXIMUM LIKELIHOOD ESTIMATES
#(IGNORE THIS METHOD)

#ESTIMATES ARE SOLUTIONS OF THE TWO FUNCTIONS

fn_1=function(lambda_MLE,pie_MLE)
{
  n=length(count)
  num_0=length(count[count==0])
  
  (n*mean(count)/lambda_MLE)-
   n+num_0-(num_0*(1-pie_MLE)*exp(-lambda_MLE))/
    (pie_MLE+(1-pie_MLE)*exp(-lambda_MLE))
}

fn_2=function(lambda_MLE,pie_MLE)
{
  n=length(count)
  num_0=length(count[count==0])
  
  
    n-num_0-(num_0*(1-pie_MLE)*(1-exp(-lambda_MLE)))/
    (pie_MLE+(1-pie_MLE)*exp(-lambda_MLE))
}

#MAXIMUM LIKELIHOOD ESTIMATE (METHOD II)
#(METHOD USING)

count 
n=length(count)

y=length(count[count==0]);y   #NUMBER OF ZEROES IN THE SAMPLE

sample_no.zero=count[count!=0];sample_no.zero   #THE SAMPLE EXCLUDING 
#                                                THE ZEROES
x=sample_no.zero
count
#DEFINING THE LIKELIHOOD FUNCTION
Likelihood=function(l,p)
{
  
  -sum(dzipois(count,l,p,log=T))
}

k=mle(Likelihood,start=list(l=lambda_MME,p=pie_MME))
summary(k)

#ESTIMATES BY MLE _ZIP_PIE,LAMBDA
lambda_MLE=4.9292075;lambda_MLE
pie_MLE=0.2781128;pie_MLE

#COMPARISION OF MSE ESTIMATOR AND MLE ESTIMATOR


#library(VGAM)




#FITTING OF THE DISTRIBUTION WITH ZIP MODEL AND POISSON MODEL
#07.11.21
#MLE ESTIMATES USING POISSON DISTRIBUTION 
Likelihood=function(l)
{
  
  -sum(dpois(count,l,log=T))
}

k=mle(Likelihood,start=9) #9 is a arbitrary choice
k

#AIC AND ESTIMATES OBTAINED FOR POISSON DISTRIBUTION
summary(k) 

#ESTIMATE OF LAMBDA USING POISSON DISTRIBUTION

lambda_POISSON_MLE=3.558  


#FITTED VALUES
fit_POISSON_MLE=dpois(seq(0,20,1),lambda_POISSON_MLE);fit_POISSON_MLE

fit_ZIP_MLE=dzipois(seq(0,20,1),lambda_MLE,pie_MLE);fit_ZIP_MLE

fit_ZIP_MME=dzipois(seq(0,20,1),lambda_MME,pie_MME);fit_ZIP_MME

y=c(34,5,8,15,16,15,8,6,7,0,3,1,0,0,0,1,0,0,0,0,1)
ACTUAL_prob=y/sum(y);ACTUAL_prob             #ACTUAL DATA

#Graph of Poisson fitting
library("tidyr")
lit2=data.frame(0:20,Actual=ACTUAL_prob,Fitted=fit_POISSON_MLE)
lit2=pivot_longer(lit2,-1,"Index",values_to="v")
lit2
ggplot(lit2,aes(X0.20,v,fill=Index))+
  geom_bar(position="dodge",stat="identity")+
  labs(title="Fitting of Poisson Distribution",subtitle="Poisson(3.558)",x="No. of seminars attended",
       y="Probabilities")+theme_dark()+
  scale_fill_manual(values=c(9,3))

#Graph of ZIP Fitting

library("tidyr")
lit2=data.frame(0:20,Actual=ACTUAL_prob,Fitted=fit_ZIP_MME)
lit2=pivot_longer(lit2,-1,"Index",values_to="v")
lit2
ggplot(lit2,aes(X0.20,v,fill=Index))+
  geom_bar(position="dodge",stat="identity")+
  labs(title="Fitting of ZIP Distribution",subtitle="Parameters are estimated by MME Method",x="No. of seminars attended",
       y="Probabilities")+theme_dark()+
  scale_fill_manual(values=c(9,2))












A=rbind(ACTUAL_prob,fit_ZIP_MLE,fit_ZIP_MME,fit_POISSON_MLE)
barplot(A,beside = T,ylim=c(0,0.4),col=c(2,3,4,7),names.arg =0:20,main="
        FITTING OF ZIP AND POISSON DISTRIBUTION")
legend("topright",
       legend=c("ACTUAL DATA","ZIP(MLE ESTIMATOR)",
                "ZIP(MME ESTIMATOR)","POISSON DISTRIBUTION"),
       lty=c(1,1,1,1),col=c(2,3,4,7),
       title="INDEX",lwd=8)


#MODIFICATION 
#07.11.21

#METHOD OF MOMENTS ESTIMATE
lambda_POIS_MME=sum(y*(0:20))/sum(y);lambda_POIS_MME
se=sqrt(lambda_POIS_MME/sum(y));se

A=rbind(ACTUAL_prob,fit_ZIP_MME,fit_POISSON_MLE)
barplot(A,beside = T,ylim=c(0,0.4),col=c(2,3,4),names.arg =0:20,main="
        FITTING OF ZIP AND POISSON DISTRIBUTION.PARAMETERS ESTIMATED
        USING METHOD OF MOMENTS")
legend("topright",
       legend=c("ACTUAL DATA",
                "ZIP(MME ESTIMATOR)","POISSON (MME ESTIMATOR)"),
       lty=c(1,1),col=c(2,3,4),
       title="INDEX",lwd=8)


#METHOD OF MAXIMUM LIKELIHOOD ESTIMATE

A=rbind(ACTUAL_prob,fit_ZIP_MLE,fit_POISSON_MLE)
barplot(A,beside = T,ylim=c(0,0.4),col=c(2,3,4),names.arg =0:20,main="
        FITTING OF ZIP AND POISSON DISTRIBUTION.PARAMETERS ESTIMATED
        USING METHOD OF MAXIMUM LIKELIHOOD")
legend("topright",
       legend=c("ACTUAL DATA",
                "ZIP(MLE ESTIMATOR)","POISSON (MLE ESTIMATOR)"),
       lty=c(1,1),col=c(2,3,4),
       title="INDEX",lwd=8)


#Computation of SE from Fisher Information Matrix
#Let lambda=l ,pie=p ,Y= no. of Zeroes ,n=sample size

lambda_MLE=4.9292075;lambda_MLE
pie_MLE=0.2781128;pie_MLE

l=lambda_MLE
p=pie_MLE

n=120
Y=34
Xbar=(1-p)*l

I11=((Y*p*(1-p)*exp(-l))/(p+(1-p)*exp(-l))^2)-((n*Xbar)/l^2)
I12=(Y*exp(-l))/(p+(1-p)*exp(-l))^2
I22=-((Y*(1-exp(-l))^2)/(p+(1-p)*exp(-l))^2)-((n-Y)/(1-p)^2)

FISMAT=matrix(c(-I11,-I12,-I12,-I22),byrow=T,nrow=2)
A=solve(FISMAT)
A
SE_pie=sqrt(0.00171)
SE_lambda=sqrt(0.059)
SE_pie
SE_lambda

#Computation of SE of MME using Delta Method
x=0:20
f=c(34,5,8,15,16,15,8,6,7,0,3,1,0,0,0,1,0,0,0,0,1)

n=sum(f)
u1=sum(x*f)/sum(f);u1
u2=sum((x^2)*f)/sum(f);u2
u3=sum((x^3)*f)/sum(f);u3
u4=sum((x^4)*f)/sum(f);u4

var_t1=(u2-u1^2)/n
var_t2=(u4-u2^2)/n
cov_t1t2=(u3-u1*u2)/n

dl.dt1=-(u2/u1^2)
dl.dt2=1/u1

dp.dt1=(u1^2-2*u1*u2)/(u2-u1)^2
dp.dt2=(u1/(u2-u1))^2

var_pie.hat=var_t1*(dp.dt1)^2+
            var_t2*(dp.dt2)^2+
            2*cov_t1t2*(dp.dt1)*(dp.dt2);var_pie.hat

SE_piehat=sqrt(var_pie.hat)
SE_piehat


var_lambda.hat=var_t1*(dl.dt1)^2+
               var_t2*(dl.dt2)^2+
               2*cov_t1t2*(dl.dt1)*(dl.dt2);var_lambda.hat

SE_lambdahat=sqrt(var_lambda.hat)
SE_lambdahat

