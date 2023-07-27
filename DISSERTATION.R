
#---------------------------------------------
#---------------------------------------------

            #READING THE EXCEL SHEET

setwd("X:/")
getwd()
data=read.csv("Data Dissertation.csv");data

#--------------------------------------------
#--------------------------------------------

             #Calling Libraries

library(ggplot2)
library(VGAM)
library(tidyr)

#--------------------------------------------
#--------------------------------------------

              #Data Visualisation

table(data$Count)
table(data$Stream)

#Defining theme for plots
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
ggplot(NULL,aes(z))+
  geom_bar(col=5,fill=3)+theme_dark()+
  theme_new+
  labs(title="Plot of No.of Seminars attended 
       vs no. of students",
       subtitle="Column Diagram",
       x="\nNo. of seminars attended",
       y="No. of students\n",
       col="Index")

x=0:20
y=c(34,5,8,15,16,15,8,6,7,0,3,1,0,0,0,1,
    0,0,0,0,1)
sum(y)
A=cbind(x,y);A

#--------------------------------------------------------
#--------------------------------------------------------
#METHOD OF MOMENTS ESTIMATES

count=rep(x,times=y);count
table(count)
#MEAN AND VARIANCE OF THE COLLECTED SAMPLE
sample_mean=mean(count);sample_mean

sample_variance=var(count);sample_variance

#--------------------------------------------------------
#--------------------------------------------------------

#ESTIMATES  _ MME _ZIP _PIE ,LAMBDA

lambda_MME=sample_mean+(sample_variance/sample_mean)+1
lambda_MME

pie_MME=(sample_variance-sample_mean)/
  (sample_mean^2+
     (sample_variance-sample_mean));

#---------------------------------------------------------
#---------------------------------------------------------

#ESTIMATES  _ MLE _ZIP _PIE ,LAMBDA

count 
n=length(count)

y=length(count[count==0]);y   #NUMBER OF ZEROES IN THE SAMPLE

sample_no.zero=count[count!=0];sample_no.zero #THE 
                       #SAMPLE EXCLUDING THE ZEROES
x=sample_no.zero
count

#DEFINING THE LIKELIHOOD FUNCTION

Likelihood=function(l,p)
{
  
  -sum(dzipois(count,l,p,log=T))
}

k=mle(Likelihood,start=list(l=lambda_MME,p=pie_MME))
summary(k)

#-------------------------------------------------------
#-------------------------------------------------------

#MLE ESTIMATES USING POISSON DISTRIBUTION 

Likelihood=function(l)
{
  
  -sum(dpois(count,l,log=T))
}

k=mle(Likelihood,start=9) #9 is a arbitrary choice
k

#--------------------------------------------------------
#--------------------------------------------------------

#FITTING OF ZIP AND POISSON DISTRIBUTION

fit_POISSON_MLE=dpois(seq(0,20,1),lambda_POISSON_MLE);fit_POISSON_MLE

fit_ZIP_MLE=dzipois(seq(0,20,1),lambda_MLE,pie_MLE);fit_ZIP_MLE

fit_ZIP_MME=dzipois(seq(0,20,1),lambda_MME,pie_MME);fit_ZIP_MME

y=c(34,5,8,15,16,15,8,6,7,0,3,1,0,0,0,1,0,0,0,0,1)
ACTUAL_prob=y/sum(y);ACTUAL_prob             #ACTUAL DATA

#Graph of Poisson fitting

lit2=data.frame(0:20,Actual=ACTUAL_prob,Fitted=fit_POISSON_MLE)
lit2=pivot_longer(lit2,-1,"Index",values_to="v")
lit2
ggplot(lit2,aes(X0.20,v,fill=Index))+
  geom_bar(position="dodge",stat="identity")+
  labs(title="Fitting of Poisson Distribution",
       subtitle="Poisson(3.558)",x="No. of seminars attended",
       y="No. of students")+theme_dark()+
  scale_fill_manual(values=c(9,3))


#Graph of ZIP Fitting by MLE Method


lit2=data.frame(0:20,Actual=ACTUAL_prob,Fitted=fit_ZIP_MLE)
lit2=pivot_longer(lit2,-1,"Index",values_to="v")
lit2
ggplot(lit2,aes(X0.20,v,fill=Index))+
  geom_bar(position="dodge",stat="identity")+
  labs(title="Fitting of ZIP Distribution",
       subtitle="Parameters are estimated by MLE Method",
       x="No. of seminars attended",
       y="No. of students")+theme_dark()+
  scale_fill_manual(values=c(9,7))

#Graph of ZIP Fitting by MME Method

lit2=data.frame(0:20,Actual=ACTUAL_prob,Fitted=fit_ZIP_MME)
lit2=pivot_longer(lit2,-1,"Index",values_to="v")
lit2
ggplot(lit2,aes(X0.20,v,fill=Index))+
  geom_bar(position="dodge",stat="identity")+
  labs(title="Fitting of ZIP Distribution",
       subtitle="Parameters are estimated by MME Method",
       x="No. of seminars attended",
       y="No. of students")+theme_dark()+
  scale_fill_manual(values=c(9,7))

#----------------------------------------------------------------
#----------------------------------------------------------------

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

#----------------------------------------------------------------
#----------------------------------------------------------------

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

#---------------------------------------------------------
#---------------------------------------------------------