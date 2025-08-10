#Parameters(3):~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MortR=c(0.004521, 0.000244, 0.000165, 0.000109, 0.000083, 0.000086, 0.00007, 0.000072, 0.000076, 0.000071, 0.000059, 0.000095, 0.000102, 0.000118, 0.000132, 0.000178, 0.00024, 0.00031, 0.000434, 0.000507, 0.000479, 0.000514, 0.00052, 0.000582, 0.000546, 0.000544, 0.000664, 0.000643, 0.000699, 0.000782, 0.000795, 0.000886, 0.000928, 0.000992, 0.00103, 0.001129, 0.00127, 0.001342, 0.001446, 0.001616, 0.001731, 0.001871, 0.00197, 0.002115, 0.002275, 0.002503, 0.00268, 0.002936, 0.003262, 0.003519, 0.003822, 0.004054, 0.004486, 0.004692, 0.005087, 0.005438, 0.005867, 0.006409, 0.00686, 0.007387, 0.008134, 0.009036, 0.009714, 0.010535, 0.0116, 0.012545, 0.013886, 0.015413, 0.016882, 0.018186, 0.019951, 0.021931, 0.024026, 0.025858, 0.028047, 0.031242, 0.035063, 0.038891, 0.043968, 0.048651, 0.055679, 0.062008, 0.070065, 0.076758, 0.086004, 0.096018, 0.107573, 0.120841, 0.136178, 0.151392, 0.170648, 0.187061, 0.209933, 0.229476, 0.252988, 0.276409, 0.297662, 0.32832, 0.339591, 0.378378, 0.401956)
#Mortality rate for ages 1-101 i.e. q1,q2,...,q101
#must format so that the first position in the vector represents the mortality
#rate for going from 0 to 1 year old, position 2 for rate going from 1 to 2 yr old etc.
#The data in use currently is from the ONS, the mortality rates for males in the UK

RI=c(rep(0.98,69),rep(0.982,5),rep(0.985,5),rep(0.986,5),rep(0.992,10),rep(0.995,7))
#Rate of improvement for each age, i.e. r1,r2,...,r101
#For now these are placeholders, these would need to be calculated/estimated from past data
age=70
Delay=1
#From how many years ago was the mortality rate recorded? If a year ago, set Delay=1

#Calculating Mortality rates adjusted for Improvement rates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RI1=RI[(age+1):length(RI)]
#Indexes the rate of improvements relevant for calculation
#c(r71,...,r101)

RI2=1  #Initialise

#This for loop is to create the vector c(r71^1,r72^2,...,r101^31) 
for (n in 1:(length(RI)-age)) {
  RI2[n]=RI1[n]^(n+Delay)
}

AdMortR=1
#We want to create vector, c(q71*r71,q72*r72^2,...,q101*r101^31)
AdMortR=c(RI2*MortR[(age+1):101],1)
AdMortR
#This represents the adjusted mortality rate for a person of stated age
#A one is added in the end to make sure that a person doesn't just live forever
#Not enough data, so assume person dies after year 101


#PMF & CDF~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Now we construct our PMF, which should be the vector:
#c(q71*r71,(1-q71)q72*r72^2,...,(1-q71r71)(1-q72r72^2)...(1-q100r100^30)q101r101^31,(1-q71r71)(1-q72r72^2)...(1-q100r100^30)(1-q101r101^31))

neg=1-AdMortR
#This is the vector c(1-q71r71,1-q72r72^2,....,1-q101r101^31)
if (n==1) NULL else neg[(age+1):(age+n-1)]
#This indexes c(1-q71r71,....,1-q(70+n)r(70+n)^n), NULL if n=1

PMF=1 #initialise
for (n in 1:length(AdMortR)) {
  PMF[n]=AdMortR[n]*prod(if (n<=1) NULL else neg[1:(n-1)])
}
#Calculates PMF where PMF[n] holds the probability of dying in (n-1,n] years



CDF=1
for (n in 1:length(PMF)) {
  CDF[n]=sum(PMF[1:n])
}
#Calculates CDF from PDF


#Barplots~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

barplot(CDF,space=0,main='CDF',ylab='p',xlab='years left') #plots CDF
axis(1,at=0:length(CDF),cex.axis=0.5)

barplot(PMF,space=0,main='PMF',ylab='p',xlab='years left') #plots PMF
axis(1,at=0:length(PMF),cex.axis=0.5)
#Note that the final bar for reaching age 102 is higher than age 101, that is because
#the last bar represents the probability of reaching age 102+ not just 102
#The later calculations will consider this as 102, not 102+ though


#Percentiles & Mean~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ExEx=sum(seq(age+1,length(MortR)+1)*PMF)-age
ExEx
#This calculates expected life expectancy, i.e. the mean years left to live

q=0.95
which(CDF>=q)[1]
#Shows q percentile life-expectancy

p=23
CDF[p]
#Shows which percentile is having life expectancy of p


#When we plot a graph of PMF for all ages, we notice a jump of PMF from age 84 to age 85
#After some investigation, this is due to a significant increase for both Mortality rate and 
#Improvement rate between these two ages, which combine together to have a large effect in final results
age
MortR[82:86]
AdMortR[(82-age):(86-age)]
RI[82:86]
RI1[(82-age):(86-age)]

length(PMF)

#Annuity Calculation~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

i=0.05
v=1/(1-i)
DR=1
for (n in 1:length(AdMortR)) {
  DR[n]=v^n
}
DR
#DR=(v,v^2,v^3,...,v^(102-age))

neg2=c(1,1-CDF[1:(length(CDF)-1)]) #This vector represents probability of surviving [n] years
neg2
CDF

EPVb=sum(DR*PMF)
#Expected present value of the benefits where benefits is $1 awarded at death
EPVp=sum(DR/v*(neg2))
#Expected present value of premiums

Pm=EPVb/EPVp
Pm
#Annuity premium for interest rate [i], calculated using EPV method
Pn=if (v==1) v^ExEx/(floor(ExEx)+1) else (1-v)/(1-v^(floor(ExEx)+1))*v^(ExEx)
Pn
#Calculated using NPV=0 method, using expected life expectancy at age, [age]

#Pm and Pn represents the annuity paid for each $1 of benefit
