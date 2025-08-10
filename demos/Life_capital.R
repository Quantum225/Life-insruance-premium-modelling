#This version of the code simulates each simulation [ntries] times, and takes the 
#worst-case scenario (earliest death) out of [ntries] nested simulations, this increases runtime by
# [ntries] times, but simulates extreme cases, which could be useful for capital modelling.
#maybe setting ntries to 200 could simulate 'once in 200-year events'

#parameters:
MortR=c(0.004521, 0.000244, 0.000165, 0.000109, 0.000083, 0.000086, 0.00007, 0.000072, 0.000076, 0.000071, 0.000059, 0.000095, 0.000102, 0.000118, 0.000132, 0.000178, 0.00024, 0.00031, 0.000434, 0.000507, 0.000479, 0.000514, 0.00052, 0.000582, 0.000546, 0.000544, 0.000664, 0.000643, 0.000699, 0.000782, 0.000795, 0.000886, 0.000928, 0.000992, 0.00103, 0.001129, 0.00127, 0.001342, 0.001446, 0.001616, 0.001731, 0.001871, 0.00197, 0.002115, 0.002275, 0.002503, 0.00268, 0.002936, 0.003262, 0.003519, 0.003822, 0.004054, 0.004486, 0.004692, 0.005087, 0.005438, 0.005867, 0.006409, 0.00686, 0.007387, 0.008134, 0.009036, 0.009714, 0.010535, 0.0116, 0.012545, 0.013886, 0.015413, 0.016882, 0.018186, 0.019951, 0.021931, 0.024026, 0.025858, 0.028047, 0.031242, 0.035063, 0.038891, 0.043968, 0.048651, 0.055679, 0.062008, 0.070065, 0.076758, 0.086004, 0.096018, 0.107573, 0.120841, 0.136178, 0.151392, 0.170648, 0.187061, 0.209933, 0.229476, 0.252988, 0.276409, 0.297662, 0.32832, 0.339591, 0.378378, 0.401956,1)
nsim=1000 
i=0.05 
ntries=200

#initialise
age=1
death_age=numeric(nsim)
years_left=numeric(nsim)
DDay=numeric(102)
Years=numeric(102)
TRIES=numeric(ntries)

for (age in 0:101) {
  for (n in 1:nsim) {
    for (a in 1:ntries) {
      p=1 
      Death=1
      
      p=MortR[(age+1):length(MortR)]
      
      Death=rbinom(length(p),1,p)
      
      dec=seq(from=length(p),to=1,by=-1)
      
      Death*dec
      TRIES[a]=which.max(Death*dec)
    }
    years_left[n]=min(TRIES)-1
    death_age[n]=min(TRIES)+age-0.5
    
  }
  DDay[age+1]=mean(death_age)
  Years[age+1]=mean(years_left)
}
DDay
Years

v=1/(1+i)
P_age=(1-v)/(1-v^(Years+1))
P_age


barplot(Years,space=0,axes=FALSE,ylim=c(0,90))
axis(1,at=seq(0,100,10),cex.axis=0.9)
axis(2,at=seq(0,90,5),cex.axis=0.8)

barplot(P_age,space=0,axes=FALSE,ylim=c(0,1),xlab="current age",ylab="premium/year")
axis(1,at=seq(0,100,10),cex.axis=0.9)
axis(2,at=seq(0,1,0.05),cex.axis=0.9)