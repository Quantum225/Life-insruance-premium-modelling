#We want to use Monte Carlo simulations to calculate the expected years left of a person for every age
#Then using that data, calculate how much that person's premium must be for a company to breakeven
#given an effective annual interest rate

MortR=c(0.004521, 0.000244, 0.000165, 0.000109, 0.000083, 0.000086, 0.00007, 0.000072, 0.000076, 0.000071, 0.000059, 0.000095, 0.000102, 0.000118, 0.000132, 0.000178, 0.00024, 0.00031, 0.000434, 0.000507, 0.000479, 0.000514, 0.00052, 0.000582, 0.000546, 0.000544, 0.000664, 0.000643, 0.000699, 0.000782, 0.000795, 0.000886, 0.000928, 0.000992, 0.00103, 0.001129, 0.00127, 0.001342, 0.001446, 0.001616, 0.001731, 0.001871, 0.00197, 0.002115, 0.002275, 0.002503, 0.00268, 0.002936, 0.003262, 0.003519, 0.003822, 0.004054, 0.004486, 0.004692, 0.005087, 0.005438, 0.005867, 0.006409, 0.00686, 0.007387, 0.008134, 0.009036, 0.009714, 0.010535, 0.0116, 0.012545, 0.013886, 0.015413, 0.016882, 0.018186, 0.019951, 0.021931, 0.024026, 0.025858, 0.028047, 0.031242, 0.035063, 0.038891, 0.043968, 0.048651, 0.055679, 0.062008, 0.070065, 0.076758, 0.086004, 0.096018, 0.107573, 0.120841, 0.136178, 0.151392, 0.170648, 0.187061, 0.209933, 0.229476, 0.252988, 0.276409, 0.297662, 0.32832, 0.339591, 0.378378, 0.401956,1)
#This vector contains the mortality rate at each age from 0 to 101 where the mortality rate at
#age 0 indicates the chance of dying by age 1 etc.The first position of this vector contains
#mortality rate of age 0, we don't have mortality rates for age 101 and above, so a person is 
#assumed to have a mortality rate of 1 at age 101.
#This is mortality rate for males in the UK for the year 2021-2023 obtained from ONS 

#parameters:
nsim=10000 #number of simulations
i=0.05 #effective annual interest rate
#initialise
age=1
death_age=numeric(nsim)
years_left=numeric(nsim)
DDay=numeric(102)
Years=numeric(102)

for (age in 0:101) {
  for (n in 1:nsim) {
    p=1 #initialise
    Death=1
    p=MortR[(age+1):length(MortR)]
    #indexes mortality rate from current age to age 101
    
    Death=rbinom(length(p),1,p)
    
    #Simulates bernoulli for each potential year the person might live, where if bernoulli results 1, 
    #the person dies, and stores data in a vector of same length as p, this output tells us that a 
    #person dies multiple times, which doesn't make sense, we must only get the first time someone dies
    dec=seq(from=length(p),to=1,by=-1)
    #creates a vector of decreasing numbers
    Death*dec
    #multiplies the above vectors generated, so that the earliest death records the largest number
    #in the vector, whilst others are either 0 or a smaller number
    death_age[n]=which.max(Death*dec)+age-0.5
    #this then returns the first year someone dies
    years_left[n]=which.max(Death*dec)-1
    #From an actuarial perspective, it's safer to assume a person will live a year less when, when
    #calculating NPV,as they could just die the next day after buying a policy, especially the old folks.
    #which.max tells us the the time the person dies, and we add the person's age to find the age they died
  }
  DDay[age+1]=mean(death_age)
  #stores the expected year of death for each age in a vector
  Years[age+1]=mean(years_left)
  #stores the expected years left of life for each age in a vector
}
DDay
Years

v=1/(1+i)
#the discount rate calculated from assumed interest rate
P_age=(1-v)/(1-v^(Years+1))
#the constant premium a policyholder should pay anually for insurance company to breakeven
#calculated using NPV=0, calculates the premium per dollar/pound of benefit
P_age


barplot(Years,space=0,axes=FALSE,ylim=c(0,90))
axis(1,at=seq(0,100,10),cex.axis=0.9)
axis(2,at=seq(0,90,5),cex.axis=0.8)
#plots the years left to live against current age

barplot(P_age,space=0,axes=FALSE,ylim=c(0,1),xlab="current age",ylab="premium/year")
axis(1,at=seq(0,100,10),cex.axis=0.9)
axis(2,at=seq(0,1,0.05),cex.axis=0.9)
#plots premium against current age


#Note that this code only takes age into account for mortality rate, when in fact, whether a person
#has diseases, syndromes or other risk factors can contribute towards their mortality rate, we should
#also be wary of adverse selection, a situation where mostly unhealthy people apply for the policy
#when the model assumes a mix of healthy and unhealthy people, leading to financial losses.

#This code also assumes a constant known interest rate which is often not the case, investments fail
#and interest rates fluctuate, we can use reddington immunisation to offset these effects, by structuring
#premiums such that, the policy is more resilient under small changes in interest rate, maybe by
#shifting portions of later year premiums to the early years, investing the excess to subsidize later
#years.

#Assumes constant mortality rates, but in reality life expectancy is increasing, so this model
#underestmates life expectancy. Could use past data to project mortality rates in the future.


