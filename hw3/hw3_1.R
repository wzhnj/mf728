K=0.0125
delta=0.25
sigma=0.15
F0=0.0125
T=1.25
t=1
discount=function(F0,T){return(1/(1+F0)^T)}
d1_black=function(F0,K,sigma,t){return((log(F0/K)+1/2*t*sigma^2)/(sqrt(t)*sigma))}
d2_black=function(F0,K,sigma,t){return((log(F0/K)-1/2*t*sigma^2)/(sqrt(t)*sigma))}
put_black=function(F0,K,sigma,t,T,delta){delta*discount(F0,T)*(K*pnorm(-d2_black(F0,K,sigma,t))-F0*pnorm(-d1_black(F0,K,sigma,t)))}
put_black(F0,K,sigma,t,T,delta)

implied_vol=function(price,F0,K,t,T,delta){
func=function(price,F0,K,t,T,delta,sigma) price-put_black(F0,K,sigma,t,T,delta)
return (uniroot(func,price=price,F0=F0,K=K,t=t,T=T,delta=delta,interval = c(0.01,100)))
}



# sigma=0.001875
# d_bachelier =function(F0,K,sigma,t){return( (F0-K)/(sigma*sqrt(t)))}
# put_bachelier = function(F0,K,sigma,t,T,delta){return(delta*discount(F0,T)*sigma*sqrt(t)*(-d_bachelier(F0,K,sigma,t)*pnorm(-d_bachelier(F0,K,sigma,t))+dnorm(-d_bachelier(F0,K,sigma,t))))}
# put_bachelier(F0,K,sigma,t,T,delta)


greek_delta = function(F0,K,sigma,t,T,delta){
return (-T*put_black(F0,K,sigma,t,T,delta)-discount(F0,T)*delta*pnorm(-d1_black(F0,K,sigma,t))*(1+delta*F0))
}
greek_delta(F0,K,sigma,t,T,delta)


greek_gamma = function(F0,K,sigma,t,T,delta){
return (-T*greek_delta(F0,K,sigma,t,T,delta)+delta*discount(F0,T)*(dnorm(d1_black(F0,K,sigma,t))/(F0*sigma*sqrt(t))-T*pnorm(d1_black(F0,K,sigma,t))))
}
greek_gamma(F0,K,sigma,t,T,delta)


greek_vega = function(F0,K,sigma,t,T,delta){
return(discount(F0,T)*delta*F0*dnorm(d1_black(F0,K,sigma,t))*sqrt(t))
}
greek_vega(F0,K,sigma,t,T,delta)



greek_theta = function(F0,K,sigma,t,T,delta){
  a=F0*sigma/(2*sqrt(t))*dnorm(d1_black(F0,K,sigma,t))
  b=(K*pnorm(-d2_black(F0,K,sigma,t))-F0*pnorm(-d1_black(F0,K,sigma,t)))*F0
  return(discount(F0,T)*delta*(-a+b))
}
greek_theta(F0,K,sigma,t,T,delta)

greek=function(F0,K,sigma,t,T,delta){
cat("F0:",F0,"\nK:",K,"\nsigma:",sigma,"\nt:",t,"\nT:",T,"\ndelta:",delta)
cat("\ndelta",greek_delta(F0,K,sigma,t,T,delta))
cat("\ngamma",greek_gamma(F0,K,sigma,t,T,delta))
cat("\nvega",greek_vega(F0,K,sigma,t,T,delta))
cat("\ntheta",greek_theta(F0,K,sigma,t,T,delta))
cat("\n")
}

#K=0.005
K=0.005
greek(F0,K,sigma,t,T,delta)
K=0.0075
greek(F0,K,sigma,t,T,delta)

K=0.01
greek(F0,K,sigma,t,T,delta)

K=0.0125
greek(F0,K,sigma,t,T,delta)


#problem 2
K=0.01
delta=0.25
F0=0.01

one_year=0
sigma=0.15
t=2
T=t+delta
one_year_1=one_year+put_black(F0,K,sigma,t,T,delta)
one_year_2=one_year+put_black(F0,K,sigma,t,T,delta)+put_black(F0,K,sigma,t+0.25,T+0.25,delta)
one_year_3=one_year+put_black(F0,K,sigma,t,T,delta)+put_black(F0,K,sigma,t+0.25,T+0.25,delta)+put_black(F0,K,sigma,t+0.5,T+0.5,delta)
one_year=one_year+put_black(F0,K,sigma,t,T,delta)+put_black(F0,K,sigma,t+0.25,T+0.25,delta)+put_black(F0,K,sigma,t+0.5,T+0.5,delta)+put_black(F0,K,sigma,t+0.75,0.75+T,delta)



two_year=one_year
sigma=0.2
t=3
T=t+delta
two_year_1=two_year+put_black(F0,K,sigma,t,T,delta)
two_year_2=two_year+put_black(F0,K,sigma,t,T,delta)+put_black(F0,K,sigma,t+0.25,T+0.25,delta)
two_year_3=two_year+put_black(F0,K,sigma,t,T,delta)+put_black(F0,K,sigma,t+0.25,T+0.25,delta)+put_black(F0,K,sigma,t+0.5,T+0.5,delta)
two_year=two_year+put_black(F0,K,sigma,t,T,delta)+put_black(F0,K,sigma,t+0.25,T+0.25,delta)+put_black(F0,K,sigma,t+0.5,T+0.5,delta)+put_black(F0,K,sigma,t+0.75,0.75+T,delta)

three_year=two_year
sigma=0.225
t=4
T=t+delta
three_year_1=three_year+put_black(F0,K,sigma,t,T,delta)
three_year_2=three_year+put_black(F0,K,sigma,t,T,delta)+put_black(F0,K,sigma,t+0.25,T+0.25,delta)
three_year_3=three_year+put_black(F0,K,sigma,t,T,delta)+put_black(F0,K,sigma,t+0.25,T+0.25,delta)+put_black(F0,K,sigma,t+0.5,T+0.5,delta)
three_year=three_year+put_black(F0,K,sigma,t,T,delta)+put_black(F0,K,sigma,t+0.25,T+0.25,delta)+put_black(F0,K,sigma,t+0.5,T+0.5,delta)+put_black(F0,K,sigma,t+0.75,0.75+T,delta)

four_year=three_year
sigma=0.225
t=5
T=t+delta
four_year_1=four_year+put_black(F0,K,sigma,t,T,delta)
four_year_2=four_year+put_black(F0,K,sigma,t,T,delta)+put_black(F0,K,sigma,t+0.25,T+0.25,delta)
four_year_3=four_year+put_black(F0,K,sigma,t,T,delta)+put_black(F0,K,sigma,t+0.25,T+0.25,delta)+put_black(F0,K,sigma,t+0.5,T+0.5,delta)
four_year=four_year+put_black(F0,K,sigma,t,T,delta)+put_black(F0,K,sigma,t+0.25,T+0.25,delta)+put_black(F0,K,sigma,t+0.5,T+0.5,delta)+put_black(F0,K,sigma,t+0.75,0.75+T,delta)

five_year=four_year
sigma=0.25
t=6
T=t+delta
five_year_1=five_year+put_black(F0,K,sigma,t,T,delta)
five_year_2=five_year+put_black(F0,K,sigma,t,T,delta)+put_black(F0,K,sigma,t+0.25,T+0.25,delta)
five_year_3=five_year+put_black(F0,K,sigma,t,T,delta)+put_black(F0,K,sigma,t+0.25,T+0.25,delta)+put_black(F0,K,sigma,t+0.5,T+0.5,delta)
five_year=five_year+put_black(F0,K,sigma,t,T,delta)+put_black(F0,K,sigma,t+0.25,T+0.25,delta)+put_black(F0,K,sigma,t+0.5,T+0.5,delta)+put_black(F0,K,sigma,t+0.75,0.75+T,delta)

one_year_1
one_year_2
one_year_3
one_year

two_year_1
two_year_2
two_year_3
two_year

three_year_1
three_year_2
three_year_3
three_year

four_year_1
four_year_2
four_year_3
four_year

five_year_1
five_year_2
five_year_3
five_year



t=2
impolied_vol_v=rep(0,20)
impolied_vol_v[[1]]=implied_vol(one_year_1,F0,K,t,2.25,delta)$root
impolied_vol_v[[2]]=implied_vol(one_year_2,F0,K,t,2.5,delta*2)$root
impolied_vol_v[[3]]=implied_vol(one_year_3,F0,K,t,2.75,delta*3)$root
impolied_vol_v[[4]]=implied_vol(one_year,F0,K,t,3,delta*4)$root

impolied_vol_v[[5]]=implied_vol(two_year_1,F0,K,t,3.25,delta*5)$root
impolied_vol_v[[6]]=implied_vol(two_year_2,F0,K,t,3.5,delta*6)$root
impolied_vol_v[[7]]=implied_vol(two_year_3,F0,K,t,3.75,delta*7)$root
impolied_vol_v[[8]]=implied_vol(two_year,F0,K,t,4,delta*8)$root

impolied_vol_v[[9]]=implied_vol(three_year_1,F0,K,t,4.25,delta*9)$root
impolied_vol_v[[10]]=implied_vol(three_year_2,F0,K,t,4.5,delta*10)$root
impolied_vol_v[[11]]=implied_vol(three_year_3,F0,K,t,4.75,delta*11)$root
impolied_vol_v[[12]]=implied_vol(three_year,F0,K,t,5,delta*12)$root

impolied_vol_v[[13]]=implied_vol(four_year_1,F0,K,t,5.25,delta*13)$root
impolied_vol_v[[14]]=implied_vol(four_year_2,F0,K,t,5.5,delta*14)$root
impolied_vol_v[[15]]=implied_vol(four_year_3,F0,K,t,5.75,delta*15)$root
impolied_vol_v[[16]]=implied_vol(four_year,F0,K,t,6,delta*16)$root

impolied_vol_v[[17]]=implied_vol(five_year_1,F0,K,t,6.25,delta*17)$root
impolied_vol_v[[18]]=implied_vol(five_year_2,F0,K,t,6.5,delta*18)$root
impolied_vol_v[[19]]=implied_vol(five_year_3,F0,K,t,6.75,delta*19)$root
impolied_vol_v[[20]]=implied_vol(five_year,F0,K,t,7,delta*20)$root

impolied_vol_v
vol_v=c(0.15,0.15,0.15,0.15,0.2,0.2,0.2,0.2,0.225,0.225,0.225,0.225,0.225,0.225,0.225,0.225,0.25,0.25,0.25,0.25)
plot(impolied_vol_v,type="l",col="red",ylab = "",ylim=c(0.13,0.35))
par(new=TRUE)
plot(vol_v,type="l",col="green",ylab = "volatility",ylim=c(0.13,0.35))

