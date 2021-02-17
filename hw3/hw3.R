K=0.0125
delta=0.25
sigma=0.15
F0=0.0125
T=1.25
t=1
discount=1/(1+F0)^T
d1_black=(log(F0/K)+1/2*t*sigma^2)/(sqrt(t)*sigma)
d2_black=(log(F0/K)-1/2*t*sigma^2)/(sqrt(t)*sigma)
put_black=delta*discount*(K*pnorm(-d2_black)-F0*pnorm(-d1_black))
put_black

sigma=0.001875
d_bachelier = (F0-K)/(sigma*sqrt(t))
put_bachelier = delta*discount*sigma*sqrt(t)*(-d_bachelier*pnorm(-d_bachelier)+dnorm(-d_bachelier))
sigma
put_bachelier

put_black_f=function(delta,discount,K,d1_black,d2_black){
return(delta*discount*(K*pnorm(-d2_black)-F0*pnorm(-d1_black)))
}

greek_delta = function(d1_black,T,V,discount,delta,F0){
return (-T*V-discount*delta*pnorm(-d1_black)*(1+delta*F0))
}

greek_gamma = function(d1_black,T,t,V,discount,delta,F0,sigma){
return (-T*greek_delta(d1_black,T,V,discount,delta,F0)+delta*discount*(dnorm(d1_black)/(F0*sigma*sqrt(t))-T*pnorm(d1_black)))}

greek_vega = function(d1_black,T,t,V,discount,delta,F0,sigma){
return(discount*delta*F0*dnorm(d1_black)*sqrt(t))
}

greek_theta = function(delta,T,K,t,F0){
d1_black_1=(log(F0/K)+1/2*(t+0.01)*sigma^2)/(sqrt(t+1/12)*sigma)
d2_black_1=(log(F0/K)-1/2*(0.01+t)*sigma^2)/(sqrt(t+1/12)*sigma)
d1_black_2=(log(F0/K)+1/2*t*sigma^2)/(sqrt(t)*sigma)
d2_black_2=(log(F0/K)-1/2*t*sigma^2)/(sqrt(t)*sigma)
discount_1=1/(1+F0)^(T+1/12)
discount_2=1/(1+F0)^T
put1=put_black_f(delta,discount_1,K,d1_black_1,d2_black_1)
put2=put_black_f(delta,discount_2,K,d1_black_2,d2_black_2)
theta=(put1-put2)/(1/12)
return(theta)
}


#K=0.005
K=0.005
delta=0.25
sigma=0.15
F0=0.0125
T=1.25
t=1
discount=1/(1+F0)^T
d1_black=(log(F0/K)+1/2*t*sigma^2)/(sqrt(t)*sigma)
d2_black=(log(F0/K)-1/2*t*sigma^2)/(sqrt(t)*sigma)
put_black=delta*discount*(K*pnorm(-d2_black)-F0*pnorm(-d1_black))
put_black
greek_delta(d1_black,T,put_black,discount,delta,F0)
greek_gamma(d1_black,T,t,put_black,discount,delta,F0,sigma)
greek_vega(d1_black,T,t,V,discount,delta,F0,sigma)
greek_theta(delta,T,K,t,F0)

#K=0.0075
K=0.0075
delta=0.25
sigma=0.15
F0=0.0125
T=1.25
t=1
discount=1/(1+F0)^T
d1_black=(log(F0/K)+1/2*t*sigma^2)/(sqrt(t)*sigma)
d2_black=(log(F0/K)-1/2*t*sigma^2)/(sqrt(t)*sigma)
put_black=delta*discount*(K*pnorm(-d2_black)-F0*pnorm(-d1_black))
put_black
greek_delta(d1_black,T,put_black,discount,delta,F0)
greek_gamma(d1_black,T,t,put_black,discount,delta,F0,sigma)
greek_vega(d1_black,T,t,V,discount,delta,F0,sigma)
greek_theta(delta,T,K,t,F0)

#K=0.01
K=0.01
delta=0.25
sigma=0.15
F0=0.0125
T=1.25
t=1
discount=1/(1+F0)^T
d1_black=(log(F0/K)+1/2*t*sigma^2)/(sqrt(t)*sigma)
d2_black=(log(F0/K)-1/2*t*sigma^2)/(sqrt(t)*sigma)
put_black=delta*discount*(K*pnorm(-d2_black)-F0*pnorm(-d1_black))
put_black
greek_delta(d1_black,T,put_black,discount,delta,F0)
greek_gamma(d1_black,T,t,put_black,discount,delta,F0,sigma)
greek_vega(d1_black,T,t,V,discount,delta,F0,sigma)
greek_theta(delta,T,K,t,F0)

#K=0.0125
K=0.0125
delta=0.25
sigma=0.15
F0=0.0125
T=1.25
t=1
discount=1/(1+F0)^T
d1_black=(log(F0/K)+1/2*t*sigma^2)/(sqrt(t)*sigma)
d2_black=(log(F0/K)-1/2*t*sigma^2)/(sqrt(t)*sigma)
put_black=delta*discount*(K*pnorm(-d2_black)-F0*pnorm(-d1_black))
put_black
greek_delta(d1_black,T,put_black,discount,delta,F0)
greek_gamma(d1_black,T,t,put_black,discount,delta,F0,sigma)
greek_vega(d1_black,T,t,V,discount,delta,F0,sigma)
greek_theta(delta,T,K,t,F0)
discount
d1_black

