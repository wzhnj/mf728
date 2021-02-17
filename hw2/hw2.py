# -*- coding: utf-8 -*-
"""
Created on Fri Feb  5 11:13:32 2021
Email:zuhuwang@bu.edu
@author: Zuhua Wang
"""

from scipy import optimize
import numpy as np
import math 
a=2.8438
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import scipy.optimize
'''
f = np.array([1,2])
k = 10
def func(x, f, K):
    return np.sum(x**f, axis=0)-k

res = scipy.optimize.fsolve(func, x0=3,args=(f[:, None], K))
print(res)
'''

#temp=df.iloc[1,1]
            #temp=float(temp)
#print(temp)
class swap:
    def forward_rate_a(swap_rate,T):
        i=np.arange(1,2*T+1)
        
        def func(x,swap_rate,i):
            #swap_rate/=100
            return swap_rate-np.sum((np.exp(x/2)-1)*np.exp(-1/2*x*i))/np.sum(0.5*np.exp(-1/2*x*i))
        rate = scipy.optimize.fsolve(func, x0=3,args=(swap_rate,i[:, None]))
        res=(np.exp(rate/2)-1)/0.5
        nu=13
        de=9
        #print(rate,res)
        nu=np.sum((np.exp(rate/2)-1)*np.exp(-1/2*rate*i))
        de=np.sum(0.5*np.exp(-1/2*rate*i))
        return res,rate
        
        #return res, rate, nu, de
    def forward_rate_b(swap_rate,T):
        i=np.arange(3,2*T+1)
        before=np.arange(1,3)
        def func(x,swap_rate,i):
            return swap_rate-np.sum(0.5*swap.forward_rate_a(2.8438,1)[0]*np.exp(-1*before*swap.forward_rate_a(2.8438,1)[1]))*np.sum(1/i*(np.exp(i*x/2)-1)*np.exp(-1/2*x*i))/(np.sum(0.5*np.exp(-1/2*x*i))*np.sum(np.exp(-1*before*swap.forward_rate_a(2.8438,1)[1])))
        res = scipy.optimize.fsolve(func, x0=3,args=(swap_rate,i[:, None]))
        res=(np.exp(res*T)-1)/T
        print("nu",np.sum(0.5*swap.forward_rate_a(2.8438,1)[0]*np.exp(-1*before*swap.forward_rate_a(2.8438,1)[1])))
        return res
    def b(swap_rate,T):
        i=np.arange(3,2*T+1)
        before=np.arange(1,3)
        print("i",i)
        def func(x,swap_rate,i):
            return swap_rate-swap.forward_rate_a(2.8438,1)[2]*np.sum(1/i*(np.exp(i*x/2)-1)*np.exp(-1/2*x*i))/(np.sum(0.5*np.exp(-1/2*x*i))*swap.forward_rate_a(2.8438,1)[3])
        res = scipy.optimize.fsolve(func, x0=3,args=(swap_rate,i[:, None]))
        res=(np.exp(res)-1)
        print("T",T)
        return res
    def forward_rate_c():
        count=0
        l=[2,3,4,5,7,10,30]
        for j in l:
            if count>0: 
                i=np.arange(l[count-1]*2+1,l[count]*2+1)
            else:
                i=np.arange(3,5)
            
            swap_rate=df.iloc[l[count]-1,0]
            print(i,swap_rate)
            before=np.arange(1,i[0])
            temp=df.iloc[l[count]-2,1]
            #temp=float(temp)
            print(temp)
            def func(x,swap_rate,i):
                return swap_rate-df["nu"].dropna().iloc[-1]*np.sum((np.exp(x/2)-1)*np.exp(-1/2*x*i))/(np.sum(0.5*np.exp(-1/2*x*i))*df["de"].dropna().iloc[-1])
                #return swap_rate-np.sum(0.5*swap.forward_rate_a(2.8438,1)[0]*np.exp(-1*before*swap.forward_rate_a(2.8438,1)[1]))*np.sum(1/i*(np.exp(i*x/2)-1)*np.exp(-1/2*x*i))/(np.sum(0.5*np.exp(-1/2*x*i))*np.sum(np.exp(-1*before*swap.forward_rate_a(2.8438,1)[1])))
            res = scipy.optimize.fsolve(func, x0=3,args=(swap_rate,i[:, None]))
            df.iloc[l[count]-1,2]=res
            x=res
            df.iloc[l[count]-1,3]=df["nu"].dropna().iloc[-1]*np.sum((np.exp(x/2)-1)*np.exp(-1/2*x*i))
            df.iloc[l[count]-1,4]=np.sum(0.5*np.exp(-1/2*x*i))*df["de"].dropna().iloc[-1]
            res=(np.exp(res/2)-1)/0.5
            
            df.iloc[l[count]-1,1]=res
            count+=1
            print(res)
    def test():
        f1=2.8438
        r1=1.7691
        
        def func(x,f1,f2):
            return 0.5*3.06*(np.exp(-1*r1/2)+np.exp(-1*r1))+3.06/2*np.exp(-0.5*x)*np.exp(-1*r1)+3.06/2*np.exp(-1*x)*np.exp(-1*r1)-f1/2*(np.exp(-1*r1/2)+np.exp(-1*r1))-(np.exp(x/2)-1)*(np.exp(-1*r1)*np.exp(-1*x/2))-(np.exp(x/2)-1)*(np.exp(-1*r1)*np.exp(-1*x))
        rate = scipy.optimize.fsolve(func, x0=3,args=(f1,r1))
        forward_rate=(np.exp(rate/2)-1)/0.5
        return rate,forward_rate
    def forward_rate_c_1():
        count=0
        l=[2,3,4,5,7,10,30]
        last=1
        record=1
        for j in l:
            
            def func(x):
                
                r=[]
                forward=[]
                sumr=0
                c=1
                for i in range(j*2):
                    if (i<2*last):
                        #print("np.sum(r)",np.sum(r),df.iloc[int(i/2),2]/2)
                        if (i!=0):
                            r.append(r[-1]-(df.iloc[int(i/2),2]/2))
                        else:
                            r.append(-1*(df.iloc[int(i/2),2]/2))
                        #print(np.sum(r)-df.iloc[int(i/2),2]/2)
                        forward.append(df.iloc[int(i/2),1])
                        sumr=r[-1]
                        #print(forward)
                    else:
                        r.append(sumr-x*0.5*c)
                        forward.append((np.exp(x/2)-1)/0.5)
                        c+=1
                    
                
                
                return np.sum(df.iloc[j-1,0]*np.power(math.e,r))-np.sum(forward*np.power(math.e,r))
                
            rate = scipy.optimize.fsolve(func, x0=0)
            #print("rate3",rate[0])
            forward_rate=(np.exp(rate[0]/2)-1)/0.5
            #print("forward_rate",forward_rate)
            #print(record,j)
            for i in range(last,j):
                #print("sdafsdf")
                df.iloc[i,2]=rate
                df.iloc[i,1]=forward_rate
            last=j
            #print(j,r)
            print()
            #print(rate)
            if (j==19):
                return
            #print(df)
        return
    def test_1():
        
        def func(x):
            #l=[x,2*x+1]
            l=[]
            for i in range(2):
                l.append(x)
            r=[]
            r=[x,x]
            print(l)
            #l=np.array(l)
            #return np.sum(np.exp())
            print("type:",type(l))
            print("type:",type(r))
            return np.sum(np.power(math.e,r)-5.5)
            return np.sum(np.power(math.e,l))-5.5
        rate = scipy.optimize.fsolve(func, x0=2)
        print(rate)
    def find_swap_rate(year,df1):
        tvalue=0
        for i in range(year):
            value1=df1.iloc[i,1]/2
            value2=df1.iloc[i,1]/2
            value1=value1*np.exp(-np.sum(df1.iloc[0:i,2]))
            value1=value1*np.exp(-df1.iloc[i,2]/2)
            value2=value2*np.exp(-np.sum(df1.iloc[0:i,2]))
            value2=value2*np.exp(-df1.iloc[i,2])
            tvalue=tvalue+value1+value2
        #print(tvalue)
        def func(x):
            r=[-1*df1.iloc[0,2]/2]
            for i in range(2*year-1):
                r.append(r[-1]-df1.iloc[int((1+i)/2),2]/2)
            #0.03223673print(r)
            return np.sum(0.5*x*np.power(math.e,r))-tvalue
        rate = scipy.optimize.fsolve(func, x0=0)
        #print("swap rates",rate)
        return rate
    def find_zero_rate():
        zero=[]
        for i in range(len(df)):
            zero.append(np.exp(df.iloc[i,2])-1)
        print(zero)
        df["zero"]=zero
        return
    def move_forward(dftemp):
        
        dftemp.iloc[:,1]+=0.01
        for i in range(len(dftemp)):
            dftemp.iloc[i,2]=np.log(dftemp.iloc[i,1]*0.5+1)*2
        dftemp1=dftemp
        for i in range(len(dftemp)):
            dftemp.iloc[i,0]=swap.find_swap_rate(i+1,dftemp1)
        print(dftemp)
    def g(change,df2):
        year=[1,2,3,4,5,7,10,30]
        for i in range(len(change)):
            df2.iloc[year[i]-1,0]+=(change[i]/10000)
            
            
        
df=pd.DataFrame([2.8438,3.060,3.126,3.144,3.150,
                 None,3.169,None,None,3.210,
                 None,None,None,None,None,
                 None,None,None,None,None,
                 None,None,None,None,None,
                 None,None,None,None,3.237],columns=["swap rates"],index=range(1,31))
df=df/100

df["forward rate"]=None
df["rate"]=None
df["nu"]=None
df["de"]=None
df.iloc[0,1]=swap.forward_rate_a(df.iloc[0,0],1)[0]
df.iloc[0,2]=swap.forward_rate_a(df.iloc[0,0],1)[1]




print(df)
            

swap.forward_rate_c_1()
#for i in range(len(df)):
#    if (df.iloc[i,0])
swap.test_1()
print(df)
plt.figure()
plt.scatter(x=df.index,y=df.iloc[:,0])
plt.scatter(x=df.index,y=df.iloc[:,1])
plt.ylim(0.0275,0.035)
plt.plot()
print(df)
df=df.iloc[:,0:3]
swap.find_swap_rate(15,df)
print(df)
#swap.find_swap_rate(2)
swap.find_zero_rate()
print(df)
plt.figure()
plt.scatter(x=df.index,y=df.iloc[:,0])
plt.scatter(x=df.index,y=df.iloc[:,3])
plt.ylim(0.0275,0.035)
plt.plot()
df1=df.copy()
swap.move_forward(df1)
print(df)
df2=df.copy()
swap.g([0,0,0,5,10,15,25,50],df2)
print("df2",df2)
print("Original df")
print(df)
original_df=df.copy()
df=df2.copy()
df.iloc[0,1]=swap.forward_rate_a(df.iloc[0,0],1)[0]
df.iloc[0,2]=swap.forward_rate_a(df.iloc[0,0],1)[1]

swap.forward_rate_c_1()

print(df)
print(10000*(df.iloc[:,0:2]-original_df.iloc[:,0:2]))



df2=original_df.copy()
swap.g([-50,-25,-15,-10,-5,0,0,0],df2)
print("df2",df2)
print("Original df")
print(original_df)
df=df2.copy()
df=df2.copy()
df.iloc[0,1]=swap.forward_rate_a(df.iloc[0,0],1)[0]
df.iloc[0,2]=swap.forward_rate_a(df.iloc[0,0],1)[1]

swap.forward_rate_c_1()

print(df)
print(10000*(df.iloc[:,0:2]-original_df.iloc[:,0:2]))


