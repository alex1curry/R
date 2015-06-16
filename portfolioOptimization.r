#THIS FUNCTION IS FOR ONE PERIOD 
RecurseOne = function(sig,mu,h,rf,alpha,prob_mat,Wr,n,W1,p, Wrx,prob_t){

xx=seq(.1,1,.1e-1)
alpha=matrix(xx,length(xx),1)
jj=seq(1,length(xx))
dummy = matrix(0,n,length(xx))
a_star1 = matrix(0,1,length(xx))
a_star = matrix(0,n,1)

for (d in jj) {  ##ALPHA BRUTE FORCE LOOP. CALCULATES THE PROB_MAT FOR EVERY ALPHA STORES IN DUMMY AND FINDS THE ONE THE MAXS THE V

for (i in k) { # ROW OF PROB_MAT
for(j in k) { #COLUMN OF PROB_MAT


if((Wrx[j]/Wr[i]-(1-alpha[d])*exp(rf*h))>0){  #CONDITIONAL STATMENT TO AVOID /0
prob_mat[i,j]=dnorm((log(((Wrx[j]/Wr[i])-(1-alpha[d])*exp(rf*h))/alpha[d])-(mu-.5*sig^2)*h)/(sig*sqrt(h)))
}

else { prob_mat[i,j]=0}

}
}

for(i in k) {   #SUM OF THE PROBABILITIES----WE WILL WANT TO DNORM THIS
sum_prob[i]=sum(prob_mat[i,])
}


for (ii in k) { #FILLS THE VALUE FUNCTION WITH MATRIX MULTIPLY
V[ii,1] = (prob_mat[(ii),]%*%V[,2])/sum_prob[ii,]
}



dummy[,d] = V[,1]   #DUMMY STORAGE TO FIND MAXIMUM VALUE
a_star1[,d]=alpha[d]




 } #END ALPHA BRUTE FORCE LOOP 
for (i in k) {#ROW--- FINDS THE ALPHA THAT MAXIZES VALUE FCN. AND STORES THE ALPHA

V[i,1] = max(dummy[i,])

t=which.max(dummy[i,])
a_star[i,]=a_star1[t]

}


for (ii in k) { #ROW-- FILLS THE PROBABILTY MATRIX

prob_t[ii,1] = (prob_mat[(ii),]%*%prob_t[,2])/sum_prob[ii,]
}



result = cbind(V,prob_t,a_star) #PREPARE THE ONE PERIOD RESULTS FOR RECURSION 

}
##################

#ENTER DATA FOR THE PROBLEM
W1  = seq(5,.25,-.25);  #WEALTH SEQUENCE
n   = length(W1);		#WEALTH MATRIX SCALER
Wr  = matrix(W1,n,1);	#WEALTH MATRIX
Wrx = matrix(W1,n,1)	#LAST PERIOD WEALTH MATRIX-- i.e. THE TOP ROW OF THE EXCEL FILE
y=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1) #HACKED PROBAILITY MATRIX
prob_mat=matrix(0,n,n); #probability matrix
 
rf=.04			#INTEREST RATE
h=.25				#PERIOD LENGTH
mu=.07			#EX RETURN
sig=.3			#VOLATILITY
x=2   ##### DONT CHANGE!! SIZE OF THE ONE PERIOD RECURSION
prob_t=matrix(y,n,x)	#PROBAILITY MATRIX OF THE VALUE FNC.
V=matrix(Wr,n,x)		#VALUE FCN. FOR ONE PERIOD RECURSION
sum_prob=matrix(0,n,1)	#SUM OF THE PROBAILITY MATRIX
k=seq(1,n)			#SCALER FOR THE PROBAILITY MATRIX AND WEALTH VCTR.
kk=seq(x,1,-1)		#SCALER FOR THE DUMMMY
p=3 # PERIOD!!!!		#TOTAL PERIODS THE RECURSION IS RUN OVER
q=p-1				#USED TO PREP THE RECURSION
b=seq(p-2,1,-1)		#USED TO PREP THE RECURSION

sol=RecurseOne(sig,mu,h,rf,alpha,prob_mat,Wr,n,W1,p,Wrx,prob_t) #INITIAL CALL OF THE RECURSION TO LOAD THE FIRST TWO PERIODS IN
A_MAX=matrix(0,n,q)	#CREATE THE FINAL ALPHA MATRIX
A_MAX[,q]=sol[,5]		#FILL THE FINAL ALPHA MATRIX
c=cbind(sol[,1],Wr)	#PREPARATION FOR THE FINAL VALUE MATRIX
nn=seq(p,p-1,-1)		#SCALER FOR THE FINAL RECURSION
V_MAX = matrix(0,n,p)	#CREATE THE FINAL VALUE MATRIX
V_MAX[,nn]=c[,2]		#FILL IT WITH INITIAL ONE PERIOD RECURSION
V_MAX[,nn[2]]=c[,1]	#FILL IT WITH INITIAL ONE PERIOD RECURSION
##############################

#RUN THE RECURSION LOOP AND FILL APPROPRIATE MATRICES 
for(i in b) {
w = i+1
Wr = V_MAX[,w]

final= RecurseOne(sig,mu,h,rf,alpha,prob_mat,Wr,n,W1,p,Wrx,prob_t)


V_MAX[,i]=final[,1]
A_MAX[,i]=final[,3]
}


