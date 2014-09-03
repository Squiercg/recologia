#Parametro                                            Simbulo  Valor Padrão  Todos os Valores
#Densidade de Polinizadores                           P        -             0 - 1
#Forma da curva de polinização                        alpha    0.5           0.5,1,2
#Eficiencia dos polinizadores                         pii      1             0.5,1,2
#Densidade de granivoros                              G        -             0 - 1
#Forma da curva de granivoria                         beta     1             0.5,1,2
#Efeito da disponibilidade de sementes na Granivoria  epsilon  0             0,1,2
#Densidade maxima de presas removidas                 Dmax     1             0.5,1,2
#Efeito da densidade de presas na remoção de presas   delta    0.5           0.2,0.5,0.8
#Tipo de Presa preferido                              gamma    1             0.5,1,1.5
#predação densidade-dependente                        lambda   0             -1,0,1

Pe<-function(P,theta,delta,gamma,lambda,G) {
    return(P-theta*P*delta*(gamma+lambda*(P-G)))
    }

Ge<-function(G,theta,delta,gamma,lambda,P) {
    return(G-theta*G*delta*(2-gamma-lambda*(P-G)))
    }

theta<-function(Dmax,delta,P,gamma,lambda,G) {
    return((Dmax)/(delta*P*(gamma+lambda*(P-G))+delta*G*(2-gamma-lambda*(P-G))))
    }

F<-function(pii,Pe,alpha) {
    temp<-(pii*Pe)^alpha
    temp[is.nan(temp)]<-0
    return(temp)
    }
C<-function(phi,Ge,beta) {
    return((phi*Ge)^beta)
    }
phi<-function(epsilon,F) {
    return(1+epsilon(F-0.5))
    }
S<-function(F,C) {
    return(F*(1-C))
    }


#densidade de polinizadores vs net effect Sp-Snp
P<-seq(0.1,1,0.1)
G<-0
gamma<-1
lambda<-0
delta<-0.5
Dmax<-1

theta.out<-theta(Dmax,delta,P,gamma,lambda,G)
theta.out

Pe.out<-Pe(P,theta=theta.out,delta,gamma,lambda,G)
Pe.out


pii<-1
alpha<-0.5

F.out<-F(pii,Pe=Pe.out,alpha)
F.out

epsilon<-0

phi(epsilon,F)

Ge.out<-Ge(G,theta=theta.out,delta,gamma,lambda,P)
Ge.out

beta=1

C.out<-C(phi,Ge=Ge.out,beta)
