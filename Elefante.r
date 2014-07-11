#John von Neumann disse:
#"With four parameters I can fit an elephant,
#and with five I can make him wiggle his trunk".

#Quatro parametros, eles são número complexos, podemos escrevelos no R assim:

p1 <- complex(real = 50, imaginary = -30)

#Ou de forma simplificada:
p1<-  50-30i
p2<-  18+8i
p3<-  12-10i
p4<- -14-60i

#O modelo vai ser representado por uma série de fourier, que de acordo com o
#wikipedia é:
#Em matemática, uma série de Fourier, nomeada em honra de
#Jean-Baptiste Joseph Fourier (1768-1830), é a representação de uma
#função periódica (muitas vezes, nos casos mais simples, tidas como tendo
#período 2pi) como uma soma de funções periódicas da forma.


#Então definimos uma função pra calcular a série de Fourier:
#Veja que ela começa definindo um vetor pra descarregar os resultados.
#Em seguida extrai a parte real e a imaginaria do parametro em A e B
#o R faz coerção para numeros reais na maioria das operações o que faz tudo dar
#pau. No loop ali dentro da função ele faz a formula que esta no artigo.

fourier<-function(tt,CC){
                         f<-numeric(length(tt));
                         A<-Re(CC);
                         B<-Im(CC);
                         k<-1:length(CC);
                         for(i in 1:length(tt)){
                                                co<-cos(tt[i]*k)
                                                si<-sin(tt[i]*k)
                                                f[i]<-sum(A*co+B*si)
                                                f
                                               }
                        f
                        }


#Agora que temos uma função para calcular a serie de fourier, fazemos a função
#que é o modelo do nosso elefantinho, nossas observações e 4 parametros


elefante <- function(tt,p1,p2,p3,p4) {
                                      n<-6
                                      Cx<-complex(n);
                                      Cy<-complex(n);
                                      Cx[1]<-Re(p1)*1i
                                      Cx[2]<-Re(p2)*1i
                                      Cx[3]<-Re(p3)
                                      Cx[5]<-Re(p4)
                                      Cy[1]<-Im(p4)+Im(p1)*1i
                                      Cy[2]<-Im(p2)*1i
                                      Cy[3]<-Im(p3)*1i

                                      x<-fourier(tt,Cx)
                                      y<-fourier(tt,Cy)
                                      res<-cbind(y,-x)
                                      res
                                      }


#Pegamos uma sequencia de observações para introduzir no nosso
#modelo deterministico
observações<-seq(from=0,to=2*pi,length=100)

#E vemo o que acontece
elefantinho<-elefante(observações,p1,p2,p3,p4)

plot(elefantinho,type="l",col="red",ylab="",xlab="",lwd=2)

#Com 5 parametros da pra adicionar um olhinho :)
p5<-40+20i
points(Im(p5),Im(p5),pch=20,col="red")

#Artigo original:
#“Drawing an elephant with four complex parameters”
#by Jurgen Mayer, Khaled Khairy, and Jonathon Howard,
#Am. J. Phys. 78, 648 (2010), DOI:10.1119/1.3254017.
#http://java-srv1.mpi-cbg.de/publications/getDocument.html?id=ff8080812daff75c012dc1b7bc10000c


#Codigo em python que usei de base:
#"""
#Author: Piotr A. Zolnierczuk (zolnierczukp at ornl dot gov)
#
#Based on a paper by:
#Drawing an elephant with four complex parameters
#Jurgen Mayer, Khaled Khairy, and Jonathon Howard,
#Am. J. Phys. 78, 648 (2010), DOI:10.1119/1.3254017
#"""
#import numpy as np
#import pylab
#
## elephant parameters
#p1, p2, p3, p4 = (50 - 30j, 18 +  8j, 12 - 10j, -14 - 60j )
#p5 = 40 + 20j # eyepiece
#
#def fourier(t, C):
#    f = np.zeros(t.shape)
#    A, B = C.real, C.imag
#    for k in range(len(C)):
#        f = f + A[k]*np.cos(k*t) + B[k]*np.sin(k*t)
#    return f
#
#def elephant(t, p1, p2, p3, p4, p5):
#    npar = 6
#    Cx = np.zeros((npar,), dtype='complex')
#    Cy = np.zeros((npar,), dtype='complex')
#
#    Cx[1] = p1.real*1j
#    Cx[2] = p2.real*1j
#    Cx[3] = p3.real
#    Cx[5] = p4.real
#
#    Cy[1] = p4.imag + p1.imag*1j
#    Cy[2] = p2.imag*1j
#    Cy[3] = p3.imag*1j
#
#    x = np.append(fourier(t,Cx), [-p5.imag])
#    y = np.append(fourier(t,Cy), [p5.imag])
#
#    return x,y
#
#x, y = elephant(np.linspace(0,2*np.pi,1000), p1, p2, p3, p4, p5)
#pylab.plot(y,-x,'.')
#pylab.show()
