#Menor que 2520 não vai ser
numero = 2520

#Iniciando uma variavel
teste = False

#Daqui é só um loop que soma os restos da divisão de 2 a 20.
#Se a soma for zero, i.e divisivel por  todos os valores de 2 a 20
#então é o número que queremos
while(teste == False):
    numero += 2520
    somarestos = sum([numero %i for i in range(2, 21)])
    if( somarestos == 0) :
        print numero
        teste = True
