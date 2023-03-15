#Library import 
from random import randint

#Para inserir os valores de população e amostra 
n_pop = int(input("Tamanho da população: "))
n_amostra = int(input("Tamanho da amostra: "))

#Determinar os valores de amplitude e valor inicial 
amplitude = int(n_pop/n_amostra)
val_inicial = randint(1, amplitude)

#Operações para definir a lista de elementos selecionados da população 
cont_elementos = 0
cont = 0
lista = []
while cont_elementos <= n_pop:
    if (val_inicial + cont *amplitude) < n_pop:
        lista.append(val_inicial + cont*amplitude)
    cont_elementos+=amplitude
    cont+=1
    
#Imprimindo os valores 
print("Amostra: ", lista)
print("Quantidade de elementos na amostra: ", len(lista))
