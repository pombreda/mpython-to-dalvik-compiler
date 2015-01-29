# algoritmo "ex6"
# Função : Faça um algoritmo que leia um número de 1 a 5 e escreva por
#          extenso. Caso o usuário digite um número que não esteja 
#          neste intervalo, exibir mensagem: número inválido.

def case_1():
    print("Um")
def case_2():
    print("Dois")
def case_3():
    print("Três")
def case_4():
    print("Quatro")
def case_5():
    print("Cinco")
def case_default():
    print "Número Inválido!!!"

dict = {"1" : case_1, "2" : case_2, "3" : case_3, "4" : case_4, "5" : case_5}

def switch(x):
    try:
        dict[x]()
    except:
        case_default()
     
numero = raw_input("Digite um número de 1 a 5: ")
switch(numero)



