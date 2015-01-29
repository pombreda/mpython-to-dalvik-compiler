# algoritmo "ex5"
#  Função : Escrever um algoritmo que leia o nome e o sexo de 56 pessoas
#           e informe o nome e se ela é homem ou mulher. No final informe
#           o total de homens e de mulheres.

def case_1():
    h = h + 1
def case_2():
    m = m + 1
def case_default():
    print "Sexo só pode ser H ou M!"

dict = {"H" : case_1, "M" : case_2}

def switch(x):
    try:
        dict[x]()
    except:
        case_default()

for x in range(1,5):
     nome = raw_input("Digite o nome: ")
     sexo = raw_input("H - Homem ou M - Mulher: ")

     switch(sexo)

print("Foram inseridos Homens")
print("Foram inseridos Mulheres")