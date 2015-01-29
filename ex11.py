# algoritmo "ex7"
#  Função : Faça um algoritmo que receba N números e mostre positivo,
#           negativo ou zero para cada número.

programa = 1
while programa = 1:
    numero = raw_input("Digite um número: ")
    if numero > 0:
        print("Positivo")
    else:
        if numero = 0:
            print("O número é igual a 0")

        if numero < 0:
            print("Negativo")

    opc = raw_input("Deseja finalizar? (S/N) ")

    if opc == "S":
        programa = 0