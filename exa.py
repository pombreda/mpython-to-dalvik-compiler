def contador(int: a,int: b)-> int:
    soma = 0
    for i in range(5):
        soma = soma + a + b
    return soma
def imprimeSoma():
    x = raw_input("Digite o primeiro numero: ")
    x = int(x)
    y = raw_input("Digite o segundo numero: ")
    y = int(y)
    soma = x + y
    print(soma)
count = contador(2, 3)
imprimeSoma()