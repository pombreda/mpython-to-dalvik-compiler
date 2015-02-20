def contador(a: int, b: int) -> int:
   soma = 0
   i = 0
   for i in range(5):
      soma = soma + a + b
   return soma

def retorna()->void:
    print("teste")

count = contador(2,3)
a = retorna()