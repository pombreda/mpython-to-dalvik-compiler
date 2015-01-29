# algoritmo "ex9"

preco = raw_input("Digite o preco: ")
venda = raw_input("Digite a venda: ")

if (venda < 500) or (preco < 30):
      novo_preco = preco + 10/100 * preco
elif (vendas >= 500 and venda < 1200) or (preco >= 30 and preco < 80):
      novo_preco = preco + 15/100 * preco
elif venda >= 1200 or preco >= 80:
      novo_preco = preco - 20/100 * preco

print("O novo preco é %d" novo_preco)

