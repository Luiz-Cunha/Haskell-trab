import System.IO

type Cliente = (String, Integer , String)

type Date = (Integer, Integer, Integer)

type Pedido = (Integer , Cliente, Date, Float)

type Expresso = (Pedido, Date) 

clienteToString :: Cliente -> String
clienteToString (nome,tel,rua) = nome

pedidosimplesToString (nr, cliente, date, preco) = clienteToString cliente ++ " " ++ show preco

pedidoexpressoToString ((nr, cliente, date, preco), dataentrega) = pedidosimplesToString (nr, cliente, date, preco)++ " " ++ verificaEntrega ((nr, cliente, date, preco), dataentrega)

verificaprazo :: Expresso -> Bool
verificaprazo ((numero, cliente, date, _),dataentrega)
    |date==dataentrega = True 
    |otherwise = False 

verificaEntrega ((numero, cliente, date, preco),dataentrega) 
    |verificaprazo ((numero, cliente, date, preco),dataentrega) = "Entregue no prazo"
    |otherwise = "Nao foi entregue"

getprice :: Fractional d => ((a, b1, c, d), b2) -> ((a, b1, c, d), b2)
getprice ((numero, cliente, date, price),dataentrega) = ((numero, cliente, date, price*1.2),dataentrega)

escreverArquivo x = do
            arq <- openFile "arquivo.txt" AppendMode
            hPutStrLn arq x
            hFlush  arq
            hClose arq

main:: IO ()
main = do

let cliente_1 = ("Fulano", 9999, "rua A")

let cliente_2 = ("Ciclano", 8888, "rua B")

let pedidosimples_cliente1 = (1, cliente_1, (23, 09, 2021), 500.00)

let pedido_cliente2 = getprice ((2, cliente_2, (23, 09, 2021), 500.00), (23, 09, 2021))

escreverArquivo (clienteToString cliente_1)
escreverArquivo (clienteToString cliente_2)
escreverArquivo (pedidosimplesToString pedidosimples_cliente1)
escreverArquivo (pedidoexpressoToString pedido_cliente2)

print (pedidosimplesToString pedidosimples_cliente1)
print (pedidoexpressoToString pedido_cliente2)
