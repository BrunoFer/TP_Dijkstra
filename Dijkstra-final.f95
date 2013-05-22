! Programa que elabora o cálcula da fórmula de Dijkistra para o menor caminho
! Aluno: Bruno Ferreira da Costa
! Disciplina: TCP/IP e Roteamento

program Dijkstra

!declaração de variáveis
integer :: x,y,z,verticeAtual,menorAtual,erro,verticeOrigem,verticeDestino,numeroParametros
integer :: posicao1 = 1, posicaoEspaco, qtdeNumeros = 0, numeroLinhas = 0,INFINITO
integer, dimension(2000,2000) :: matriz
integer, dimension(:), allocatable :: verticesOrigem
integer, dimension(:), allocatable :: distancias
integer, dimension(:), allocatable :: caminho
logical, dimension(:), allocatable :: verticeFixo
character(LEN=2000) linha
character(LEN=20) parametro
character(LEN=20) nomeArquivo
parameter (INFINITO=1000000)

! leitura dos parametros da linha de comando que informam o nome do arquivo, os vértices de origem e destino
numeroParametros = iargc()
if(numeroParametros < 3) then
    print *, 'Está faltando argumento!'
    return
end if
call getarg(1, parametro)
read(parametro, '(A)') nomeArquivo
call getarg(2, parametro)
read(parametro, '(i10)') verticeOrigem
call getarg(3, parametro)
read(parametro, '(i10)') verticeDestino

if (verticeDestino /= verticeOrigem) then
	
	!abrindo o arquivo
	open(10,file=nomeArquivo,iostat=erro)

	do
		! lê cada linha do arquivo e verifica se chegou ao final do arquivo

		read(10,'(A)',iostat=erro) linha
		if (erro < 0) exit
		
		! obtem os valores da matriz que estão na linha
		!print *,linha
		numeroLinhas = numeroLinhas + 1
		qtdeNumeros = 0
		do
			posicaoEspaco = index(linha(posicao1:), ' ')
			!print *,'posicao do espaco = ',posicaoEspaco
			if (posicaoEspaco==1) then
				exit
			end if
			!print *,'Caracter = ',linha(posicao1:posicao1+posicaoEspaco-2)
			qtdeNumeros = qtdeNumeros + 1
			read(linha(posicao1:posicao1+posicaoEspaco-2),'(i10)') matriz(numeroLinhas,qtdeNumeros)
			posicao1 = posicao1+posicaoEspaco
		end do
		
		posicao1 = 1

	end do
	
	!fechando o arquivo
	close(10)
	
	! alocando memória para os vetores
	allocate(verticesOrigem(numeroLinhas))
	allocate(distancias(numeroLinhas))
	allocate(verticeFixo(numeroLinhas))

	! inicializando os vetores utilizados para armazenar as distâncias, vértices de origens e a condição de analisado ou não de cada vértice
	do x=1,numeroLinhas,1
		verticesOrigem(x) = 0
		distancias(x) = INFINITO
		verticeFixo(x) = .False.
	end do

	print *,''
	print *,'======= Terminada a leitura dos números do arquivo. Matriz Pronta! =========='

	verticeAtual = verticeOrigem
	distancias(verticeAtual) = 0

	!percorre todos os vértices da matriz. Quando todos os vértices forem analisados, o "DO" será encerrado.
	do x=1,numeroLinhas,1
		!print *,'------vertice analisado = ', verticeAtual,' -----'
		z=verticeAtual
		!percorre todos os vértices do grafo à procura de vizinhos do vértice analisado e que ainda não foram analisados
		do y=1,numeroLinhas,1
			
			! analisa se o vértice é vizinho do vértice analisado em questão
			if (matriz(verticeAtual,y) /= INFINITO .and. verticeAtual /= y) then
				
				! só verifica o vizinho se este não foi analisado ainda
				if (verticeFixo(y) .eqv. .False.) then
				
					!print *,'Vertice vizinho e nao Fixo = ',y
					! analisa se a distância do vértice que está sendo analisado é menor que a distância já marcada para ele.
					menorAtual = distancias(verticeAtual)+matriz(verticeAtual,y);
					if (menorAtual < distancias(y)) then
						distancias(y) = menorAtual
						distancias(y) = matriz(verticeAtual,y)+distancias(verticeAtual)
						verticesOrigem(y) = verticeAtual
					end if
					!print *,'Distancia = ',menorDistancia(y)
					!print *,'Vertice de Origem = ',verticesOrigem(y)
				
				end if
				
			end if
			
		end do
		
		! analisa o próximo vértice que será utilizado na análise. Tem que ser o vértice com menor valor de distância.
		verticeFixo(verticeAtual) = .True.
		menorAtual = INFINITO
		do y=1,numeroLinhas,1
			if (verticeFixo(y) .eqv. .False.) then
				if (distancias(y) < menorAtual) then 
					menorAtual = distancias(y)
					verticeAtual = y
				end if
			end if
		end do
		
		if (verticeAtual==z) exit
		
	end do

	! fazendo o caminho inverso, do destino até a origem, para obter o menor caminho
	verticeAtual = verticeDestino
	x = 1
	allocate(caminho(numeroLinhas))
	do
		caminho(x) = verticeAtual
		verticeAtual = verticesOrigem(verticeAtual)
		x = x + 1
		if (verticeAtual == verticeOrigem) exit
		if (verticeAtual == 0) exit
	end do
	caminho(x) = verticeAtual
	
	! só imprimindo os resultados
	print *,''
	print *,'======= Matriz analisada. Imprimindo o resultado ============'
	print *,''
	print *,'Nó de origem:',verticeOrigem
	print *,'Nó de destino:', verticeDestino
	if (verticeAtual/=0) then
		print *,'Rota: '
		do y=1,x,1
			print '(i10)',caminho(x-y+1)
		end do
		print *,'Custo:',distancias(verticeDestino)
	else
		print *,'### Não há rota para este destino! ###'
		print *,'Custo: -'
	end if
	
	! liberando memória dos vetores
	deallocate(verticesOrigem)
	deallocate(verticeFixo)
	deallocate(distancias)
	deallocate(caminho)
	
else
	print *,'Rota: ',verticeOrigem
	print *, 'Custo: 0'
end if

print *,'============================================================='
print *,''

END
