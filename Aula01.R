x=1:4
z=matrix(x, nrow = 2, byrow = F) #Cria matriz com 2 linhas utilizando o vetor x, ordenando por coluna
z
z=matrix(x, nrow = 2, byrow = T) #Cria matriz com 2 linhas utilizando o vetor x, ordenando por linha
z
solve(z) #Inversa da matriz
t(z)
determinant(z)
D(expression(x^2), 'x')
D(expression(tan(x)*sin(x)^2), 'x')

install.packages("mosaicCalc")

#funcoes 
func=function(x){
  2*x-1
}
func(1)
#curva de -10 a 10
curve(func, -10, 10)
#linha h=horiz. v=vert.
abline(h=0, col='red')

#raiz
uniroot(func, lower = 0, upper = 4)

func2=function(x){
  2*x^2-3*x-4
}
curve(func2, -2, 3)
abline(h=0, v=0)

multiroot(func2, start = c(-1,2))
abline(v=c(-0.850,2.350), col=2)

integrate(func, 0, 2)

#ex1
#Crie uma funcao para a equação
func3=function(x){
  x^2-5*x+6
}
#Faca um grafico para verificar as raizes
curve(func3, 1, 3)
abline(h=0, v=0)

#Determine as raizes 
multiroot(func3, start = c(2,3))
abline(v=c(2, 3), col='green')

#Calcule a àrea entre as raizes
integrate(func3, 2, 3)
