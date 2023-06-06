####Bibliotecas####
install.packages("mosaicCalc")
install.packages("ggplot2")
install.packages("treemapify")
install.packages("gganimate")
install.packages("gifski")
install.packages("png")
library(mosaicCalc)
library(ggplot2)
library(treemapify)
library(gganimate)
library(gifski)
library(png)


####Começo####
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


#Base de Dados
idade=c(18,20,23,25)
sexo=c('M', 'F', 'F', 'M')

dados = data.frame(idade, sexo)
dados

(dados1 = read.csv('dados01.csv',sep = ',',dec = ',',header = TRUE))

(dadosTeste = read.csv('dados02.csv', sep=',',dec = ',', header = TRUE))
(dados1$classe=c('a', 'a', 'b', 'b'))
names(dados1)
head(dados1, 2)
dim(dados1)

dados1[c(1,3),1]
dados1[1:3, 1]
dados1[4,3] = 'c'
dados1$tipo=ifelse(dados1$idade<=20, 'jovem', 'tmj')

(dadosEx = read.csv('dados03.csv', sep = ',', dec = ',', header = TRUE))

imc = function(peso, altura){
  round(peso/(altura^2),2)
}

dadosEx$imc=imc(dadosEx$peso, dadosEx$altura)
dadosEx$imcCat=ifelse(dadosEx$imc>=19, 'normal', 'baixo')


(dados = read.csv('dados_plot.csv', sep = ',', dec = '.', header = TRUE))
head(dados)

names(dados)
names(dados) = c('linha', 'empresas', 'rendimento', 'tamanho', 'vendas', 'palavra', 'fre_palavra')
head(dados)

#Retorna a quantidade de empresas de cada tipo em uma tabela
table(dados$empresas)

#Média de rendimentos por empresa
#          valores                  por          medida
tapply(dados$redimento, dadosPlot$empresas, mean)

#### grafico de coluna / linha ####
ggplot(dados, aes(x=empresas))
ggplot(dados, aes(x=empresas)) + geom_bar()
ggplot(dados, aes(x=empresas)) + geom_bar() + coord_flip()
ggplot(dados, aes(x=empresas, fill=empresas)) + geom_bar()
ggplot(dados, aes(x=empresas, fill=empresas)) + geom_bar() + labs(title = 'Titulo', x = 'Empresas', y = 'Total') + 
  coord_flip()

ggplot(dados, aes(x=empresas, fill=empresas)) + geom_bar() + labs(title = 'Titulo', x = 'Empresas', y = 'Total') + 
  coord_flip() + scale_fill_brewer(palette = 'Greys')

ggplot(dados, aes(x=empresas, fill=empresas)) + geom_bar() + labs(title = 'Titulo', x = 'Empresas', y = 'Total') + 
  coord_flip() + scale_fill_brewer(palette = 'Greys') + theme_minimal()

ggplot(dados, aes(x=empresas, fill=tamanho)) + geom_bar() + labs(title = 'Titulo', x = 'Empresas', y = 'Total') + 
  coord_flip() + scale_fill_brewer(palette = 'YlGn') + theme_minimal()

ggplot(dados, aes(x=empresas, fill=tamanho)) + geom_bar() + labs(title = 'Titulo', x = 'Empresas', y = 'Total') + 
  scale_fill_brewer(palette = 'YlGn') + theme_minimal() + facet_wrap(~tamanho)


#### grafico de dispersão ####
ggplot(dados, aes(x=rendimento, y=vendas)) + geom_point()

ggplot(dados, aes(x=rendimento, y=vendas)) + geom_point() + geom_smooth()

ggplot(dados, aes(x=rendimento, y=vendas, col=empresas)) + geom_point() + geom_smooth()

ggplot(dados, aes(x=rendimento, y=vendas, col=empresas)) + geom_point() + geom_smooth(se=FALSE)

ggplot(dados, aes(x=rendimento, y=vendas, col=empresas)) + geom_point() + geom_smooth(se=FALSE) +
  facet_wrap(~empresas) + theme_classic()


#### grafico pizza ####
table(dados$empresas)
dados1 = as.data.frame(table(dados$empresas))
names(dados1)[1] = 'Empresa'

dados2 = dados1
dados2$Freq = round(dados2$Freq/sum(dados2$Freq), 3)
dados2$cum = cumsum(dados2$Freq)

ggplot(dados2, aes(x ="", y = Freq,fill = Empresa)) +  geom_bar(width = 1, stat="identity") + coord_polar("y",start = 0) +
  scale_fill_brewer(palette="Set3")

#text: geom_text()
ggplot(dados2, aes(x ="", y = Freq,fill = Empresa)) +
  geom_bar(width = 1, stat="identity") + coord_polar("y",start = 0) +
  scale_fill_brewer(palette="Set3")+geom_text(aes(y = cum-0.2, label = Freq), color ="black")

ggplot(dados2, aes(x =2, y = Freq,fill = Empresa)) +
  geom_bar(width = 1, stat="identity") + coord_polar("y",start = 0) +
  scale_fill_brewer(palette="Set3")+geom_text(aes(y = cum-0.2, label = Freq), color ="black") + xlim(0.5, 2.5)

#### grafico histograma ####
#Utiliza para valores continuos 
ggplot(dados, aes(rendimento)) + geom_histogram()
#numero de grupos : gins
ggplot(dados, aes(rendimento)) + geom_histogram(bins = 5)
#cor: fill
ggplot(dados, aes(rendimento)) + geom_histogram(bins = 5, fill='orange')
#aes(fill= empresa)
ggplot(dados, aes(rendimento)) + geom_histogram(aes(fill=empresas), bins=5)


#### grafico de densidade ####
#Utiliza-se para valores continuos também
ggplot(dados, aes(rendimento)) + geom_density()
#por empreas aes(fill=factor(empresa))
ggplot(dados, aes(rendimento)) + geom_density(aes(fill=factor(empresas)))
#transparencia: (alpha=)
ggplot(dados, aes(rendimento)) + geom_density(aes(fill=factor(empresas)),alpha=0.5)


#### grafico boxplot ####
#Utiliza-se para valores continuos
ggplot(dados, aes(y=rendimento)) + geom_boxplot()
ggplot(dados, aes(x=empresas, y=rendimento)) + geom_boxplot()
ggplot(dados, aes(x=empresas, y=rendimento, fill=empresas)) + geom_boxplot()
ggplot(dados, aes(x=empresas, y=rendimento, fill=empresas)) + geom_boxplot() + facet_wrap(~tamanho)


#### grafico violino ####
#utiliza-se para valores continuos
ggplot(dados, aes(x=empresas, y=rendimento, fill=empresas)) + geom_violin() 
ggplot(dados, aes(x=rendimento, y=empresas, fill=empresas)) + geom_violin()


####rendimento medio por tamanho x empresa####

ggplot(dados1, aes(area=Freq, fill=Empresa)) + geom_treemap()

dados$interacao = interaction(dados$tamanho, dados$empresas)
dados3 = as.data.frame(tapply(dados$rendimento, dados$interacao, mean))
names(dados3)[1] ='total'

dados3$nomes = labels(dados3)[[1]]
ggplot(dados3, aes(area=total, fill=nomes)) + geom_treemap()


#### nuvem de letras ####
dados4 = dados[,6:7]
wordcloud2(data=dados4, size=1.6)



####Prova1####
dados_p = read.csv("dados1.csv");
head(dados_p)
ggplot(dados_p, aes(x=fabricante, fill=tamanho)) + geom_bar() + labs(title = 'Fabricante x Tamanho', x = 'Fabricante', y = 'Tamanhos') +
  scale_fill_brewer(palette = 'YlGn')

ggplot(dados_p, aes(x = fabricante, y=resistencia, fill = fabricante)) + geom_boxplot() + facet_wrap(~tamanho) +
  labs(title='Resistencia x Frabricante', x = 'Fabricante', y = 'Resistencia')


interacao=interaction(dados_p$fabricante,dados_p$tamanho)
dados1=as.data.frame(tapply(dados_p$resistencia, interacao, mean))
dados1$Fabricante_Tamanho=labels(dados1)[[1]]
names(dados1)[1] = 'media'
ggplot(dados1, aes(area=media, fill=Fabricante_Tamanho)) + geom_treemap() + labs(title = 'Média de Resistência de Produtos') +
  scale_fill_brewer(palette = "Set3")

ggplot(dados_p, aes(x=tempo, y=peso, col=tamanho)) + geom_point() + geom_smooth(se=FALSE) +
   theme_classic() + labs(title = 'Tempo x Peso', x = 'Tempo', y = 'Peso')

ggplot(dados_p, aes(x=fabricante, y=resistencia, fill=fabricante)) + geom_violin() + 
  labs(titles = 'Resistência x Fabricante', x = 'Fabricante', y = 'Resistência')





####Modelo Binominal n = n####
#dbinom(quantidade de sucessos, quantidade de tentativas, probabilidade de sucesso)
p1 = dbinom(2, 5, (2/6)) 
p2 = dbinom(0:5, 5, (2/6)) 
#pbinom(quantidade de sucessos menor ou igual a x, ...)
pbinom(2, 5, (2/6))
#qbinom(probabibilidade, ...)
qbinom(0.7901, 5, (2/6))
#rbinom(numero de amostra, ...) = gera a quantidade de sucessos 
rbinom(10, 5, (2/6))

#Gráficos 
##Criando base de dados
#dbinom
x = 0:5
px = dbinom(x, 5, 2/6)
dados = data.frame(x, px)
#pbinom
x = 0:5
px = pbinom(x, 5, 2/6)
dados = data.frame(x, px)


##Gerando o grafico 
ggplot(dados, aes(x, px)) + geom_col()



####Modelo Poisson n = infinito####
#dpois(prababilidade, media)
dpois(35, 40)
dpois(0:480, 40)
#ppois(probabilidade ou menos, media)
sum(dpois(0:45, 40))
ppois(45, 40)
#qpois(valor da probabilidade, media)
qpois(0.8096, 40)
#rpoid(amostra, media)
rpois(10,40)

##Grafico
x = 0:100;
px = dpois(x, 45)
dados = data.frame(x, px)
ggplot(dados, aes(x, px)) + geom_col()



####Modelo Exponencial####
#Usado para geralmente calcular tempo 
#Não existe igualdade em modelos continuos 
#exp(x, media)
dexp(6, 1/7) ##Sempre vai ser 0
pexp(5, 1/7) #5 ou menos
qexp(0.5, 1/7)#probabilidade da mediana 
rexp(10, 1/7)

#Verificando que tipo de modelo usar 
tempos = rexp(500, 1/7);
px = dexp(tempos, 1/7);
dados = data.frame(tempos, px)
ggplot(dados, aes(tempos)) + geom_histogram(aes(y=..density..)) + geom_line(aes(tempos, px), col='red')

####Modelo normal####
#norm(x, media, desvio padrao)
dnorm(20, 50, 25)#sempre 0
p = 1-pnorm(20, 50, sqrt(25))#probabilidade de ser maior que 20 (100% - pnorm)
##Ou
pnorm(20, 50, 5, lower.tail = FALSE) #assim já pega maior que 20

#Determinando o primeiro quartil (0.25 ou 25%)
qnorm(0.25, 50, 5)

#Amostra aleatoria de tamanho 20
rnorm(20, 50, 5)

#Grafico 
pesos = rnorm(700, 50, 5)
px = dnorm(pesos, 50, 5)
dados = data.frame(pesos, dx)
ggplot(dados, aes(pesos)) + geom_histogram(aes(y = ..density..)) + geom_line(aes(pesos, dx), col = 'red')


####Exercicio p/ prova####
#Ex1
k = 1/sum(1:10)


#Ex2
#dbinom(quantidade de sucessos, quantidade de tentativas, probabilidade de sucesso)
#a)
p1 = dbinom(0, 10, 0.3)

#b)x>=1
#pbinom(quantidade de sucessos menor ou igual a x, ...)
p2 = 1 - p1 #ou
p2 = sum(dbinom(1:10,10, 0.3))

#c)x>=3
p3 = 1 - pbinom(2, 10, 0.3) #ou
p3 = sum(dbinom(3:10, 10, 0.3))

#d) 
#p4 = mean(rbinom(50, 10, 0.3))
p4 = 0.3 * 50


#Ex3
#a) 
p1 = dpois(4, 4)

#b)
p2 = 1 - ppois(5, 4)

#c) 
media = 4 * 24
p3 = ppois(85, media)

#d)
x = 0:20;
px = dpois(x, 4)
dados = data.frame(x, px)
ggplot(dados, aes(x, px)) + geom_col()


#Ex4 
f = function(x) {x^4}
integral = integrate(f, 0, 1)
k = 1/0.2

#Ex5
dadosEx5 = read.csv('dadosex5.csv')
ggplot(dadosEx5, aes(x)) +  geom_histogram(bins = 5, fill='orange')

#a)
#Modelo exponencial

#b)
media = mean(dadosEx5$x)

#c) 
p1 = 1 - pexp(2, 1/media)

#d)
p2 = pexp(5, 1/media)
dadosEx5$px =  dexp(dadosEx5$x, 1/ media)
ggplot(dadosEx5, aes(x, px)) + geom_histogram(aes(y=..density..)) + geom_line(col='red')


#Ex6
dadosEx6 = read.csv('dadosex6.csv')
head(dadosEx6)
ggplot(dadosEx6, aes(x)) + geom_histogram(bins = 5, fill = 'green')

#a)
#Modelo Normal

#b)


####Revisao prova####
#Ex1
xi = 1:10;
p = 1;
k = p / sum(xi)

#Ex2 
#a) nenhum falhar 
p1 = dbinom(0, 10, 0.3)

#b) no minimo 1 falhar 
p2 = sum(dbinom(1:10, 10, 0.3))

#c) ao menos 3 falharem
p3 = 1 - pbinom(2, 10, 0.3) 

#d) amostra de 50, quantos falham em média
p4 = 50 * 0.3

#e) grafico de todas as probabilidade de falhar
x = 0:10
px = dbinom(x, 10, 0.3)
dados = data.frame(x, px)
ggplot(dados, aes(x, px)) + geom_col() + theme_minimal()

#Ex3
#a) todos 4 serem defeituosos em 1 hr
p1 = dpois(4, 4)

#b) 6 ou mais defeituosos em 1 hr
p2 = 1 - ppois(5, 4)

#c) 80 defeituosos em 24 hrs
p3 = dpois(80, 4*24)

#d) grafico defeituosos em 1 hr
x = 0:15;
px = dpois(x, 4)
dados = data.frame(x, px)
ggplot(dados, aes(x, px)) + geom_col()


#Ex4
f = function(x) {x^4}
int = integrate(f, 0, 1)
k = 1/0.2


#Ex5
dados = read.csv('dadosex5.csv')
head(dados)
#a)
x = dados$x
px = dexp(x, 1/mean(x));
dados = data.frame(x, px)
ggplot(dados, aes(x)) + geom_histogram(aes(y=..density..)) + geom_line(aes(x, px), col='red')
#resp : modelo exponencial

#b) 
mean(x)

#c)
p3 = 1 - pexp(2, 1/mean(x))

#d)
p4 = pexp(5, 1/mean(x))

#e)
ggplot(dados, aes(x)) + geom_histogram(aes(y=..density..)) + geom_line(aes(x, px), col='red')


#Ex6
dados = read.csv('dadosex6.csv')
head(dados)
ggplot(dados, aes(x)) + geom_histogram()

#a)
#resp: modelo normal

#b) pesar entre 15 e 25
media = mean(dados$x)
variancia = sd(dados$x)
p2 = pnorm(25, media, variancia) - pnorm(15, media, variancia) 

#c) descartar 20% dos produtos de menor peso, determinar o peso limite
p3 = qnorm(0.2, media, variancia)

#d) grafico ajustado
x = dados$x
px = dnorm(x, media, variancia);
dados = data.frame(x, px)
ggplot(dados, aes(x)) + geom_histogram(aes(y=..density..)) + geom_line(aes(x, px), col='red')



####Prova 2####
dadosEx1 = read.csv("ex1.csv")
dadosEx4 = read.csv("ex4.csv")

##Ex1
#a)
head(dadosEx1)
ggplot(dadosEx1, aes(tempo)) + geom_histogram()
#Modelo exponencial

#b)
mediaEx1 = mean(dadosEx1$tempo)
p1 = pexp(300, 1/mediaEx1)

#c)
mediaEx1 = mean(dadosEx1$tempo)
p2 = pexp(400, 1/mediaEx1) - pexp(200, 1/mediaEx1)

#d)
mediaEx1 = mean(dadosEx1$tempo)
p3 = qexp(0.7, 1/mediaEx1)

#e)
mediaEx1 = mean(dadosEx1$tempo)
tempo = dadosEx1$tempo
px = dexp(tempo, 1/mediaEx1);
dados = data.frame(tempo, px)
ggplot(dados, aes(tempo)) + geom_histogram(aes(y=..density..)) + geom_line(aes(tempo, px), col='red') + theme_minimal()

##Ex2
#a)
#30% apresentam falha 
#70% não apresentam falha 
p1 = sum(dbinom(75:100, 100, 0.7))
p1 = 1 - pbinom(25, 100, 0.3)

#b)
p2 = dbinom(70, 100, 0.7)

#c) 
p3 = sum(dbinom(1:20, 100, 0.3))

#d)
p4 = 4000 * 0.3

#e)
x = 0:100
px = dbinom(x, 100, 0.7)
dados = data.frame(x, px) 
ggplot(dados, aes(x, px)) + geom_col() + labs(x = 'Produtos', y = 'Probabiblidade de Falha')

#Ex3
#a)
p1 = dpois(15, 20)

#b)
p2 = ppois(30, 20) - ppois(20, 20)

#c)
p3 = 1 - ppois(449, 20*24)

#d)
p4 = 1 - ppois(34, 30)

#e)
x = 0:40;
px = dpois(x, 20)
dados = data.frame(x, px)
ggplot(dados, aes(x, px)) + geom_col() + labs(x = 'Erros', y = 'Probabiblidade de Erros/Hora')

#Ex4
#a)
dadosEx4 = read.csv("ex4.csv")
head(dadosEx4)
ggplot(dadosEx4, aes(dadosEx4$peso)) + geom_histogram()

#b)
media = mean(dadosEx4$peso)
desvio = sd(dadosEx4$peso)
p1 = 1 - (pnorm(55, media, desvio) - pnorm(45, media, desvio))

#c) 
media = mean(dadosEx4$peso)
desvio = sd(dadosEx4$peso)

#d)
media = mean(dadosEx4$peso)
desvio = sd(dadosEx4$peso)
leve = qnorm(0.3, media, desvio)
medio = qnorm(0.4, media, desvio)
pesado = qnorm(0.7, media, desvio)


#e)
media = mean(dadosEx4$peso)
desvio = sd(dadosEx4$peso)
peso = dadosEx4$peso

px = dnorm(peso, media, desvio)
dados = data.frame(peso, px)

ggplot(dados, aes(peso)) + geom_histogram(aes(y = ..density..)) + geom_line(aes(peso, px), col = 'red')






####Intervalo de confiaça####
dados = read.csv('ex1Aula09.csv')
head(dados)
mean(dados$tamanho)
#95%
t.test(dados$tamanho, conf.level = 0.95)
#99%
t.test(dados$tamanho, conf.level = 0.99)
#100%
t.test(dados$tamanho, conf.level = 1)

##Proporção
# 8 em 20 produtos 
prop.test(8, 20, conf.level = 0.95)
# 12 em 20 produtos 
prop.test(12, 20, conf.level = 0.95)



####Teste de Hipotese####
dados1 = read.csv('ex1Aula10.csv')
dados2 = read.csv('ex2Aula10.csv')
dados3 = read.csv('ex3Aula10.csv')

#h0 : media = 20
#h1 : media != 20
t.test(dados1$tamanho, alternative = 'two.side', mu = 20, conf.level = 0.94)

#h0 : media = 18
#h1 : media < 18
t.test(dados1$tamanho, alternative = 'less', mu = 18, conf.level = 0.96)

#h0 : media = 16
#h1 : media > 16
t.test(dados1$tamanho, alternative = 'greater', mu = 16, conf.level = 0.97)

##Proporção
#h0 : p = 0.5
#h1 : p != 0.5
prop.test(24, 50, alternative = "two.side", p = 0.5, conf.level = 0.98)

#h0 : p = 0.65
#h1 : p < 0.65
prop.test(24, 50, alternative = "less", p = 0.65, conf.level = 0.94)


##Duas Amostras Independentes
head(dados2)
#h0 : media1 - media2 = 0
#h1 : meida1 - media2 != 0
t.test(dados2$a, dados2$b, alternative = 'two.side', conf.level = 0.95)

#h0 : media1 - media2 = 0
#h1 : media1 - media2 < 0
t.test(dados2$a, dados2$b, alternative = 'less', conf.level = 0.95)

#h0 : media1 - media2 = 0
#h1 : media1 - media2 > 0
t.test(dados2$a, dados2$b, alternative = 'greater', conf.level = 0.95)

##Proporção com duas amostras
#h0 : 
prop.test(c(20, 30), c(50, 80), alternative = 'two.side', conf.level = 0.95)


##Duas Amostras Dependentes 
head(dados3)
#h0 : media1 - media2 = 0
#h1 : media1 - media2 != 0 
t.test(dados3$antes, dados3$apos, alternative = 'two.side', conf.level = 0.95, paried = T)


####Exercicios P3####
dadosEx1 = read.csv('ex1P3.csv')
dadosEx2 = read.csv('ex2P3.csv')
dadosEx5 = read.csv('ex5P3.csv')
dadosEx6 = read.csv('ex6P3.csv')

##Exercicio 1
head(dadosEx1)
t.test(dadosEx1$altura, conf.level = 0.93)
ggplot(dadosEx1, aes(dadosEx1$altura)) + geom_histogram(bins = 6) + geom_vline(xintercept=19.41,col='green') + 
  geom_vline(xintercept = 18.64, col = "blue") + geom_vline(xintercept = 20.19, col = "red")

##Exercicio 2
head(dadosEx2)
table(dadosEx2)
prop.test(107, 200, conf.level = 0.97)

##Exercício 3
#h0 : mu = 21
#h1 : mu < 21
head(dadosEx1)
t.test(dadosEx1$altura, alternative = "less", mu = 21, conf.level = 0.95)
ggplot(dadosEx1, aes(altura)) + geom_histogram(bins = 6) + geom_vline(xintercept = 20.11, col = "blue") +
  geom_vline(xintercept = mean(dadosEx1$altura), col = "red")

##Exercicio 4
#h0 : p = 60% 
#h1 : p != 60%
#conf = 2%
#alpha = 0.02 
head(dadosEx2)
table(dadosEx2)
prop.test(107, 200, alternative = 'two.side', p = 0.6, conf.level = 0.98)
#LIMITES: 0.4506527 0.6174476
# ou 45% e 61%
#Neste caso não pode-se afirmar a proporção de equipamentos quebrados é diferente de 60%
# e também não se pode afirmar que a proporção de equipamentos quebrados é igual a 60%

##Exercicio 5 
#Independentes 
head(dadosEx5)

ggplot(dadosEx5, aes(y=temperaturaA, x=temperaturaB)) + geom_boxplot()
ggplot(dadosEx5, aes(y=temperaturaB)) + geom_boxplot()

#Fazendo por dois boxplot
ggplot(dadosEx5, aes(y=temperaturaA)) + geom_boxplot(fill='green') + geom_boxplot(aes(y=temperaturaB,x=1,fill='red'))

#Fazendo por data frame
y=c(dadosEx5$temperaturaA,dadosEx5$temperaturaB)
tipo=c(rep(c('A','B'),each=60))
dd2=data.frame(y,tipo)
ggplot(dd2,aes(tipo,y,fill=tipo))+geom_boxplot()

#h0 : m1 - m2 = 0
#h1 : m1 - m2 > 0 #desconsidero 
#alpha = 0.06
t.test(dadosEx5$temperaturaA, dadosEx5$temperaturaB, alternative = 'greater', conf.level = 0.94)
#Pode-se concluir que a marca A tem uma resistencia maior que a da marca B

##Exercicio 6
#h0 : media1 - media2 = 0
#h1 : media1 - media2 > 0
head(dadosEx6)
ggplot(dadosEx6, aes(y= antes-apos)) + geom_boxplot()
t.test(dadosEx6$antes, dadosEx6$apos, alternative = 'less', conf.level = 0.95)

####Prova3####
dadosEx1 = read.csv("Prova3/p3ex1.csv");
dadosEx2 = read.csv("Prova3/p3ex2.csv");
dadosEx3 = read.csv("Prova3/p3ex3.csv");
dadosEx4 = read.csv("Prova3/p3ex4.csv");
dadosEx5 = read.csv("Prova3/p3ex5.csv");

##Questão 01
head(dadosEx1)
t.test(dadosEx1$peso, conf.level = 0.96)
ggplot(dadosEx1, aes(peso)) + geom_histogram(bins = 12) +
  geom_vline(xintercept = 50.63513, col = "blue") + geom_vline(xintercept = 47.51532, col = "red") +
  geom_vline(xintercept = 49.07522, col = 'yellow')

##Questão 02
head(dadosEx2)
table(dadosEx2$tamanho)
#De 100, 53 estão quebrados, nivel de confiança de 96%
prop.test(53, 100, conf.level = 0.96)

##Questão 03
head(dadosEx3)
#nivel de confiança de 95% | H0 : media = 50 -> Compra | H1 : media > 50 -> Não Compra
#nivel de significancia 5% ou 0.05, p-valor 0.0001, logo H0 é rejeitada.
t.test(dadosEx3$tempo, alternative = "greater", mu = 50, conf.level = 0.95)

##Questão 04
head(dadosEx4)
#antes e apos -> dependente
#confiança -> 94% ou 0.94 | alpha -> 6% ou 0.06
#H0 : antes = depois | H1 : antes < depois
t.test(dadosEx4$antes, dadosEx4$depois, alternative = "less", conf.level = 0.94, paired = T)
#p-valor = 0.0002991

##Questão 05
head(dadosEx5)
#maquinas diferentes -> independente
#confiança -> 92% ou 0.92 | alpha -> 8% ou 0.08
#H0 : mediaA - mediaB = 0 | H1 : mediaA - mediaB != 0
t.test(dadosEx5$A, dadosEx5$B, alternative = 'two.side', conf.level = 0.92)
#p-valor = 0.3487 > alpha | não rejeita-se H0, não afirma-se que os dois são diferentes

#H0 : mediaA - mediaB = 0 | H1 : mediaA > mediaB 
t.test(dadosEx5$A, dadosEx5$B, alternative = 'less', conf.level = 0.92)
#p-valor = 0.1743 595.2724  612.3158 