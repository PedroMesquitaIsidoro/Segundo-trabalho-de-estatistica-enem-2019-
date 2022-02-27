
#Trabalho Probabilidade e EstatÃ­stica LÃ­via Soares e Pedro Henrique Isidoro
#Lendo o arquivo em formato .xlsx
library(tidyverse)
library(viridis)
ENEM <- read.csv2('ENEM_AL.csv' , sep=',' , dec=',')


#1)Utilizando a coluna NOTA_ENEM, elaborar um intervalo de confiança com 5% e  1% de nível de significância

#5%
alfa = 0.05
n = length(ENEM$NOTA_ENEN)
n
desvio = sd(ENEM$NOTA_ENEN)
desvio
media = mean(ENEM$NOTA_ENEN)
media

tc = qt(p = 1- alfa/2, df = n - 1)
tc = round(tc, 3)
tc

erro = tc * desvio/sqrt(n)
erro = round(erro, 3)
erro

cat('[',media - erro, ',',media + erro, ']')


#1%

alfa = 0.01
n = length(ENEM$NOTA_ENEN)
n
desvio = sd(ENEM$NOTA_ENEN)
desvio
media = mean(ENEM$NOTA_ENEN)
media

tc = qt(p = 1- alfa/2, df = n - 1)
tc = round(tc, 3)
tc

erro = tc * desvio/sqrt(n)
erro = round(erro, 3)
erro

cat('[',media - erro, ',',media + erro, ']')

##################################################################################
#2)Utilizando a base dados, escolha um colégio, utilizando o atributo CO_ESCOLA 
#letra a
#Chamar alguns pacotes para serem usados
if(!require(dplyr)) install.packages("dplyr") # Instalação do pacote caso não esteja instalado
library(dplyr)

View(ENEM)
glimpse(ENEM)

#criando um data frame pra escola 27049140
ESCOLASdf <- data.frame(ENEM)
codigo <- c(27049140)
select.escola <- subset(ESCOLASdf, `CO_ESCOLA` %in% codigo)
select.escola

#shapiro teste
shapiro.test(select.escola$NU_NOTA_CN)
shapiro.test(select.escola$NU_NOTA_CH)
shapiro.test(select.escola$NU_NOTA_LC)
shapiro.test(select.escola$NU_NOTA_MT)
shapiro.test(select.escola$NU_NOTA_REDACAO)

###################################################################################

### letra c

if(!require(dplyr)) install.packages("dplyr") # Instalação do pacote caso não esteja instalado
library(dplyr)
#if(!require(ggpubr)) install.packages("ggpubr") # Instalação do pacote caso não esteja instalado
#library(ggpubr)
#if(!require(MESS)) install.packages("MESS") # Instalação do pacote caso não esteja instalado
#library(MESS)

if(!require(RVAideMemoire)) install.packages("RVAideMemoire") # Instalação do pacote caso não esteja instalado
library(RVAideMemoire)                                        # Carregamento do pacote
if(!require(car)) install.packages("car") # Instalação do pacote caso não esteja instalado
library(car)                                # Carregamento do pacote


View(ENEM)
glimpse(ENEM)

#separando as meninas
SEXOdf <- data.frame(select.escola)
cod <- c('Feminino')
meninas <- subset(SEXOdf, `TP_SEXO` %in% cod)
meninas

#separando os meninos
cod2 <- c('Masculino')
meninos <- subset(SEXOdf, `TP_SEXO` %in% cod2)
meninos

#SHAPIRO PRA OS SEXOS
byf.shapiro(NU_NOTA_CN ~  TP_SEXO, select.escola)
byf.shapiro(NU_NOTA_CH ~  TP_SEXO, select.escola)
byf.shapiro(NU_NOTA_LC ~  TP_SEXO, select.escola)
byf.shapiro(NU_NOTA_MT ~  TP_SEXO, select.escola)
byf.shapiro(NU_NOTA_REDACAO ~  TP_SEXO, select.escola)


#verifica homogeneidade de variancias
leveneTest(NU_NOTA_CN ~ TP_SEXO, select.escola , center=mean)
leveneTest(NU_NOTA_CH ~ TP_SEXO, select.escola , center=mean)
leveneTest(NU_NOTA_LC ~ TP_SEXO, select.escola , center=mean)
leveneTest(NU_NOTA_MT ~ TP_SEXO, select.escola , center=mean)
leveneTest(NU_NOTA_REDACAO ~ TP_SEXO, select.escola , center=mean)


#REALIZA O TESTE-T
t.test(NU_NOTA_CN ~ TP_SEXO, select.escola, var.equal=TRUE)
t.test(NU_NOTA_CH ~ TP_SEXO, select.escola, var.equal=TRUE)
t.test(NU_NOTA_LC ~ TP_SEXO, select.escola, var.equal=TRUE)
t.test(NU_NOTA_MT ~ TP_SEXO, select.escola, var.equal=TRUE)
t.test(NU_NOTA_REDACAO ~ TP_SEXO, select.escola, var.equal=TRUE)



###################################################################################
### letra d

par(mfrow=c(1,5)) # Estabeleci que quero que os gráficos saiam na mesma linha
boxplot(NU_NOTA_CN ~ TP_SEXO, data = select.escola, ylab="Ciências da  Natureza", xlab="Sexo")
boxplot(NU_NOTA_CH ~ TP_SEXO, data = select.escola, ylab="Ciências Humanas", xlab="Sexo")
boxplot(NU_NOTA_LC ~ TP_SEXO, data = select.escola, ylab="Linguagens", xlab="Sexo")
boxplot(NU_NOTA_MT ~ TP_SEXO, data = select.escola, ylab="Matemática", xlab="Sexo")
boxplot(NU_NOTA_REDACAO ~ TP_SEXO, data = select.escola, ylab="Redação", xlab="Sexo")

####################################################################################

# 3 questão

ENEM <- read.csv('vasco.csv', sep = ';' , dec = ',')

#Chamar alguns pacotes para serem usados
if(!require(dplyr)) install.packages("dplyr") # Instalação do pacote caso não esteja instalado
library(dplyr)


View(ENEM)
glimpse(ENEM)

######## cidades #####

### leste de Alagoas
cidades1 <- c("Chã Preta", "Santana do Mundaú", "Ibateguara", "Viçosa", "São José da Laje", "Atalaia", "Campestre", "Pindoba", "União dos Palmares", "Flexeiras", "Jundiá", "Murici", "São Luís do Quitunde", "Branquina", "Capela", "Jacuípe", "Matriz de Camaragibe", "Novo Lino", "Cajueiro", "Colônia Leopoldina", "Joaquim Gomes", "Messias", "Porto Calvo", "Japaratinga", "Porto de Pedras", "Maragogi", "São Miguel dos Milagres", "Passo de Camaragibe", "Barra de Santo Antônio", "Maceió", "Pilar", "Satuba", "Barra de São Miguel", "Marechal Deodoro", "Rio Largo", "Coqueiro Seco", "Paripueira", "Santa Luzia do Norte", "Anadia", "Coruripe", "Roteiro", "Boca da Mata", "Jéquia da Praia", "São Miguel dos Campos", "Campo Alegre", "Junqueiro", "Teotônio Vilela", "Feliz Deserto", "Piaçabuçu", "Igreja Nova", "Porto Real do Colégio", "Penedo" )
regiao1 <- subset(ESCOLASdf, `NO_MUNICIPIO_PROVA` %in% cidades1)

### Sertão de Alagoas 
cidades2 <- c("Água Branca", "Mata Grande", "Canapi", "Pariconha", "Inhapi", "Delmiro Gouveia", "Olho d'Água do Casado", "Piranhas", "Carneiros", "Ouro Branco", "Poço das Trincheiras", "Senador Rui Palmeira", "Dois Riachos", "Palestina", "Santana do Ipanema", "Maravilha", "Pão de Açúcar", "São José da Tapera", "Batalha", "Jaramataia", "Olho d'Água das Flores", "Belo Monte", "Major Isidoro", "Olivença", "Jacaré dos Homens", "Monteirópolis")
regiao2 <- subset(ESCOLASdf, `NO_MUNICIPIO_PROVA` %in% cidades2)

### Agreste de Alagoas
cidades3 <- c("Belém", "Igaci", "Minerador do Negrão", "Quebrangulo", "Cacimbinhas", "Mar Vermelho", "Palmeira dos Índios", "Tanque d'Arca", "Estrela de Alagoas", "Maribondo", "Paulo Jacinto", "Arapiraca", "Craíbas", "Lagoa da Canoa", "Taquarana", "Campo Grande", "Feira Grande", "Limoeiro de Anadia", "Coité do Nóia", "Girau do Ponciano", "São Sebastião", "Olho d'Água Grande", "São Brás", "Traipu")
regiao3 <- subset(ESCOLASdf, `NO_MUNICIPIO_PROVA` %in% cidades3)

media_regiao1 <- mean(regiao1$NOTA_ENEN)
media_regiao1
media_regiao2 <- mean(regiao2$NOTA_ENEN)
media_regiao2
media_regiao3 <- mean(regiao3$NOTA_ENEN)
media_regiao3


varia_regiao1 <- var(regiao1$NOTA_ENEN)
varia_regiao1
varia_regiao2 <- var(regiao2$NOTA_ENEN)
varia_regiao2
varia_regiao3 <- var(regiao3$NOTA_ENEN)
varia_regiao3

#TESTE DE VARIÂNCIA LESTE(1) X SERTÃO(2)
F.R1R2 <- varia_regiao1/varia_regiao2
var.test(regiao1$NOTA_ENEN, regiao2$NOTA_ENEN)

#TESTE DE VARIÂNCIA LESTE(1) X AGRESTE(3)
F.R1R3 <- varia_regiao1/varia_regiao3
var.test(regiao1$NOTA_ENEN, regiao3$NOTA_ENEN)

#TESTE DE VARIÂNCIA SERTÃO(2) X AGRESTE(3)
F.R2R3 <- varia_regiao2/varia_regiao3
var.test(regiao2$NOTA_ENEN, regiao3$NOTA_ENEN)

par(mfrow=c(1,3)) # Estabeleci que quero que os gráficos saiam na mesma linha
boxplot(NOTA_ENEN ~ TP_SEXO, data = regiao1, ylab="LESTE NOTAS", xlab="Sexo")
boxplot(NOTA_ENEN ~ TP_SEXO, data = regiao2, ylab="SERTÃO NOTAS ", xlab="Sexo")
boxplot(NOTA_ENEN ~ TP_SEXO, data = regiao3, ylab="AGRESTE NOTAS", xlab="Sexo")

####################################################################################

#4)Utilizando as médias dos 26 municípios de alagoas elabore uma matriz de
#correlação entre as notas das 5 disciplinas do ENEM. Escolha as duas disciplinas de
#maior correlação e gerar a equação de regressão, o coeficiente de determinação,
#elabore um teste de hipótese para validar a correlação e comente os resultados.
library(corrplot)

Notas <- ENEM[,c('NU_NOTA_CN','NU_NOTA_LC', 'NU_NOTA_CH', 'NU_NOTA_MT','NU_NOTA_REDACAO')]

Matriz <- cor(Notas)
Matriz

corrplot(Matriz, method = "number", type = "lower")

#Maior correlação entre a NU_NOTA_LC e a NU_NOTA_CH

N_LC_CH <- Notas[,c('NU_NOTA_LC', 'NU_NOTA_CH')]

plot(N_LC_CH, main = "Notas Linguagens X Humanas", xlim=c(300,800),ylim=c(300,800))
regressao <- lm(N_LC_CH$NU_NOTA_LC ~N_LC_CH$NU_NOTA_CH, data = N_LC_CH)
abline(regressao, col = "red")

#Teste de hipótese:

t.test(ENEM$NU_NOTA_LC ~ENEM$TP_SEXO)
t.test(ENEM$NU_NOTA_CH ~ENEM$TP_SEXO)





