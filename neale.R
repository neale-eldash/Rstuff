##################################################################################
################################ Fun??es Neale ###################################
######## Fun??es desenvolvidas para facilitar e automatizar o uso do R  ##########
########               Data de desenvolvimento: 18/06/2010              ##########
##################################################################################

##################################################################################
#Comandos para acessar fun??es do arquivo
##################################################################################

#source("neale.R");
#setwd('Y:\\DADOS\\ESTATISTICA\\DOUTORADO\\TESE');
#setwd('G:\\DADOS\\ESTATISTICA\\DOUTORADO\\TESE');
#setwd('C:\\NEALE');


#runApp("MyAppDir", launch.browser = rstudio::viewer)


##################################################################################
# RCommander, Rattle and JGR GUI - good for importing data
##################################################################################

install.packages("rattle", dependencies=TRUE)
require(rattle)
rattle()

install.packages(c("JGR","Deducer","DeducerExtras"))
library(JGR)
JGR()

install.packages("Rcmdr")
library(Rcmdr)


##################################################################################
#Comandos do R - memoria, velocidade e debug
##################################################################################

tracemem(x) #  -  endereco da memoria usada por x
gc() # - mostraobject.size quanta memoria foi utilizada
object.size(x) # - memoria utilizada pelo objeto x
system.time(f) # - tempo que leva para executar f
Rprof(f) # - tempo que leva para executar f discrimando cada sub-item 
library(compiler); cf <- cmpfun(f) # -  compila a funcao f, fazendo sua execucao mais rapida
debug(f) # - liga o modo debug toda vez que a funcao f e' executada
browser() # - para a execucao do codigo e permite olhar variaveis - coloca "n" pra ir passo a passo
# c - continua execucao ate o fim
# n - executa passo a passo dentro do browser
get('x') - seleciona o objeto x
eval(call('f',par))  - calcula o valor da funcao f com parametro 6
parse(text="x^2") - calcula a expressao a partir do texto, sem avaliar o resultado 
class(x) or mode(x) - mostra o tipo da variavel
sprintf("texto %d",8) - substitue o % pelo valor, usando a especificao d

##################################################################################
### Debugging and Profing

#Profing (tempo gasta em cada funcao/nivel)
Rprof()
Rprof(NULL)
summaryRprof()

#time
system.time()

#debugging

#logo apos erro - diz qual o nivel do erro
traceback()

#liga o debug para a funcao lm
debug(lm)

#para o R no momento do erro
options(error = recover)


##################################################################################
#Funcao para avaliar o uso de memoria de cada objeto
##################################################################################

.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(print(object.size(x), units = "auto")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

# shorthand
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

##################################################################################
#comandos para copiar e colar no Windows/Office
##################################################################################

#read.table("clipboard", sep="\t",header = TRUE)
#read.table("clipboard", sep="\t",header = TRUE,stringsAsFactors = FALSE)
#write.table(data.frame, "clipboard", sep="\t", row.names=FALSE)
#readClipboard()
#writeClipboard(array)

##################################################################################
#comandos apply
##################################################################################

#apply(dataframe,1-linha ou 2-coluna,funcao) -aplica a funcao em cada lin/col 
#tapply(var.numerica,var.categorica,funcao) - aplica a funcao pra cada categoria
#lapply(lista,funcao) - aplica a funcao em cada elemento da lista
#sapply(lista.de.valores,funcao)  - chama a funcao usando cada valor como paramentro
#mapply(funcao,lista1,lista2,...) - chama a funcao usando o i-esimo elemento de cada lista

#em todos os casos, se a funcao tiver mais de um parametro, ele pode ser passado 
#usando o nome do parametro. Exemplo:
#sapply(1:5,rnorm,mean=10,sd=10)
#mapply(rnorm,1:4,4:1,1:4)

##################################################################################
########### Slidify
##################################################################################

Install	require(devtools)
install_github("slidify", "ramnathv")
install_github("slidifyLibraries", "ramnathv")

library(slidify)
author("teste")

#2	Edit	Write in RMarkdown, separating slides with a blank line followed by three dashes ---.
#3	Slidify	slidify("index.Rmd")
#4	Publish	publish(user = "USER", repo = "REPO")

##################################################################################
#fazer tabelas ponderadas com o xtabs
##################################################################################

#http://forums.psy.ed.ac.uk/R/P01582/essential-12/

#xtabs(peso~var+var2) - calcula a soma da var peso cruzando var1 e var2
#addmargins(xtabs(peso~var+var2),2) - adiciona o total coluna a tabela
#prop.table(addmargins(xtabs(peso~var1+var2),2),2) - calcula o percentual coluna
#round(prop.table(addmargins(xtabs(peso~var1+var2),2),2)*100) - arredonda tabela
#ftable(peso~var+var2+var3) - formata melhor a tabela com 3 dimensoes 

##################################################################################
#Comandos intressantes pra usar no R - GR?FICOS
##################################################################################

#par(new = T)   - Para desenhar um gr?fico sobre o outro
# nova janela gr?fica - windows()
#par(mfrow = c(lin,col))  - N?mero de gr?ficos na janela
# layout(c(1,2,3,4)) - Disposi??o dos gr?ficos - para ver -> layout.show()
#symbols(banco$p_pop, banco$p_est, circles = banco$erro_obs_abs)   -  bubble plot

##################################################################################
#Comandos intressantes pra usar no R - MANIPULA??O DE BANCOS DE DADOS
##################################################################################

# require(car) - fun??o recode() para recodificar vari?veis   -  g <- recode(var, "c(c1,c2) = r1; c3 =r2")
# agregar dados -   agregate(dados/var, list(var1,var2), mean) 
# juntar data.frames - merge(banco.x, banco.y,by.x = "Var.x", by.y = "Var.y")
# ordenar banco segundo var.x - banco[order(banco$var.x, descending = F),]
# observa??es duplicadas - duplicated(banco)
# observa??es completas - complete.cases(banco)
# m?dia ponderada - weighted.mean(x,p)
# m?dia podnerada com o tapply -  tapply(seq(1,tam,1),grupo, function(x)weighted.mean(var[x],peso[x], na.action = na.exclude))
# tabelas de m?dias de 2 dimens?es - tapply(var.media, list(var_cat1,var_cat2), mean)
# fun??o para fazer tabelas de propor??es - prop.table(var, dimens?o)
# fun??o para fazer tabelas de frequencias - table(var1, var2)
# fun??o para melhorar a visualiza??o de tabelas com 3 dim - ftable(table(var1, var2, var3))
# fun??o para subtrair m?dias de todas as colunas de cada elemento - scale(tabela)

##################################################################################
#Comandos intressantes pra usar no R - MODELOS LINEARES
##################################################################################

# model.tables(modelo, "means", se = TRUE) - calcula todas as m?dias dos fatores e suas intera??es
# replications(formula, banco) - mostra o n?mero de obs em cada fator definido pela formula
# TukeyHSD(modelo, "nome.fator") - compara??es m?ltiplas de Tukey
# Criar um df com todas as combina??es - expand.grid(var1 = c("A", "B", "C"), var2 = c("trat1","trat2","trat3", "trat4"))
# replica matriz X do modelo - model.matrix(~a+b, banco, contrasts = list(a="contr.helmert", b="contr.treatment"))

##################################################################################
##################################################################################
# An?lise de Correspond?ncia - EXEMPLO DE COMO FAZER
##################################################################################
##################################################################################

library(ca)
mytable <- with(mydata, table(A,B)) # create a 2 way table
prop.table(mytable, 1) # row percentages
fit <- ca(mytable)
print(fit) # basic results
summary(fit) # extended results
plot(fit) # symmetric map
plot(fit, mass = TRUE, contrib = "absolute", map ="rowgreen", arrows = c(FALSE, TRUE)) # asymmetric map

#op??o customizada
symbols(fit$rowcoord[,1], fit$rowcoord[,2], circles = fit$rowmass, inches = 0.2, fg ="green", bg = "dark green")
symbols(fit$colcoord[,1], fit$colcoord[,2], circles = fit$colmass, inches = 0.2, fg ="blue", bg = "dark blue", add = T)
text(fit$colcoord[,1], fit$colcoord[,2], fit$colnames, col ="blue", adj = c(-0.5,-0.5), font = 2, ps = 4)
text(fit$rowcoord[,1], fit$rowcoord[,2], fit$rownames, col ="green", adj = c(-0.5,-0.5), font = 2, ps = 4)

##################################################################################
#para gerar fotos no R
##################################################################################

# library(ReadImages)
# resultado = read.jpeg('C:/NEALE/Personal/monster truck.jpg')
# plot(resultado)

##################################################################################
#Series temporais no R
##################################################################################

#plot.ts(serie) #plottar a serie
#ts(serie, frequency = 12, start = c(ano, mes))  #cria a serie temporal - mudar frequency pra colocar ano, quarto, etc
#acf(serie1, lags, main="Titulo") #funcao de auto-correlacao
#pacf(serie1, lags, main="Titulo") # funcao de auto-correlacao parcial
#lag.plot1(serie1, lags)  # grafico da serie1 vs seus lags - funcao do livro
#lag.plot2(serie1, serie2, lags) # grafico da serie1 vs lags daa serie2 - funcao do livro
#arima.sim(list(order=c(1,0,0), ar=.9), n=100) #Simular ARIMA
#ARMAtoMA(ar=.9, ma=.5, 50) #converter ARMA em um MA - sempre e possivel
#serie_diff = diff(serie, lag = 1, differences = 1) #tira diferenca da serie
#sarima(serie, 1, 0, 0) # estimar parametros da serie - funcao do livro
#arima(serie, order = c(0, 0, 0), seasonal = list(order = c(0, 0, 0), period = NA)) # estimar parametros da serie
#arima(serie1, order = c(1, 0, 0), seasonal = list(order = c(0, 0, 0), period = 12, xreg = c(serie2,serie3)))  # com variaveis regressoras
#predict(arima(serie1, order = c(0,0,0)), n.ahead = 16) #previsao com a serie
#sarima.for(serie,tempo,ar,s,ma,AR,S,MA,periodo) #previsao com a serie  - funcao do livro

##################################################################################
##################################################################################
#Mapa animado - N?O TERMINEI - imitando mapa animado do Google Spreadsheet
##################################################################################
##################################################################################

require(audio)

animacao <- function(x,y,veloc = 0.3, aux = 0.01, tam = 0.05, trace = F){

#inicializando
  temp <- length(x);
  tam_vec <- as.matrix(rep(1, temp))
  symbols(x, y, circles = tam_vec,inches = tam,  fg ="white", bg = "white")
 
  symbols(x[1], y[1], circles = tam,inches = tam, fg ="green", bg = "dark green", add = T)
  wait(veloc)
  if (trace == F) {symbols(x[1], y[1], circles = tam,inches = tam, fg ="white", bg = "white", add = T)}
  for (k in 2: temp){
  symbols(x[k], y[k], circles = 0.05,inches = tam, fg ="green", bg = "dark green", add = T)
  wait(veloc)
  if (trace == F) {symbols(x[k], y[k], circles = 0.05,inches = tam, fg ="white", bg = "white", add = T)}
  }
}


##################################################################################
##################################################################################
# Essa fun??o abra um janela do explorer para escolher arquivos a serem importados do Spss ou do DBF
##################################################################################
##################################################################################

require(tcltk);
require(foreign);
require(RODBC);
require(xlsx);

getfile <- function()  {
  
  
  aux_diret <- getwd();
  name <- tclvalue(tkgetOpenFile(filetypes="{{SPSS Files} {.sav}} {{DBF Files} {.dbf}} {{Excel Files} {.xls}} {{Excel 2007-2010 Files} {.xlsx}}"))
  
  if (name=="") return;
  
  pos <- max(as.numeric(gregexpr("/",name)[[1]])) - 1;
  diret <- substr(name,1,pos);

  arq <- substr(name,pos+2,nchar(name));
  pos2 <- max(lapply(gregexpr("\\.",arq),as.numeric)[[1]]) + 1;
  tipo <- substr(arq,pos2,nchar(arq));
  
  #    tkmessageBox(message = tipo);
  setwd(diret);
  if (tipo == "dbf") {
    banco <- read.dbf(arq);
  } else if (tipo == "sav") {
    banco <- read.spss(arq,use.value.label=T,to.data.frame=T);
  } else if (tipo == "xls") {
    channel <- odbcConnectExcel(arq);
    nome <- sqlTables(channel)$TABLE_NAME;
    nome <- gsub("\\$","", nome);
    nome <- gsub("'","", nome);
    
    if (length(nome) > 1) {
      print(nome);
      nome <- readline("Qual das planilhas acima deseja importar: ");
    }
    
    banco <- sqlFetch(channel,nome);
    odbcClose(channel)
  } else if (tipo == "xlsx") {
  	 wb <- loadWorkbook(arq);
  	nome <- names(getSheets(wb));
  	if (length(nome) > 1) {
      print(nome);
      nome <- readline("Qual das planilhas acima deseja importar: ");
    }
  	banco <- read.xlsx(arq, nome, stringsAsFactors = FALSE)
  	
  } 
  
  setwd(aux_diret);
  str(banco);
  banco
}

#file.choose()
#file.path(pathin, filename)


#comandos para exportar o banco de dados:

#To an text file 
write.table(mydata, "c:/mydata.txt", sep="\t") 

#To an Excel Spreadsheet 
library(xlsReadWrite)
write.xls(mydata, "c:/mydata.xls") 

# write out text datafile and an SPSS program to read it
library(foreign)
write.foreign(mydata, "c:/mydata.txt", "c:/mydata.sps",   package="SPSS") 

# write out text datafile and an SAS program to read it
library(foreign)
write.foreign(mydata, "c:/mydata.txt", "c:/mydata.sas",   package="SAS") 

# export data frame to Stata binary format 
library(foreign)
write.dta(mydata, "c:/mydata.dta") 


##################################################################################
##################################################################################
# Essa fun??o coloca intervalos de confian?a nos pontos especificados em qualquer gr?fico ativo
##################################################################################
##################################################################################



intervalos <- function(x,y, dp, conf = 0.95, cor = 'blue', linha = 1.5, tam = 0.5, pts = 1) {

if ((length(x) != length(y)) | (length(x) != length(dp)) | (is.numeric(x) == F) | (is.numeric(y) == F) | (is.numeric(dp) == F)){
  tkmessageBox(message = "Os par?metros de entrada n?o foram definidos corretamente!");
  return;
}

n <- length(x);
z_c <- qnorm(1 - ((1-conf)/2), mean = 0, sd = 1);

points(x,y, col = cor, pch = pts);
for (k in 1:n){
  lines(c(x[k],x[k]),c(y[k]- z_c * dp[k],y[k] + z_c * dp[k]),lwd = linha, col = cor)
  lines(c(x[k]-tam,x[k]+tam),c(y[k]- z_c * dp[k],y[k]- z_c * dp[k]),lwd = linha, col = cor)
  lines(c(x[k]-tam,x[k]+tam),c(y[k]+ z_c * dp[k],y[k]+ z_c * dp[k]),lwd = linha, col = cor)
  }
  
}


##################################################################################
##################################################################################
# Essa fun??o testa todos os fatores, e tamb?m faz as compara??es multiplas
##################################################################################
##################################################################################
# Nessa fun??o, os argumentos medias e fatores s?o os nomes das vari?veis, e n?o as vari?veis em si.
# comp.medias(c("var_y1","var_y2"),c("var_fat1","var_fat2"),banco, filtro = 1, peso = NULL) -- comp.medias(c("y1","raiz_pq","PRE_1"),c("cargo_","categorias"),banco)
# para ver a defini??o dos diferentes tipos, ver help(contrMat)

require(multcomp);
comp.medias <- function(medias, fatores, dados, filtro = 1, tip = 2, peso = NULL) {

tipos <- c("Dunnett", "Tukey", "Sequen", "AVE", "Changepoint", "Williams", "Marcus", "McDermott", "UmbrellaWilliams", "GrandMean");
var_tipo <- tipos[tip];

fat <- length(fatores);
n <- length(medias);
str_tukey <- paste(fatores, " = ",shQuote(var_tipo), collapse = ", ")

  for (k in 1:n){

    # calcula o modelo lm para cada vari?vel dependente
    form <- as.formula(paste(medias[k], " ~ ", paste(fatores, collapse = " + ")));
    mod <- lm(form, data = dados, na.action = na.exclude, weights = peso);
    eval(parse(text = paste("testes <- glht(mod, linfct = mcp(",str_tukey,"), alternative = c(",shQuote("two.sided"),"), level = 0.95, calpha = adjusted_calpha(), na.action = na.exclude, weights = peso)", collapse = "")));
    pvalor <- summary(testes);

    # formata o dataframe para os resultados da anova
    if (k == 1) {    
      ggg <- cbind(as.data.frame(rownames(summary.aov(mod)[[1]])[1:fat]),as.data.frame(summary.aov(mod)[[1]][1:fat,5]))
      names(ggg) <- c("Fator","p_valor")
      ggg$Var_Dep <- matrix(medias[k],fat,1);
      dados_anova <- ggg[,c(3,1,2)];
      } else {
      ggg <- cbind(as.data.frame(rownames(summary.aov(mod)[[1]])[1:fat]),as.data.frame(summary.aov(mod)[[1]][1:fat,5]))
      names(ggg) <- c("Fator","p_valor")
      ggg$Var_Dep <- matrix(medias[k],fat,1);
      ggg <- ggg[,c(3,1,2)];
      dados_anova <- rbind(dados_anova,ggg);                     
      }
      
      #LIMPAR BANCO DE DADOS - TIRANDO NA

     #criando o vetor com as m?dias de cada categoria
    for (i in 1:fat){
      eval(parse(text = paste("med_",i," <- aggregate(dados$", medias[k], ", list(dados$",fatores[i],"), mean)", sep = "")))
      if ( i == 1) { med <- med_1}
      else {eval(parse(text = paste("med <- rbind(med,med_",i,")", sep = "")))}
    }

    # formata o dataframe para os resultados das compara??es m?ltiplas
    nada <- as.data.frame(cbind(as.matrix(pvalor$test$coefficients),as.matrix(pvalor$test$pvalues)));
    tit <- rownames(nada);
    names(nada)[1] = "Dif. (I-J)";
    names(nada)[2] = "p_valor";
    dados1 <- as.data.frame(as.matrix(substr(tit,1, regexpr(":",tit)-1)));
    names(dados1)[1] = "Fator";
    dados2 <- as.data.frame(as.matrix(substr(tit, regexpr(":",tit)+2,regexpr("-",tit)-2)));
    names(dados2)[1] = "Var_I";
    dados3 <- as.data.frame(as.matrix(substr(tit, regexpr("-",tit)+2,nchar(tit))));
    names(dados3)[1] = "Var_J";
    dados_ <- cbind(dados1,dados2,dados3,nada);
    merge1 <- merge(dados_, med, by.x = "Var_I", by.y = "Group.1", sort = F)
    merge2 <- merge(merge1, med, by.x = "Var_J", by.y = "Group.1", sort = F)
    merge2$dif <- merge2$x.x - merge2$x.y;
    v <- c(3,2,1,6,7,8,5);
    dados_ <- format(merge2[,v], digits = 4, scientific = F);
    names(dados_) <- c("Fator","Var_I","Var_J","Med_I","Med_J","Dif. (I-J)","p_valor");
    dados_ <- subset(dados_, dados_$p_valor <= filtro);
    eval(parse(text = paste("dados_",medias[k]," <<- dados_",sep="", collapse = "")))
  }
  dados_anova <<- format(dados_anova, digits = 4, scientific = F); 

}

##################################################################################
####graficos ano novo
##################################################################################

par(mfrow = c(3,2))
ttt = 1.5
ajust.h = 0.35
ajust.v = 1.1
 for (i in 1:6) { 
     symbols(1, 1, circles = 1, fg ="gray", bg = "white", xlab ="", ylab = "", xaxt = "n", yaxt = "n")
     text(1, 1, teste[i+1,2],srt = 0, col ="black", cex = ttt*1.1, font = 4)
     text(1 - ajust.h * 0.15,1 + ajust.v * 0.15, teste[i+1,3],srt = 45, col ="black", cex = ttt, font = 10)
     text(1 + ajust.h * 0.15,1 - ajust.v * 0.15, teste[i+1,4],srt = 45, col ="black", cex = ttt, font = 10)
     text(1 - ajust.h * 0.15, 1 - ajust.v * 0.15, teste[i+1,5],srt = -45, col ="black", cex = ttt, font = 10)
 #   text(1 + ajust.h * 0.15, 1 + ajust.v * 0.15, teste[i+1,6],srt = -45, col ="black", cex = ttt, font = 10)
     }
     
##################################################################################
##################################################################################
# Fun??o boba - AMOR
##################################################################################
##################################################################################


core=function(nome="Meu amor")
{
  x=seq(-2,2,0.001)
  y1=sqrt(1-((abs(x)-1)^2))
  y2=-3*sqrt(1-(sqrt(abs(x))/sqrt(2)))
  plot(main="Enfim, achei uma f?rmula pra explicar", ylab="www.profmsouza.blogspot.com",xlab="o que sinto por voc?...",sub=nome,c(x,x),c(y1,y2),lwd=3,pch=20,cex=6)
  grid()
  cores=paste(c("red","tomato"),sort(rep(1:4,2)),sep="")
  eq=c("( AM + BC ) * X = AM ( X + BOC ) - BCTE","AMX + BCX = AMX + AMBOC - BCTE","BCX = AMX - AMX + BC (AMO - TE)","BCX = BC ( AMO - TE )","X = ???")
  pos=c(-0.3,-0.6,-0.9,-1.2,-1.5)
  cont=0
  for (i in 1:30)
  {
    points(c(x,x),c(y1,y2),col=cores[sample(1:8,1)],lwd=sample(1:4,1),pch=20,cex=round(runif(1,1,4)))
    if (i%%5 == 0 & i <=25)
    {
      cont=cont+1
      text(0,pos[cont],eq[cont])
    }
  }
  text(1.2,-2.5,"AMO-TE",col="red",cex=3)
}

##################################################################################
##################################################################################
# Gr?fica para visualizar o ajuste do modelo de Regress?o Log?stica
##################################################################################
##################################################################################

#glm(formula = PROP_CORRETA ~ p_pop, family = binomial(link = "logit"))

# graf_log(PROP_CORRETA,p_pop,betas)
graf_log <- function(y,x,beta,color = "blue", resol = 50, fat = 1.7) {

intercp <- names(beta)[1] == "(Intercept)"
if (intercp == TRUE) { print("Modelo com Intercepto")} else {print("Modelo sem Intercepto")}
categ <-length(beta)
fator <- class(x) == "factor"
if (fator == TRUE) { print("Modelo com vari?vel categ?rica")} else {print("Modelo com vari?vel cont?nua")}

if (fator == FALSE) { 
  plot(x, jitter(y, factor = 0.2), pch = 20,cex = 0.7, ylab = paste("Prob. de Sucesso - ", substitute(y), sep = ""), xlab = substitute(x))
  x <- seq(min(x[is.na(x) == F]),max(x[is.na(x) == F]),(max(x[is.na(x) == F]) - min(x[is.na(x) == F]))/resol)
  if (intercp == TRUE) {  
    lines(x,(exp(beta[1] + beta[2]*x)) / (1 + exp(beta[1] + beta[2]*x)), lwd = 3, col = color)
    } else {
    lines(x,(exp(beta[1]*x)) / (1 + exp(beta[1]*x)), lwd = 3, col = "blue")    
    }
} else {
  plot(jitter(as.numeric(x),factor = fat), jitter(y, factor = 0.2), pch = 20,cex = 0.7, ylab = paste("Prob. de Sucesso - ", substitute(y), sep = ""), xlab = substitute(x))
  prob_ <- beta
  if (intercp == TRUE) {  
      prob_[1] <- exp(beta[1]) / (1 + exp(beta[1]))
     for (i in 2:categ) {
        prob_[i] <- exp(beta[1] + beta[i]) / (1 + exp(beta[1] + beta[i]))
     }
  } else {
     for (i in 1:categ) {
        prob_[i] <- exp(beta[i]) / (1 + exp(beta[i]))
     }  
  }
  lines(1:categ,prob_, lwd = 3, col = color)
  }
}


##################################################################################
##################################################################################
# graphs Ipsos
##################################################################################
##################################################################################

banco <- getfile()
model <- lm(Favorability ~ Familiarity + I(Familiarity^2), data = banco)
betas <- coefficients(model)
attach(banco)

plot(Familiarity, Favorability, main="Familiarity and Favorability Graph of Associations", xlab="Familiarity ", ylab="Favorability", pch=24, col = 'blue', bg = 'blue')
#text(banco$Familiarity, banco$Favorability, labels = banco$assoc, cex = 0.7, pos=2,adj = c(0.5,0.5))
text(jitter(banco$Familiarity,2), jitter(banco$Favorability,2), labels = banco$assoc, cex = 0.7, pos=2,adj = c(0.5,0.5))
x <- seq(min(banco$Familiarity),max(banco$Familiarity),0.2)
y <- betas[1]+betas[2]*x + betas[3]*(x^2)
n <- length(x)
for (k in 1:(n-1)){
    lines(c(x[k],x[k+1]),c(y[k],y[k+1]),lwd = 2, col = 'red')
}

fav_m <- mean(banco$Favorability)
fam_m <- mean(banco$Familiarity)
#lines(c(fam_m,fam_m),c(min(banco$Favorability), max(banco$Favorability)),lwd=1)
lines(c(min(banco$Familiarity), max(banco$Familiarity)),c(fav_m,fav_m),lwd=1)
text(max(banco$Familiarity)-0.2, (fav_m + 0.075), "More likely to",cex = 0.7)
text(max(banco$Familiarity)-0.2, (fav_m + 0.05), "be favorable",cex = 0.7)
text(max(banco$Familiarity)-0.2, (fav_m + 0.025), "than unfavorable",cex = 0.7)
text(max(banco$Familiarity)-0.2, (fav_m - 0.025), "More likely to",cex = 0.7)
text(max(banco$Familiarity)-0.2, (fav_m - 0.05), "be unfavorable",cex = 0.7)
text(max(banco$Familiarity)-0.2, (fav_m - 0.075), "than favorable",cex = 0.7)

### graf nigeria

plot(Muslim,CPC, main="Religion versus Voting for CPC", xlab="Muslim Religion ", ylab="Voting CPC", pch=24, col = 'blue', bg = 'blue')
text(Muslim,CPC, labels = state, cex = 0.7, pos=2,adj = c(0.5,0.5))
model <- lm(CPC ~ Muslim + I(Muslim^2), data = nigeria)
betas <- coefficients(model)
x <- seq(0,1,0.2)
n <- length(x)
y <- betas[1]+betas[2]*x + betas[3]*(x^2)
n <- length(x)
for (k in 1:(n-1)){
    lines(c(x[k],x[k+1]),c(y[k],y[k+1]),lwd = 2, col = 'red')
}

##################################################################################
## FAvorability of Nations
##################################################################################

plot(Other.Countries, US, main="Average difference between Social Media Users and Population", xlab="Favorability towards Other Countries", ylab="Favorability towards US", pch=24, col = 'blue', bg = 'blue')
text(jitter(Other.Countries,2), jitter(US,2),  labels = Countries, cex = 0.7, pos=2,adj = c(0.5,0.5))
lines(c(0,0),c(min(US),max(US)),lwd=1)
lines(c(min(Other.Countries),max(Other.Countries)),c(0,0),lwd=1) 

#paint labels in accordance to BRIC, emerging countries and others. 
#fix axis so labels fit in display!

##################################################################################
### Time series
##################################################################################


#banco$r_date <- as.Date(banco$r_date,format="%Y%m%d")
#sub <- banco[,c(4,5,6,31,8,11,12,13,14)]


load("C:\\NEALE\\Downloads\\R Program\\Time Series - Book\\tsa3.rda")
dados <- getfile();
attach(dados)
windows()
serie1 <- ts(log_approval,frequency = 12, start = c(1948,1))
plot.ts(serie1);
serie2 <- ts(de_trended_CPI,frequency = 12, start = c(1948,1))
plot.ts(serie2);
par(mfrow=c(2,1))
acf(serie1, 40, main="ACF")
pacf(serie1, 40, main="PACF")
windows()
lag.plot1(serie1, 12)
windows()
lag.plot2(serie1, serie2, 12)

ARMAtoMA(c(1.0, 0.63), 1.0, 10)  # transforming ARMA in MA
sarima(serie1,1,0,0,0,0,0)   # EStimating parameters
sarima.for(serie1, 6, 2, 0, 0)   #Forecasting using sarima model

serie3 <- ts(dados$CPI_lag1,frequency = 12, start = c(1948,1))
serie_reg <- arima(serie1, order = c(1, 0, 0), seasonal = list(order = c(0, 0, 0), period = 12, xreg = c(serie2,serie3))) # estimar parametros da serie
predict(serie_reg, n.ahead = 4, newxreg = c(serie2_,serie3_),se.fit = TRUE)


calcular serie2_ e serie3_  com mais 4 casos - tentar predict de 5 meses na frente.

estimar series colocando param AR1 - AR4 igual a zero




##################################################################################
### Plotting Maps
##################################################################################

library(maptools);
library(RColorBrewer);

russia.shp <- readShapePoly(file.choose());
summary(russia.shp);

plot(russia.shp)
plot(russia.shp, xlim=c(0, 50), ylim=c(-23,23))

##################################################################################
### Designing Conjoint and Choice Models
##################################################################################

library(AlgDesign);

k <- choose(6,4)      # numero combinacoes entre os produtos existentes (6) e o numero de produtos mostrados ao respondente (4)
dat <- gen.factorial(c(6,6,6,6,k), center = FALSE)     #gera o desenho
desF<-optFederov(~.^2,dat,nTrials=1500)         # escolhe as melhores 1500 combinacoes de forma a minimizar a variancia para o modelo .^2
desFBlk<-optBlock(~.^2,desF$design,rep(10,150))       #ordena os experimentos de forma a tentar minimizar o efeito entre blocos - 150 blocos (pessoas) de 10 experimentos

dados <- as.data.frame(desF$design)
sapply(dados,table)
dados.blocos <- as.data.frame(desFBlk[3])
sapply(dados.blocos,table)
lista <- as.data.frame(sapply(dados,table)[5])

# depois tem que gerar a legenda para as choose(6,4) combinacoes geradas

#generating all permutations

library(gregmisc)
permutations(n=4,r=4)


##################################################################################
### Raking
##################################################################################


require(survey);
base_ <- getfile();
fda <<- svydesign(id=~RESPID,data = base_);

pop.sex <<- xtabs(pop ~ sex, sex)
pop.age <<- xtabs(pop ~ age, age)
pop.race <<- xtabs(pop ~ race_, race)
pop.inc <<- xtabs(pop ~ income, income)
pop.party <<- xtabs(pop ~ party_id, party_id)

 
saida <- rake(fda, sample=list(~sex, ~age, ~race_, ~income, ~party_id), population=list(pop.sex, pop.age, pop.race, pop.inc, pop.party), control = list(maxit = 400));
pesos <- weights(saida)
pesos <- 1007 * (pesos / sum(pesos))

### para utilizar raking porem permiting que estratos populacionais nao
### existam na amostra, basta copiar a funcao rake, e incluir na chamada
### da funcao "postStratify" o parametro ",partial = TRUE"


##################################################################################
### Wgts Calibrating
##################################################################################

require(survey);                           
base_ <- getfile();
plano <<- svydesign(id=~RESPID,data = base_);

pop.total <- colSums(model.matrix(~sex+edu, base_))
pop.total[1] = 3002
pop.total[2] = 1000
pop.total[3] = 2000
pop.total[4] = 2
pop.total[5] = 1000
pop.total[6] = 1000

# o total da primeira categoria e definido por subtracao - cosnt - categ1 - categ2 
calib <- calibrate(plano, ~sex, population = pop.total) 
peso <- weights(calib)

# calibracao com restricao nos pesos - pesos entre 0 e 9
calib <- calibrate(plano, ~sex+edu, population = pop.total,bounds=c(0,9))

# force resultado mesmo quando os bounds nao sao atingidos
calib <- calibrate(plano, ~sex+edu, population = pop.total,bounds=c(0.0001,5), force = TRUE)

##################################################################################
### Grafico Political Models
##################################################################################

attach(banco)
mod <- glm(formula = government_dep ~ as.factor(incumbent) + as.factor(type_elections) + approval, family = binomial(link = "logit"))

coef_inc_pre <- coef(mod)[c(1,4)]
coef_inc_pre[1] <- coef(mod)[1]+coef(mod)[2]         #porque muda somente o intercepto
coef_inc_parl <- coef(mod)[c(1,4)]
coef_inc_parl[1] <- coef(mod)[1]+coef(mod)[2]+coef(mod)[3]         #porque muda somente o intercepto

graf_log(government_dep,approval,coef_inc_parl,color = "dark green", resol = 50, fat = 1.7)
par(new = T)
graf_log(government_dep,approval,coef(mod)[c(1,4)],color = "red", resol = 50, fat = 1.7)
par(new = T)
graf_log(government_dep,approval,coef_inc_pre,color = "blue", resol = 50, fat = 1.7)

legend("bottomright", legend = c("Presidential Incumbent", "Parlamentary Incumbent", "Sucessor"), col = c("blue","dark green", "red"), pch = 7)

##################################################################################
### Sampling Simulation
##################################################################################

coef_cor <- function(corr) {
 
  require(polynom)
  if (corr == -1) {
    b = -1
  } else if (corr == 1) {
    b = 1
  } else if (corr == 0) {
    b = 0
  } else  if (corr > 0) {
    raiz <- solve(polynomial(c(1,-2,(2-1/(corr^2)))))
    n <- length(raiz)
     for (i in 1:n) {
       if ((raiz[i] > 0) & (raiz[i] < 1)) {b <- raiz[i]};
     }
  } else  if (corr < 0) {
    raiz <- solve(polynomial(c(1,2,(2-1/(corr^2)))))
    n <- length(raiz)
    for (i in 1:n) {
      if ((raiz[i] < 0) & (raiz[i] > -1)) {b <- raiz[i]}; 
    }
  }
  b
  }

univ <- function(N = 1000, n = 20, resol = 32, dist = 'rnorm', param = '200,20') {
  require(sampling)
    
  x <- eval(parse(text = paste(dist,"(N,",param,")",sep="")))
  z <- eval(parse(text = paste(dist,"(N,",param,")",sep="")))
  #x <- as.matrix(rnorm(N,200,20))
  #z <- as.matrix(rnorm(N,400,20))
  prob <- inclusionprobabilities(x,n)
  
  corr <- seq(-1,1,2/resol) 
  aux <- length(corr)
  base <- matrix(0,N,aux)
  for (i in 1:aux) {
    b <- coef_cor(corr[i])
      if (corr[i] >= 0) {
        base[,i] <- x[order((1-b)*z + b * x)]
      } else if (corr[i] < 0) {
        base[,i] <- x[order((-1-b)*z + b * x)]
      }
  }
 
  attr(base,"prob") = prob[order(prob)]
  attr(base,"dist") = paste(dist,"(N,",param,")",sep="")
  base
  
}

simulation <- function(data, iter = 1000, sample = 'UPpoisson') {
  
  require(sampling)
  prob <- attr(data,"prob")
  n <- sum(prob)
  N <- dim(data)[1]
  resol <- dim(data)[2]
  data <- as.data.frame(data)
  
  sample_SRS <- matrix(0,iter,resol)
  sample_HT <- matrix(0,iter,resol)
  sample_Hajek <- matrix(0,iter,resol)
    
  indexes <- eval(parse(text = paste("as.data.frame(replicate(iter,",sample,"(prob)))",sep="")))
  for (i in 1:iter){
      
    dados_aux <- data[indexes[,i]==1,]
    prob_aux <- prob[indexes[,i] == 1] 
    sample_SRS[i,] <-apply(dados_aux,2,mean)
    sample_HT[i,] <- apply(dados_aux,2,HTestimator,prob_aux)/N
    sample_Hajek[i,] <-apply(dados_aux,2,Hajekestimator,prob_aux,type="mean")
      
  }
  
  corr <- as.data.frame(cor(prob,data))
  SRS.b <- abs(colMeans(sample_SRS) - colMeans(data))
  HT.b <- abs(colMeans(sample_HT) - colMeans(data))
  HAJEK.b <- abs(colMeans(sample_Hajek) - colMeans(data))
  
  SRS.v <- apply(sample_SRS,2,var)
  HT.v <- apply(sample_HT,2,var)
  HAJEK.v <- apply(sample_Hajek,2,var)
  
  SRS.eqm <- (SRS.b)^2 + SRS.v
  HT.eqm <- (HT.b)^2 + HT.v
  HAJEK.eqm <- (HAJEK.b)^2 + HAJEK.v
  
  resul <- as.data.frame(t(rbind(corr,SRS.b,HT.b,HAJEK.b,SRS.v,HT.v,HAJEK.v,SRS.eqm,HT.eqm,HAJEK.eqm)))
  
  names(resul) <- c("corr","SRS.b","HT.b","HAJEK.b","SRS.v","HT.v","HAJEK.v","SRS.eqm","HT.eqm","HAJEK.eqm")
  
  dist <- attr(data,"dist")
  
  par_aux = max(resul$SRS.eqm, resul$HT.eqm, resul$HAJEK.eqm);
  plot(resul$SRS.eqm ~ resul$corr, main = paste(" ", dist," - Sample n = ", n, " from pop size N =",N, sep = ""), ylab = "MSE", xlab = "Correlation", type = "l", lwd = 3, col = "red", ylim=c(0,par_aux))
  lines(resul$HAJEK.eqm ~ resul$corr, col = "blue", lwd=3)
  lines(resul$HT.eqm ~ resul$corr, col = "green", lwd=3)
  legend("topright",c("Average", "Hajek", "HT"), col = c("red","blue","green"), lwd = 3)

#   par_aux = max(resul$SRS.b, resul$HT.b, resul$HAJEK.b);  
#   plot(resul$SRS.b ~ resul$corr, main = paste("Sample n = ", n, " and pop size N =",N, sep = ""), ylab = "Bias", xlab = "Correlation", type = "l", lwd = 3, col = "red", ylim=c(0,par_aux))
#   lines(resul$HAJEK.b ~ resul$corr, col = "blue", lwd=3)
#   lines(resul$HT.b ~ resul$corr, col = "green", lwd=3)
#   legend("topright",c("Average", "Hajek", "HT"), col = c("red","blue","green"), lwd = 3)
#   
  resul
  
  }

#########################################
########## WORDLE IN R   ################
#########################################

library(RXKCD)
library(tm)
library(wordcloud)
library(RColorBrewer)
path <- system.file("xkcd", package = "RXKCD")
datafiles <- list.files(path)
xkcd.df <- read.csv(file.path(path, datafiles))
xkcd.corpus <- Corpus(DataframeSource(data.frame(xkcd.df)))
xkcd.corpus <- tm_map(xkcd.corpus, removePunctuation)
xkcd.corpus <- tm_map(xkcd.corpus, tolower)
xkcd.corpus <- tm_map(xkcd.corpus, function(x) removeWords(x, stopwords("english")))
tdm <- TermDocumentMatrix(xkcd.corpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:2)]
png("wordcloud.png", width=1280,height=800)
wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
dev.off()

#########################################
########## MACRO IN R   ################
#########################################

# Define the macro (we're adding more options to the plot this time, but it's the same idea)
cg <- defmacro(varname, vartext, expr={qplot(t, varname, data=df, geom="line", main=vartext, xlab="Time", ylab="") +
  )

# Create all of the graphs we want, storing them in variables
p_mean_len = cg(df$mean_len, "Mean Length")


#########################################
########## ggplot2 - Webinar Graph
#########################################
  
  qplot(house_prop_dem, house_dem_estimate,colour = election_type, geom = c("point"),title = "House Elections", data = dados) 
+ geom_text(aes(label = election_year), size = 3,hjust = -0.5, vjust=-1, data = dados) 
+ geom_point(size = 3) +opts(title = "Congress Elections") 
+ xlab("% Democrats")
+ ylab("% Democrats - Predicted") 
+ geom_abline(intercept=0, slope=1, colour = 'blue')

qplot(sen_prop_dem, senate_dem_estimate,colour = election_type, geom = c("point"),title = "House Elections", data = dados) 
+ geom_text(aes(label = election_year), size = 3,hjust = -0.5, vjust=-1, data = dados) 
+ geom_point(size = 3) + opts(title = "Senate Elections") 
+ xlab("% Democrats")
+ ylab("% Democrats - Predicted") 
+ geom_abline(intercept=0, slope=1, colour = 'blue')

#########################################
########## ggplot2 - Article Graph
#########################################

attach(article)
minx <- c(0.5,2.55,4.55,11.55)
maxx <- c(2.45,4.45,11.45,19.45)
aux <- as.data.frame(cbind(minx,maxx))
qplot(type_final,measure3,geom = "boxplot", fill = color,ylab = "Measure of Accuracy (Measure 3)", xlab = "Method of estimation") 
+ geom_jitter(position=position_jitter(width=0.1), alpha = 0.5,size = 2) 
+geom_rect(data = aux, aes(xmin =minx,xmax=maxx),ymin=0,ymax=0.225, fill = "transparent", colour = "black")

auxx <- c(1.5,3.5,8,15.5)
auxy <- c(0.2,0.2,0.2,0.2)
names <- c("Market","LV \nModels","CPS weighted","List Weighted")
aux_n <- as.data.frame(names)
aux_n <- cbind(aux_n,auxx)
aux_n <- cbind(aux_n,auxy)

require(ggplot2) 
+ labs(fill = "Method/Type") 
+ opts(axis.text.x=theme_text(angle=90)) 
+ geom_jitter(position=position_jitter(width=0.1), alpha = 0.5,size = 2)  
+ geom_rect(data = aux, aes(xmin =minx,xmax=maxx),ymin=0,ymax=0.225, fill = "transparent", colour = "black") 
+ geom_text(aes(x=auxx, y =auxy, label = names), data = aux_n)

qplot(type_final,measure3,geom = "boxplot", fill = color,ylab = "Average Absolute Difference", xlab = "", data = article) 
+ facet_grid(. ~ color, scales = "free_x", space = "free")
+ labs(fill = "Method/Type") 
+ opts(axis.text.x=theme_text(angle=90)) 
+ geom_jitter(position=position_jitter(width=0.1), alpha = 0.5,size = 2)
+ opts(strip.text.x = theme_blank())

### cortex white paper

qplot(var,absdif,geom = "boxplot", fill = Source,ylab = "Absolute Difference", xlab = "", data = cortex_dif) 
+ facet_grid(. ~ type, scales = "free_x", space = "free")+ labs(fill = "Benchmark") 
+ opts(axis.text.x=theme_text(angle=90),axis.title.y = theme_text(hjust=0.7,angle = 90)) 
+ geom_jitter(position=position_jitter(width=0.1), alpha = 0.5,size = 2)+ylim(0,20)

### AAPOR 2012 paper

qplot(Ipsos,Gallup,geom = "point",colour = type, ylab = "Gallup", xlab = "Ipsos") 
+ geom_line(aes(group=domain)) 
+ geom_text(aes(x=Ipsos_notpushed, y =Gallup_notpushed, label = domain,size = 1), data = aapor)

#########################################
####### amostragem com o R  #############
#########################################

require(sampling)

#para gerar variavel estratificao integer (nao e' necessario)
strata <- as.data.frame(as.integer(banco$estrato))
names(strata)[1] = "strata"
banco <- cbind(banco,strata)

#para gerar as probabilidades de selecao prop. ao tamanho com estratificacao
ordem <- order(banco$strata,banco$renda_m)
banco <- banco[ordem,]
prob <- as.data.frame(unlist(mapply(inclusionprobabilities,split(banco$pop,banco$estrato),c(5,16,5,40,3,21))))
names(prob)[1] = "prob"
ID_unit <- as.data.frame(1:dim(banco)[1])
names(ID_unit)[1] = "ID_unit"
banco <- cbind(banco,prob,ID_unit)

#seleciona amostra igual eu faco no excel
sample <- strata(banco, "estrato", size = c(5,16,5,40,3,21),  method=c("systematic"), pik = banco$pop,description=FALSE)
amostra <- sample[,c("ID_unit","Stratum")]  

#adiciona ao banco original a coluna indicando a amostra selecionada
require(car)
banco <- merge(banco,amostra,by = "ID_unit", all = TRUE)
banco <-cbind(banco,as.data.frame(recode(banco$Stratum, "NA=0; else=1")))
names(banco)[dim(banco)[2]] = "amostra"

#########################################
#### Lift/ROC  Regressao Logistica  #####
#########################################

# require(ROCR)

model <- glm(government_dep ~ approval * incumbent, family=binomial, data = banco)
probs <- model$fitted.values

x <- seq(0,1,.1)
y <- x[1:length(x)]
x <- x[1:(length(x)-1)]
prob_grp <- cut(probs,breaks = seq(0,1,0.1), labels = paste(x,y,sep = " thru "))

n <- dim(banco)[1]
cutpoints <- matrix(0,n,10);
for (k in 1:10){
 cutpoints[,k] <- ifelse(as.numeric(probs) >= 0.1*(10-k),1,0)
}
cutpoints <- as.data.frame(cutpoints)
names(cutpoints) <- paste("Top ",seq(10,100,10)," %", sep = "")
banco <- cbind(banco,cutpoints)

lift <- matrix(0,10,1);
roc <- matrix(0,10,2);
for (k in 1:10){
  if (k < 10) {
    lift[k,1] <- tapply(banco$gov_dummy,banco[,12+k],mean)[2]
    roc[k,2] <- tapply(banco[,12+k],banco$gov_dummy,mean)[1]
    roc[k,1] <- tapply(banco[,12+k],banco$gov_dummy,mean)[2]}
   else {lift[k,1] <- tapply(banco$gov_dummy,banco[,12+k],mean)[1]
    roc[k,2] <- tapply(banco[,12+k],banco$gov_dummy,mean)[1]
    roc[k,1] <- tapply(banco[,12+k],banco$gov_dummy,mean)[2]
  }
}


roc <- as.data.frame(roc)
names(roc) <- c("TP","FP")

lift <- as.data.frame(lift)
cuts <- as.data.frame(seq(10,100,10))
lift <- cbind(cuts,lift)
names(lift)[1] <- "Gov.Wins"
names(lift)[2] <- "Prob.cuts"

require(ggplot2)

qplot(lift[,1], lift[,2], geom = c("line"),title = "Lift Graph", data = banco)  + coord_cartesian(ylim=c(0, 1)) + scale_y_continuous(breaks=seq(0, 1, 0.2)) + ylab("% of Government party wins") + xlab("Top X% predicted probability") + geom_abline(intercept=mean(banco$gov_dummy), slope=0, colour = 'blue')  + geom_text(aes(x=50, y =mean(banco$gov_dummy)),hjust=0.3, vjust=1,label = "Base Rate (Expected Correct predictions with random allocation)")
qplot(roc[,2], roc[,1], geom = c("line"),title = "ROC Graph", data = roc)  + coord_cartesian(ylim=c(0, 1),xlim=c(0, 1)) + scale_y_continuous(breaks=seq(0, 1, 0.2)) + scale_x_continuous(breaks=seq(0, 1, 0.2)) + ylab("TRUE POSITIVE (POSITIVE CORRECTLY CLASSIFIED)") + xlab("FALSE POSITIVE(NEGATIVE WRONGLY CLASSIFIED)") + geom_text(aes(x=roc[,2], y =roc[,1],hjust=-0.2, vjust=0.7,label = paste("Top ",seq(10,100,10)," %", sep = "")))

#########################################
############ Ternary Plot  ##############
#########################################

# require(vcd)
ternaryplot(dados[,2:4],id = dados[,1],cex=0.5, pch = "*")


#############################################################
############ Video animado com os graficos  #################
#############################################################

install.packages("rgl")
library(rgl) 


# Plot 3d image
plot3d(V002,V005,V009)

# For loop rotating the 3d image 360 degrees
degrees <- seq(1,360, by = 1) # a sequence from 1 to 360
for(i in 1:length(degrees)){
  view3d(degrees[i], phi = 0) # pick the angle of view
  rgl.snapshot(paste(paste("F:/DADOS/teste", "-",formatC(i, digits = 3, flag = "0"), sep = ""), "png", sep = "."))
}

# rodar no prompt do DOS o diretorio com as fotos
#ffmpeg -f image2 -i teste-0%3d.png video.mpg

#############################################################
############ Automatizacao Reuters Online
#############################################################


###PollingReport.com

library(XML)

url <- "http://www.nationalpolls.com/2012/obama-vs-romney.html"
Lines <- readLines(url)
teste <- grep('[0-9]/[0-9]+-[0-9]+/[0-9]+/12',Lines, value = TRUE)
teste <- gsub(';','###',teste)
teste2 <- gsub('(<td>|</td>)',';',teste)
teste3 <- gsub('; *;',';',teste2)
teste4 <- gsub('( |")*(</tr>|<tr>)','',teste3)
teste5 <- gsub(';.{1,15}href=\"http://',';',teste4)
teste6 <- gsub('(\">|</a>)',';',teste5)
teste7 <- gsub('(  ;|^ ;)','',teste6)
teste8 <- gsub(';(;| ){1,4}',';',teste7)
teste8 <- sub('www[^;]*;[^;]*www','ADDRESS REMOVED;;www',teste8)
all <- grep(';',teste8)
### fixing those with only five ";"
good <- grep('.*;.*;.*;.*;.*;.*;',teste8)
bad <- setdiff(all,good)
aux <- length(bad)
for (k in 1:aux){
  teste8[bad[k]] <- paste("; ;",teste8[bad[k]],sep = "")
}
### fixing those with only six ";"
good <- grep('.*;.*;.*;.*;.*;.*;.*;',teste8)
bad <- setdiff(all,good)
aux <- length(bad)
for (k in 1:aux){
  teste8[bad[k]] <- paste(";",teste8[bad[k]],sep = "")
}
teste8 <- gsub('ConnecticutPPP','Connecticut',teste8)
teste8 <- gsub('New Hampshire','New-Hampshire',teste8)
teste8 <- gsub('New Jersey','New-Jersey',teste8)
teste8 <- gsub('New Mexico','New-Mexico',teste8)
teste8 <- gsub('North Carolina','North-Carolina',teste8)
teste8 <- gsub('North Dakota','North-Dakota',teste8)
test <- length(all) -length(grep('.*;.*;.*;.*;.*;.*;.*;',teste8))
print(paste('There were',test,'cases with less than 7 string characters ;'))
teste9 <- strsplit(teste8,';')
final <- as.data.frame(matrix(unlist(teste9), ncol=7, byrow = T))
names(final) <- c("url_original","pollster","url_np","area","date","Obama","romney")

###Pollster.com

install.packages("gtools")
library(gtools)


links <- c('http://elections.huffingtonpost.com/pollster/2012-arizona-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-california-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-colorado-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-connecticut-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-florida-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-georgia-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-illinois-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-iowa-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-maine-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-massachusetts-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-michigan-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-minnesota-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-missouri-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-montana-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-nevada-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-new-hampshire-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-new-jersey-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-new-mexico-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-new-york-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-north-carolina-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-north-dakota-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-ohio-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-oregon-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-pennsylvania-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-texas-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-virginia-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-washington-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-west-virginia-president-romney-vs-obama.csv','http://elections.huffingtonpost.com/pollster/2012-wisconsin-president-romney-vs-obama.csv')
names <- c('Arizona','California','Colorado','Connecticut','Florida','Georgia','Illinois','Iowa','Maine','Massachusetts','Michigan','Minnesota','Missouri','Montana','Nevada','New-Hampshire','New-Jersey','New-Mexico','New-York','North-Carolina','North-Dakota','Ohio','Oregon','Pennsylvania','Texas','Virginia','Washington','West-Virginia','Wisconsin')
aux <- length(links)

for (k in 1:aux){
  url <- links[k]
  Lines <- readLines(url)
  con <- textConnection(Lines)
  data <- read.csv(con)
  close(con)
  n <- dim(data)[1]
  state <- rep(names[k],n)
  data <- data.frame(cbind(data,state))
  if (k==1) {pollster <- data}
  else {pollster <- smartbind(pollster,data)}
  print(names[k])
  print(names(data) == names(pollster))
}  

### realclearpolitics

library(XML)
library(RCurl)

url <- "http://www.realclearpolitics.com/epolls/2012/president/az/arizona_romney_vs_obama-1757.html#polls"
sera <- getURL(url)
g <- htmlParse(sera, asText = TRUE)
b <- readHTMLTable(g)
b <- readHTMLTable(g, which= 10)


###PollingReport.com

library(XML)

url <- "http://www.nationalpolls.com/2012/obama-vs-romney.html"
sera <- getURL(url)
g <- htmlParse(sera, asText = TRUE)
b <- readHTMLTable(g)
b <- readHTMLTable(g, which= 6)
names(b)[4]<- "Obama"
names(b)[5]<- "Romney"

###Pollster.com

library(XML)
library(RCurl)

url <- "http://elections.huffingtonpost.com/2012/romney-vs-obama-electoral-map"
sera <- getURL(url)
g <- htmlParse(sera, asText = TRUE)
b <- readHTMLTable(g,which = 2,header = FALSE)
names(b) <- c("name","COLLEGE","OBAMA","ROMNEY","MARGIN","RESULTS08","RESULTS04","RESULTS00")
b$OBAMA <- as.numeric(sub('%','',levels(b$OBAMA))[b$OBAMA])
b$ROMNEY <- as.numeric(sub('%','',levels(b$ROMNEY))[b$ROMNEY])
b$name <- gsub('.\n +','',b$name)

### 538
url <- "http://fivethirtyeight.blogs.nytimes.com/2012/09/03/sept-2-split-verdict-in-polls-on-romney-convention-bounce/"
sera <- getURL(url)
g <- htmlParse(sera, asText = TRUE)
b <- readHTMLTable(g)


###############################################
############ Ler pdf como texto  ##############
###############################################

# pra rodar, tem que colocar o arquivo pdftotext.exe que vem com xpdf
# em um diretorio que o R acessa. Uma opcao e' trocar o working directory
# pra apontar para esse arquivo


require(tm)
f <- "F:/DADOS/ESTATISTICA/The Markov Chain Monte Carlo Revolution.pdf"
pdf <- readPDF(PdftotextOptions = "-layout")(elem = list(uri = f),language = "en",id = "id1")

#########################################################
############ Baixar pesquisas registradas  ##############
#########################################################

library(RCurl)
library(RHTMLForms)
library(XML)

#2002 ate 2010

"http://www.tse.jus.br/sadAdmPesqEleConsulta/pesquisa.jsp"

o = postForm("http://www.tse.jus.br/sadAdmPesqEleConsulta/procPesquisa.jsp",
             tribunal = "TSE", codEleicao="3",docsPerPage="100",
             style = "POST" )


#ou faz assim...
forms = getHTMLFormDescription("http://www.tse.jus.br/sadAdmPesqEleConsulta/pesquisa.jsp")
fun = createFunction(forms[[1]])
t = fun(tribunal = "TSE", codEleicao="1",docsPerPage="100")
g <- htmlParse(t, asText = TRUE)
b <- readHTMLTable(g)
View(b[3])

#####

h <- xpathApply(g,"//table/tr//font/a/@href")
txt <- toString(h)

n <- htmlTreeParse(t, asText = TRUE)
top = xmlRoot(g)
#j de 2 a length(names(top[[2]][[5]]))
top[[2]][[5]][[j]][[3]]

#####

imp_tse <- function(eleicao="3", pesquisas = 589)  {
  
  c_file <- "cookies.txt"
  curlHandle <- getCurlHandle(cookiefile=c_file, cookiejar=c_file)  
  aux_teto <- ceiling(pesquisas/100) - 1
  m <- 0
  
  for (j in 0:aux_teto) {
    
    
    site <- paste("http://www.tse.jus.br/sadAdmPesqEleConsulta/procPesquisaBySession.jsp?index=",j,"01",sep="")
    
    o <- postForm(site, .params=list(par="cookie"), curl=curlHandle,
                  tribunal = "TSE", codEleicao=eleicao,docsPerPage="100",
                  style="POST")
    
    g <- htmlParse(o, asText = TRUE)
    b <- readHTMLTable(g)
    
    if(j==0){final <- as.data.frame(b[3])} else {
      aux <- as.data.frame(b[3])
      final <- as.data.frame(rbind(final,aux))
    }
    
    inic <- 0
    fim <- 99
    if (j==aux_teto) {fim <- (pesquisas %% 100)-1}
    for (k in inic:fim) {
      m <- m + 1
      site2 <- paste("http://www.tse.jus.br/sadAdmPesqEleConsulta/procDetalhe.jsp?pesquisaIndex=",k,sep="")
      n <- getURL(site2, curl=curlHandle)
      g <- htmlParse(n, asText = TRUE)
      b <- readHTMLTable(g)
      
      print(m)
      
      if(k==0 & j==0){detalhes <- as.data.frame(cbind(m,as.data.frame(b[2])))} 
      else {
        aux <- as.data.frame(cbind(m,as.data.frame(b[2])))
        detalhes <- as.data.frame(rbind(detalhes,aux))
      }
    }
  }
  
  final <<- final
  detalhes <<- detalhes
  
  return(final)
  return(detalhes)
  
}

#2012
"http://pesqele.tse.jus.br/pesqele/publico/pesquisa/Pesquisa/consultaPublica.action?dataFimRegistro=&action:pesquisa/Pesquisa/consultarPesquisasPublica=Consultar&d-4021255-p=2&__multiselect_listaDeCargo=&dataInicioRegistro=&ufSelecionada=&pesquisa.numeroProtocolo=&eleicaoSelecionada=47&empresaSelecionadaPublica="

#muda -p2 para numeros entre 1 e 536 para baixar cada pagina

cont <- 0
for (k in 1:538) {
  txt1 <- "http://pesqele.tse.jus.br/pesqele/publico/pesquisa/Pesquisa/consultaPublica.action?dataFimRegistro=&action:pesquisa/Pesquisa/consultarPesquisasPublica=Consultar&d-4021255-p="
  txt2 <-  "&__multiselect_listaDeCargo=&dataInicioRegistro=&ufSelecionada=&pesquisa.numeroProtocolo=&eleicaoSelecionada=47&empresaSelecionadaPublica="
  site <- paste(txt1,k,txt2,sep='')
  
  erro <- 1
  while (erro==1) {
    erro <- 0
    n <- tryCatch(getURL(site), error=function(e) {erro <<- 1})
  }
  
  g <- htmlParse(n, asText = TRUE)
  b <- readHTMLTable(g)
  
  if (k == 1) { final <- as.data.frame(b[[2]])}
  else {aux <- as.data.frame(b[[2]])
        final <- as.data.frame(rbind(final, aux))}
  
  h <- xpathApply(g,"//a[@class='visualizar']/@href")
  links <- strsplit(toString(h),',')
  links <- gsub(' ','',links[[1]])
  aux_loop <- length(links)
  
  for (j in 1:aux_loop) {
    
    cont <- cont + 1
    site <- paste("http://pesqele.tse.jus.br",links[j],sep='')
    
    erro <- 1
    while (erro==1) {
      erro <- 0
      n <- tryCatch(getURL(site), error=function(e) {erro <<- 1})
    }
    
    g <- htmlParse(n, asText = TRUE)
    h <- xpathApply(g,"//label",xmlValue)
    vars <- as.data.frame(unlist(h))
    
    bn <- xpathApply(g,"//child::fieldset/text()")
    nnn <- lapply(lapply(bn[2:26],xmlValue),gsub,pattern='(\r|\t|\n)',replacement='')
    nnn <- as.data.frame(unlist(nnn))
    row.names(nnn) <- NULL
    
    print(paste('pag',k,'- item',j))
    
    if (cont == 1) {
      detalhe <- as.data.frame(cbind(cont,vars,nnn))}
    else {aux <- as.data.frame(cbind(cont,vars,nnn))
          detalhe <- as.data.frame(rbind(detalhe, aux))}
    
  }
  
}

names(eleicao2012det.part2) <- c("id","var","val")
require(reshape)
teste2012 <- as.data.frame(cast(eleicao2012det.part2, id ~ var))


tam <- dim(eleicao2012det.part2)[1]
comp <- 25
k<- (500 * comp) 
ini <- seq(1,tam,k)
aux_loop <- length(ini)
fim <- ini - 1
fim <- c(fim,tam)
fim <- fim[2:(aux_loop+1)]
for (j in 1:aux_loop) {
  if (j == 1) {
    final <- as.data.frame(cast(eleicao2012det.part2[ini[j]:fim[j],], id ~ var))}
  else {aux <- as.data.frame(cast(eleicao2012det.part2[ini[j]:fim[j],], id ~ var))
        final <- as.data.frame(rbind(final, aux))}
}


save(final,detalhe,file = 'pesquisas2012.RData')

###############################################
############ Running Bugs from R ##############
###############################################

#save model in file model.txt
#model {
#  for (i in 1:n){
#    y[i] ~ dnorm (y.hat[i], tau.y)
#    y.hat[i] <- inprod(b[],x[i,])
#  }
  
#  for (j in 1:k){
#    b[j] ~ dnorm (0, .0001)
#  }
  
#  tau.y <- pow(2, -2)
#}
#

require(R2WinBUGS)
setwd("C:/NEALE/Projects/Reuters 2012 Online Polling/Reuters Tracker 2012/Hierarquical Model")

bugs <- base_[as.numeric(base_$YEAR_WEEK) >= 40,]
bugs$ST <- as.numeric(bugs$STATE)

model1.par <- c ("b")
aux <- names(xtabs(~bugs$CP5_1))[1]
y <- ifelse(bugs$CP5_1 == aux,1,0)
n <- length(y)
const <- rep(1,n)
x <- cbind(const,bugs$LV_INDEX_DUMMY,bugs$REGISTERED)
k <- dim(x)[2]
data <- list("n", "y", "x", "k")

inits <- function() {
  list (sigma.y = runif(1,0,100), tau.y = rexp(1,3),
        b = rnorm(3,0,.0001))}

model1 <- bugs(data,inits,model1.par,"model1.txt", n.chains=3, n.iter=500)

#run with debug to not close winbugs window
model1 <- bugs(data,inits,model1.par,"model1.txt", n.chains=3, n.iter=500,debug=TRUE)




model1.par <- c ("b")
aux <- names(xtabs(~bugs$CP5_1))[1]
y <- ifelse(bugs$CP5_1 == aux,1,0)
ST <- bugs$ST
n <- length(ST)
k <- length(names(xtabs(~ST)))
data <- list("n", "y", "ST", "k")

inits <- function() {
  list (sigma.y = runif(1,0,100), tau.y = rexp(1,3),
        b = rnorm(51,0,.0001))}

model1 <- bugs(data,inits,model1.p#############ar,"model_state.txt", n.chains=1, n.iter=20000)

v <- vote$VOTE2008
vote <- cbind(vote,as.numeric(vote$STATE))
names(vote)[5] <- "ST"
               
model2.par <- c ("b")
aux <- names(xtabs(~bugs$CP5_1))[1]
y <- ifelse(bugs$CP5_1 == aux,1,0)
ST <- bugs$ST
n <- length(ST)
k <- length(names(xtabs(~ST)))
data <- list("n", "y", "ST", "k","v")

inits <- function() {
  list (sigma.y = runif(1,0,100), tau.y = rexp(1,3), tau.b = rexp(1,30),
        b = rnorm(51,0,.0001))}

model2 <- bugs(data,inits,model2.par,"model_hier.txt", n.chains=1, n.iter=2000)


var <- vote$var
data <- list("n", "y", "ST", "k","v","var")
               
inits <- function() {
       list (sigma.y = runif(1,0,100), tau.y = rexp(1,3),
       b = rnorm(51,0,.0001))}
               
               
model3 <- bugs(data,inits,model2.par,"model_hier2.txt", n.chains=1, n.iter=2000)
               
#################################################
############ Filme Eleicao 2012 #################
#################################################

require(reshape)
data <- melt(vote)
data$variable <- as.numeric(data$variable)


weeks <- 40
for(i in 1:weeks){nea
  
  aux_zoom <- min(0 + (i-1)*0.02,0.3)
  
  aux.data <- data[data$variable <= i,]
  
  png(file = paste(paste("F:/DADOS/DOWNLOAD/R Program/FFmpeg/filme/elei", "-",formatC(i, digits = 3, flag = "0"), sep = ""), "png", sep = "."), bg = "transparent")
  
  gg <- ggplot(aux.data, aes(variable, value, group = Vote, colour = Vote)) + geom_line(size = 2) +opts(title = "Presidential Election 2012",axis.text.x=theme_text(angle=90)) + xlim(0, weeks)+ ylim(aux_zoom,0.5)
  print(gg)
  
  dev.off()
  
}

# rodar no prompt do DOS no diretorio com as fotos  -vf reduz velocidade do video
#ffmpeg -f image2 -i elei-0%3d.png -vf "setpts=5.0*PTS" -y video.mpg

#add sound:  ffmpeg -i son.wav -i video_origine.avi video_finale.mpg

#################################################
####### Automated Extracting Zip Files ##########
#################################################

#Para usar e' preciso instalar o 7-Zip no diretorio dir.zip

#comandos do command line - podem ser concatenados com &&
#set path=C:\Program Files\7-Zip
#cd F:\DADOS\Bancos de Dados\Censo 2010\Mapas PDF - Setores Censitarios
#7z e 1100064.zip -o"destino" *.pdf -r

dir.mapas <- 'F:\\DADOS\\Bancos de Dados\\Censo 2010\\Mapas PDF - Setores Censitarios'
dir.zip <- 'C:\\Program Files\\7-Zip'
aux1 <- paste("cd ",shQuote(dir.mapas),sep='')
aux2 <- "f:"
aux3 <- paste("set path=",shQuote(dir.zip),sep='')

setwd("F:/DADOS/Bancos de Dados/Censo 2010/Mapas PDF - Setores Censitarios")
aux.files <- dir(pattern="*.zip")
n <- length(aux.files)

for (i in 1:n) {
  
  dir.saida <- substr(aux.files[i],1,7)
  arq.entrada <- aux.files[i]
  aux4 <- paste("7z e ",arq.entrada, " -o",dir.saida," *.pdf -r",sep ='')
  com <- paste(aux1,aux2,aux3,aux4, sep ="&&")
  shell(com)
  
}

               
#################################################
######### Aproximate String Matching ############
#################################################

#approximate string matching
agrep(exp,texto,max.distance = 2)
#distancia de Levenshtein
adist(exp,texto)
#distancia de Levenshtein explicando as substituicoes
adist(exp,texto,counts = TRUE)

ggg <- c("neale","nilon","nil","nilo","nillo","nniilloo","nuko","lara")
ggg[agrep('nilo',ggg,max.distance = 1)]
adist('nilo',ggg)
adist('nilo',ggg,counts = TRUE)

               
#################################################
################ Google Search ##################
#################################################

require(RCurl)
library(XML)

#incluir parametro: as_filetype=pdf para pesquisas apenas pdf
site <- getForm("http://www.google.com/search", hl="en",lr="lang_pt",cr="countryBR",
                q="AL-00065/2012 pesquisa registro", start=00)
g <- htmlParse(site, asText = TRUE)
h <- xpathApply(g,"//a/@href")
b <- grep('googleusercontent',h, value = TRUE)
d <- regexpr('http:',b)
fim <- substr(b,d,nchar(b))
d <- regexpr('%2B',fim)
fim <- substr(fim,rep(1,length(d)),d-1)

teste <- getURL(fim[9])

writeClipboard(teste)

       
#########################################
########## ggplot2 - Non-Voters Graph
#########################################
  
qplot(q19, q24,geom = c("point"),size = size, colour = cluster, title = "Clusters", data = mapa) 
+ geom_text(aes(label = cluster), size = 3,hjust = 0.5, vjust=-1.3, data = mapa) 
+ opts(title = "Clusters Non-Voters") 
+ xlab("Q19 - Closely following elections")+ ylab("Q24 - Improvement of financial situation") 
+ opts(axis.text.y=theme_blank(),axis.text.x=theme_blank())
               
               
library(XML)
library(RCurl)

##########
               
url <- "http://atarde.uol.com.br/politica/materias/1463311-pesquisa-ibope-aponta-vitoria-de-haddad-hoje-em-sp"
               
sera <- getURL(url)
g <- htmlParse(sera, asText = TRUE)
bn <- xpathApply(g,"//text()")
               
bn <- xpathApply(g,"//child::fieldset/text()")
               
               
g <- readLines(url)
               
agrep('haddad',g,value=TRUE)
aux <- g[805]
ttt <- unlist(strsplit(aux," "))
agrep('haddad',ttt,value=FALSE)
               
sss <- sapply(c('haddad','serra'),agrep,x = g,value = TRUE)
lapply(sss,strsplit,split = " ")
               
adist('haddad',g)
               
############################################
               
library(XML)
library(RCurl)
               
#url <- "http://noticias.uol.com.br/fernandorodrigues/pesquisas/2012/1turno/prefeito/sao-paulo.jhtm"
               
url <- "http://noticias.uol.com.br/politica/pesquisas/"
sera <- getURL(url)
g <- htmlParse(sera, asText = TRUE)
h <- xpathApply(g,"//a/@href")
h1 <- grep('fernandorodrigues',h, value = TRUE)               
#h <- xpathApply(g,"//option")
b <- readHTMLTable(g)
               

sera <- getURL(h1[2])
g <- htmlParse(sera, asText = TRUE)
h <- xpathApply(g,"//a/@href")
u <- xpathApply(g,"//li/text()")
h1 <- grep('fernandorodrigues',h, value = TRUE)               
u1 <- grep('fernandorodrigues',u, value = TRUE)
               

########################
               
setwd("C:/NEALE/Projects/Reuters 2012 Online Polling/Reuters Tracker 2012/Consolidated/Daily moving average")
xls <- dir(pattern="^Reuters Tracker - Daily (MA|Moving Average)",recursive = TRUE)
writeClipboard(xls)
file.copy(xls, "C:/NEALE/Projects/Reuters 2013 Online Polling/Reuters Tracker 2013/Consolidated/2012 Published data")
	
	
########  IMORTANDO TODOS OS ARQUIVOS DO TSE ########################

library(foreign)
	
dir <- "F://DADOS//Bancos de Dados//Dados Eleitorais//Resultado das Eleicoes//Agregado Final"
setwd(dir)

aux.files1 <- dir(pattern="votacao_candidato")
n1 <- length(aux.files1)

cont <- 0
for (i in 211:234) {	
	cont <- cont + 1
	print(i)
	file <- aux.files1[i]
	mydata.aux = read.table(file,header = FALSE, sep = ";")
	lin <- dim(mydata.aux)[1]
	col <- dim(mydata.aux)[2]
	if (col == 27) {
		 tempcol <- matrix(c(rep.int(NA,2* lin)),nrow=lin,ncol=2)
		 mydata.aux <- cbind(mydata.aux[,1:26],tempcol,mydata.aux[,27])
		 names(mydata.aux)[27:29] <- c("V27","V28","V29")
	}

	if (cont == 1) {
		mydata = mydata.aux
	} else {
		mydata = rbind(mydata,mydata.aux)
	}
}
	
names(mydata) <- c('DATA_GERACAO','HORA_GERACAO','ANO_ELEICAO','NUM_TURNO','DESCRICAO_ELEICAO','SIGLA_UF','SIGLA_UE','CODIGO_MUNICIPIO','NOME_MUNICIPIO','NUMERO_ZONA','CODIGO_CARGO','NUMERO_CAND','SQ_CANDIDATO','NOME_CANDIDATO','NOME_URNA_CANDIDATO','DESCRICAO_CARGO','COD_SIT_CAND_SUPERIOR','DESC_SIT_CAND_SUPERIOR','CODIGO_SIT_CANDIDATO','DESC_SIT_CANDIDATO','CODIGO_SIT_CAND_TOT','DESC_SIT_CAND_TOT','NUMERO_PARTIDO','SIGLA_PARTIDO','NOME_PARTIDO','SEQUENCIAL_LEGENDA','NOME_COLIGACAO','COMPOSICAO_LEGENDA','TOTAL_VOTOS')
write.table(mydata, 'F:/DADOS/Bancos de Dados/Dados Eleitorais/Resultado das Eleicoes/Agregado Final/resultados6.csv', sep=";", row.names=FALSE)
rm(mydata)
	
	
aux.files2 <- dir(pattern="detalhe_votacao")
n2 <- length(aux.files2)

cont <- 0
for (i in c(1:5,7:66,68:176,178, 180:249)) {	
	cont <- cont + 1
	print(i)
	file <- aux.files2[i]
	mydata.aux = read.table(file,header = FALSE, sep = ";")

	if (cont == 1) {
		mydata = mydata.aux
	} else {
		mydata = rbind(mydata,mydata.aux)
	}
}
	
names(mydata) <- c('DATA_GERACAO','HORA_GERACAO','ANO_ELEICAO','NUM_TURNO','DESCRICAO_ELEICAO','SIGLA_UF','SIGLA_UE','CODIGO_MUNICIPIO','NOME_MUNICIPIO','NUMERO_ZONA','CODIGO_CARGO','DESCRICAO_CARGO','QTD_APTOS','QTD_SECOES','QTD_SECOES_AGREGADAS','QTD_APTOS_TOT','QTD_SECOES_TOT','QTD_COMPARECIMENTO','QTD_ABSTENCOES','QTD_VOTOS_NOMINAIS','QTD_VOTOS_BRANCOS','QTD_VOTOS_NULOS','QTD_VOTOS_LEGENDA','QTD_VOTOS_ANULADOS_APU_SEP','DATA_ULT_TOTALIZACAO','HORA_ULT_TOTALIZACAO')
write.table(mydata, 'F:/DADOS/Bancos de Dados/Dados Eleitorais/Resultado das Eleicoes/Agregado Final/detalhes1.csv', sep=";", row.names=FALSE)
rm(mydata)

cont <- 0
for (i in c(250:n2)) {	
	cont <- cont + 1
	print(i)
	file <- aux.files2[i]
	mydata.aux = read.table(file,header = FALSE, sep = ";")

	if (cont == 1) {
		mydata = mydata.aux
	} else {
		mydata = rbind(mydata,mydata.aux)
	}
}
	
names(mydata) <- c('DATA_GERACAO','HORA_GERACAO','ANO_ELEICAO','NUM_TURNO','DESCRICAO_ELEICAO','SIGLA_UF','SIGLA_UE','CODIGO_MUNICIPIO','NOME_MUNICIPIO','NUMERO_ZONA','CODIGO_CARGO','DESCRICAO_CARGO','QTD_APTOS','QTD_SECOES','QTD_SECOES_AGREGADAS','QTD_APTOS_TOT','QTD_SECOES_TOT','QTD_COMPARECIMENTO','QTD_ABSTENCOES','QTD_VOTOS_NOMINAIS','QTD_VOTOS_BRANCOS','QTD_VOTOS_NULOS','QTD_VOTOS_LEGENDA','QTD_VOTOS_ANULADOS_APU_SEP','DATA_ULT_TOTALIZACAO','HORA_ULT_TOTALIZACAO')
write.table(mydata, 'F:/DADOS/Bancos de Dados/Dados Eleitorais/Resultado das Eleicoes/Agregado Final/detalhes2.csv', sep=";", row.names=FALSE)
rm(mydata)
	


### twitter
	
Consumer key 	NhRImDpbdiQy4OhdLzJhAg
Consumer secret 	StHC7FvbFVYtli00pQgNAuHIHkfWeK1XWwhmzVbvtHU	

require(twitteR)	

#getTwitterOAuth('NhRImDpbdiQy4OhdLzJhAg','StHC7FvbFVYtli00pQgNAuHIHkfWeK1XWwhmzVbvtHU');
#searchTwitter('apartment hunting', geocode='40.7361,-73.9901,5mi',  n=5000, retryOnRateLimit=1)

#http://davetang.org/muse/2013/04/06/using-the-r_twitter-package/
	
#install the necessary packages
install.packages("ROAuth")
install.packages("twitteR")
install.packages("wordcloud")
install.packages("tm")
 
library("ROAuth")
library("twitteR")
library("wordcloud")
library("tm")
 
#necessary step for Windows
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
 
#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
cred <- OAuthFactory$new(consumerKey='NhRImDpbdiQy4OhdLzJhAg',
consumerSecret='StHC7FvbFVYtli00pQgNAuHIHkfWeK1XWwhmzVbvtHU',
requestURL='https://api.twitter.com/oauth/request_token',
accessURL='http://api.twitter.com/oauth/access_token',
authURL='http://api.twitter.com/oauth/authorize')
 
#necessary step for Windows
cred$handshake(cainfo="cacert.pem")
#save for later use for Windows
save(cred, file="twitter authentication.Rdata")
registerTwitterOAuth(cred)
 
#the cainfo parameter is necessary on Windows
r_stats<- searchTwitter("#Rstats", n=1500, cainfo="cacert.pem")
#save text
r_stats_text <- sapply(r_stats, function(x) x$getText())
#create corpus
r_stats_text_corpus <- Corpus(VectorSource(r_stats_text))
#clean up
r_stats_text_corpus <- tm_map(r_stats_text_corpus, tolower)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()))
wordcloud(r_stats_text_corpus)

bioinformatics <- searchTwitter("#bioinformatics", n=1500, cainfo="cacert.pem")
bioinformatics_text <- sapply(bioinformatics, function(x) x$getText())
bioinformatics_text_corpus <- Corpus(VectorSource(bioinformatics_text))
bioinformatics_text_corpus <- tm_map(bioinformatics_text_corpus, tolower)
bioinformatics_text_corpus <- tm_map(bioinformatics_text_corpus, removePunctuation)
bioinformatics_text_corpus <- tm_map(bioinformatics_text_corpus, function(x)removeWords(x,stopwords()))
wordcloud(bioinformatics_text_corpus)

library(RColorBrewer)
pal2 <- brewer.pal(8,"Dark2")
wordcloud(bioinformatics_text_corpus,min.freq=2,max.words=100, random.order=T, colors=pal2)


#followers
me <- getUser("davetang31", cainfo="cacert.pem")
me$getFollowerIDs(cainfo="cacert.pem")
#or
me$getFollowers(cainfo="cacert.pem")
#you can also see what's trending
trend <- availableTrendLocations(cainfo="cacert.pem")


#data mining in twitter
	
# installation is required only required once and is rememberd across sessions
install.packages('XML')
 
# loading the package is required once each session
require(XML)
 
# initialize a storage variable for Twitter tweets
mydata.vectors <- character(0)
 
# paginate to get more tweets
for (page in c(1:15))
{
    # search parameter
    twitter_q <- URLencode('#neymar')
    # construct a URL
    twitter_url = paste('http://search.twitter.com/search.atom?q=',twitter_q,'&rpp=100&page=', page, sep='')
    # fetch remote URL and parse
    mydata.xml <- xmlParseDoc(twitter_url, asText=F)
    # extract the titles
    mydata.vector <- xpathSApply(mydata.xml, '//s:entry/s:title', xmlValue, namespaces =c('s'='http://www.w3.org/2005/Atom'))
    # aggregate new tweets with previous tweets
    mydata.vectors <- c(mydata.vector, mydata.vectors)
}
 
# how many tweets did we get?
length(mydata.vectors)
	
	###
### Use tm (text mining) package
###
 
install.packages('tm')
require(tm)
 
# build a corpus
mydata.corpus <- Corpus(VectorSource(mydata.vectors))
 
# make each letter lowercase
mydata.corpus <- tm_map(mydata.corpus, tolower)
 
# remove punctuation
mydata.corpus <- tm_map(mydata.corpus, removePunctuation)
 
# remove generic and custom stopwords
my_stopwords <- c(stopwords('english'), 'prolife', 'prochoice')
mydata.corpus <- tm_map(mydata.corpus, removeWords, my_stopwords)
 
# build a term-document matrix
mydata.dtm <- TermDocumentMatrix(mydata.corpus)
 
# inspect the document-term matrix
mydata.dtm
 
# inspect most popular words
findFreqTerms(mydata.dtm, lowfreq=30)
	
# remove sparse terms to simplify the cluster plot
# Note: tweak the sparse parameter to determine the number of words.
# About 10-30 words is good.
mydata.dtm2 <- removeSparseTerms(mydata.dtm, sparse=0.95)
 
# convert the sparse term-document matrix to a standard data frame
mydata.df <- as.data.frame(inspect(mydata.dtm2))
 
# inspect dimensions of the data frame
nrow(mydata.df)
ncol(mydata.df)

mydata.df.scale <- scale(mydata.df)
d <- dist(mydata.df.scale, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")
plot(fit) # display dendogram?
 
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red")
	
	
###################################################
################## SQL ###########################
##################################################

#http://www.r-bloggers.com/make-r-speak-sql-with-sqldf/

library(sqldf)
sqldf('select PE3, count(*) as "Freq" from data group by PE3')
	
	