#ensayo de bernoulli 

x <- c(0,1)
f<- c(0.68,0.32)


plot(x,f,type="h",ylim=c(0,1),col="red")
points(x,f,pch=16,col="red")

n<- 43
muestra <- sample(x,n,f,replace=TRUE)

pie(table(muestra))/n
mean(muestra)


bar <- barplot(table(muestra)/n,ylim=c(0,1))
lines(bar,f,type="h",col="red")
points(bar,f,pch=16,col="red")


muestra <- sample(x,n,f,replace=TRUE)
muestra
sum(muestra)



Y<-function(i){sum(sample(x,n,f,replace=TRUE))}
Y(1)
Y(2)

m<-40
sapply(1:m, Y) #repatirho 40 cops

n<-40
encuestas<-sapply(1:n, Y)

fr<- table(encuestas)/n
fr["13"]
xx<-names(fr)
###########################################################
#Ex 1


x <- c(0,1)
f<- c(0.68,0.32)
n<- 43
Y<-function(i){sum(sample(x,n,f,replace=TRUE))}
m<- 400000
muestra <- sample(x,n,f,replace=TRUE)
encuestas<-sapply(1:m, Y)

dbinom(13,43,0.32)


br<- barplot(table(encuestas/m))
lines(br,dbinom(2:29,43,0.32),type="h",col="red")
points(br,dbinom(2:29,43,0.32),pch="h",col="red")

pbinon()


#Ex2
dbinom(17,44,0.32)
plot(0:43,dbinom(0:43,44,0.32),type="h",col="red")
pbinom(16,44,0.32)


###Ex3
n<- 24
x <- c(0,1)
f<- c(0.32,0.68)

xstar<- function(i){sum(sample(x,n,f,replace=T))}
set.seed(123)
m<-400000
enquestas2<- sapply(1:m,xstar)
mean(encuestas2)
m*0.68



























