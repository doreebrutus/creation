##########################Informatique pour Science Sociales###################

###Exercice.1
#Creer une fonction qui indique si un nombre est a la fois u multiple de 3 et un multiple de 5.

fmultiple<-function(x){
  if(x%%3==0 & x%%5==0){
    print(x)
    print("est a la fois un multiple de 3 et de 5")
  }
  else{
    print(x) 
    print( "Il n'est ni un multiple de 3 et de 5")
  }
}

#Exemple
A<-15
fmultiple(A)



###Exercice.2
#Creer une fonction qui retourne tous les nombres d'un vecteur qui sont a la fois multiple de 3 et 5.
fvecteur<-function(x){
  multiple=NULL
  for ( i in 1:length(x)){
    if(x[i]%%3==0 & x[i]%%5==0){
      multiple[i]=" Celui-ci est a la fois un multiple de 3 et de 5"}
    else{
      multiple[i]="Helas, il n'est ni un multiple de 3 et de 5"}
  }
  return(multiple)
}

#Exemple
B<-1:50
fvecteur(B)

###Exercice. 3
#Creer une fonction qui permet d'afficher seulement les coefficients (Estimates) et les statistiques
#de student lorsque l'on realise une regression lineaire.


fcoefficient<-function(x){
  dat<-data.frame(y, x1, x2)
  estimation<-summary(lm(y~.,dat),na.rm=T)
  student<-t.test(dat,na.rm=T)
  Regression<-list(summary(lm(y~.,dat),na.rm=T),t.test(dat,na.rm=T))
  return(Regression)
}

#Exemple
y<-c(30,45,90,42,10)
x1<-c(14,80,46,14,17)
x2<-c(34,53,26,19,89)
fcoefficient(dat)
