arrest = read.csv('C:/Users/tshir/Downloads/Adult_Arrests_18_and_Older_by_County___Beginning_1970.csv', header = TRUE)

#Comte 
comte=arrest$County[arrest$Year=='1970']
comte


#Exemple: Moyennes des arrestations dans l'annee 1970
moy1970=mean(arrest$Total[arrest$Year=='1970'])
moy1970

#Vecteurannee
annee=factor(arrest$Year)
annee
annai=levels(annee)
annai

#Boucle pour faire la moyenne de tout les crimes et delits par annee
MTPA=c()
for (i in 1:length(annai)){
                           a=mean(arrest$Total[arrest$Year==annai[i]])
                           MTPA[i]=c(print(a))
                          }
MTPA
#Graphe 1
plot(annai, MTPA, main='Moyenne des infractions de 1970 a 2019',xlab='annee', col='blue')
lines(annai, MTPA, col='red')

#Comtés qui possèdent en moyenne le plus d'infraction
comte=arrest$County[arrest$Year=='1970']
comte
comtes=factor(comte)
comtes
comtais=levels(comtes)
comtais
mean(arrest$Total[arrest$County=='Allegany'])
MTPC=c()
for (i in 1:length(comte)){
                            b=mean(arrest$Total[arrest$County==comte[i]])
                            MTPC[i]=c(print(b))

}
MTPC
#Graph 2 frequence de tout les crime+delit
hist(MTPC, n=15, col='red', main='Histogramme moyennes infractions 1970:2019 par comte',ylab='Nombre de Comte')
boxplot(MTPC, outline = F, horizontal = T, border = 'blue', xlab='Moyenne totaux', main='Boite a moustache des moyenne des infraction pour chaque annee')
barplot(dfc[order(dfc[,1],decreasing=TRUE),][,1],names.arg=dfc[order(dfc[,1],decreasing=TRUE),][,2],las=2,main = 'Moyenne des crimes et delits par comte',col='purple' )

#Les comtes qui possedent en moyenne plus de 40000 infraction
valsup=c()
comtesup=c()
for(i in 1:length(MTPC)){
  if(MTPC[i]>=40000){
    comtesup[i]=c(print(comte[i]))
    valsup[i]=c(print(MTPC[i]))
  }
  
}
valsup
comtesup
#Data frames des moyennes d'infraction entre 1970 et 2019 par comte
dfc=data.frame(MTPC,comte)
dfc

#Data frame concernant les 4 comte ayant le plus d'infraction grace au reperage fait avant.
dfbronx=data.frame(annai,arrest$Total[arrest$County=='Bronx'])
dfkings=data.frame(annai,arrest$Total[arrest$County=='Kings'])
dfnewyork=data.frame(annai,arrest$Total[arrest$County=='New York'])
dfqueens=data.frame(annai,arrest$Total[arrest$County=='Queens'])
#Graph Bronx
plot(annai,arrest$Total[arrest$County=='Bronx'],main='Evolution des infractions pour le Bronx', xlab='annee',ylab='Total Infraction',col='orange')
lines(annai, arrest$Total[arrest$County=='Bronx'],col='black')

#Graph Kings
plot(annai,arrest$Total[arrest$County=='Kings'],main='Evolution des infractions pour Kings', xlab='annee',ylab='Total Infraction',col='orange')
lines(annai, arrest$Total[arrest$County=='Kings'],col='black')

#Graph New York
plot(annai,arrest$Total[arrest$County=='New York'],main='Evolution des infractions pour New York', xlab='annee',ylab='Total Infraction',col='orange')
lines(annai, arrest$Total[arrest$County=='New York'],col='black')

#Graph Queens
plot(annai,arrest$Total[arrest$County=='Queens'],main='Evolution des infractions pour le Queens', xlab='annee',ylab='Total Infraction',col='orange')
lines(annai, arrest$Total[arrest$County=='Queens'],col='black')
                         