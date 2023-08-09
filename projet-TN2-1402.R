
#Importation des données 
Donnees.diabetes <- read.table("diabetes_prediction_dataset.csv",header=TRUE,sep = ",")
#Observation des données
View(Donnees.diabetes) 
str(Donnees.diabetes)
#Modification du nom des colonnes
colnames(Donnees.diabetes) <- c("sex", "age", "hypertension", "heart_disease", "smoking_history", "bmi", "AbA1c_level", "glycemia", "diabetes")
View(Donnees.diabetes)
#Recoder les variables qualitatives
Donnees.diabetes$sex <- as.factor(Donnees.diabetes$sex)
Donnees.diabetes$hypertension <- as.factor(Donnees.diabetes$hypertension)
Donnees.diabetes$heart_disease <- as.factor(Donnees.diabetes$heart_disease)
Donnees.diabetes$smoking_history <- as.factor(Donnees.diabetes$smoking_history)
Donnees.diabetes$diabetes <- as.factor(Donnees.diabetes$diabetes)
#Recoder les variables quantitatives
Donnees.diabetes$age <- as.integer(Donnees.diabetes$age)
Donnees.diabetes$bmi <-as.integer(Donnees.diabetes$bmi)   
Donnees.diabetes$AbA1c_level <- as.integer(Donnees.diabetes$AbA1c_level)
Donnees.diabetes$glycemia <- as.integer(Donnees.diabetes$glycemia)
str(Donnees.diabetes)
View(Donnees.diabetes)
#Recoder les modalités des variables
levels(Donnees.diabetes$sex) <- c("Femme", "Homme", "Autre")
levels(Donnees.diabetes$hypertension) <- c("Non", "Oui")
levels(Donnees.diabetes$heart_disease) <- c("Non","Oui")
levels(Donnees.diabetes$smoking_history) <- c("current", "ever", "former", "never", "No Info", "not current")
levels(Donnees.diabetes$diabetes) <- c("Non", "Oui")
View(Donnees.diabetes)
#Vérification des types devariables
str(Donnees.diabetes)
#Dernière vérification de valeurs manquantes et fin du prétraitement des données
apply(Donnees.diabetes,2,anyNA)
#Indicateurs clés(variables qualitaives)
#Calcul des effectifs-variable sex
table(Donnees.diabetes$sex)
#Calcul des fréquences
prop.table(table(Donnees.diabetes$sex))
round(prop.table(table(Donnees.diabetes$sex)),4)
round(prop.table(table(Donnees.diabetes$sex)),4)*100
#Effectifs variables hypertension
table(Donnees.diabetes$hypertension)
round(prop.table(table(Donnees.diabetes$hypertension)),4)*100
#Effectifs variables heart_disease
table(Donnees.diabetes$heart_disease)
round(prop.table(table(Donnees.diabetes$heart_disease)),4)*100
#Effectifs variables smokin_history
table(Donnees.diabetes$smoking_history)
round(prop.table(table(Donnees.diabetes$smoking_history)),4)*100
#Effectifs variables diabètes
table(Donnees.diabetes$diabetes)
round(prop.table(table(Donnees.diabetes$diabetes)),4)*100
#Indicateurs pour les variables quantitatives
#Variable âge
summary(Donnees.diabetes$age)
#Variable bmi
summary(Donnees.diabetes$bmi)
#Variable AbA1c_level
summary(Donnees.diabetes$AbA1c_level)
#Variable glycemia
summary(Donnees.diabetes$glycemia)
#Calcul des indicateurs statistiques
var(Donnees.diabetes$age)
sd(Donnees.diabetes$age)
var(Donnees.diabetes$bmi)
sd(Donnees.diabetes$bmi)
var(Donnees.diabetes$AbA1c_level)
sd(Donnees.diabetes$AbA1c_level)
var(Donnees.diabetes$glycemia)
sd(Donnees.diabetes$glycemia)
#Les diagrammes destinées aux variables qualitatives
#Variable "sex"
graph1 <-plot(Donnees.diabetes$sex, xlab="sex", ylab="Effectifs",
              main="Répartition des patients selon le sex",
              las=1,
              sub="Données:www.kaggle.com/datasets/iammustafatz/,
                  diabetes-prediction-dataset/discussion",
              col=c("green", "red"),
              cex.main=0.8,
              cex.axis=0.8,
              cex.lab=0.8,
              ylim=c(0,100000))
text(x=graph1, y=table(Donnees.diabetes$sex)+10,labels=as.character(table(Donnees.diabetes$sex)),
     cex=1.1,font=3)

#Les diagrammes des variables quantitatives
#La variable "âge"
graph2 <- boxplot(Donnees.diabetes$age,
        ylab="Age", main="Boite à moustache des patients selon l'âge",
        col="green",
        las=1,
        cex.main=0.8,
        cex.lab=1.0,
        sub="Données:www.kaggle.com/datasets",
        notch=TRUE,
        border="blue",
        ylim=c(0,80))
#La variable "glycemie"
graph3 <- hist(Donnees.diabetes$glycemia,
               xlab="Glycemia",
               ylab="Effectifs",
               main="Répartition des patients selon le taux de glycemie",
               las=1,
               sub="Données:www.kaggle.com/datasets",
               col="green",
               ylim=c(0,50000),
               xlim=c(0,250),
               cex.main=0.7,
               cex.lab=0.7)
text(x=graph3$mids, graph3$counts,labels=graph3$counts,
     adj=c(0.3,0.3))
#Graphiques croisés: Analyse bivariée
#Variable sex avec la varible diabètes
graph4 <- barplot(table(Donnees.diabetes$diabetes, Donnees.diabetes$sex),
                  beside=TRUE,
                  col=c("green","red"),
                  xlab="sex",
                  ylab="patients",
                  main="Répartition des patients selon la présence,
                  \nd de la maladie diabètes et le sexe",
                  ylim=c(0,40000),
                  cex.main=0.9,
                  cex.lab=0.9)
legend("topright", legend=levels(Donnees.diabetes$diabetes),
       fil=c("green", "red"),
       title="Maladie diabetes",
       horiz=TRUE)
text(x=graph4,y=table(Donnees.diabetes$diabetes,Donnees.diabetes$sex)+10,
     labels=as.character(table(Donnees.diabetes$diabetes,Donnees.diabetes$sex)),
     cex=1.0,font=3, adj=c(0.2, 0.2))
#Boites à moustaches croisées
#Variables "age" et "Maladies du diabètes"
graph5 <- boxplot(Donnees.diabetes$age~Donnees.diabetes$diabetes,
                  main="Boite à moustache des patients selon l'âge \n et,
                  la présence du diabète",
                 xlab="Présence du diabète",
                 ylab="Age",
                 col="yellow",
                 las=1,
                 ylim=c(0, 80),
                 cex.main=0.7,
                 cex.lab=0.7)

#Tests statistiques
#Calcul des pourcentages
table(Donnees.diabetes$sex, Donnees.diabetes$diabetes)
round(prop.table(table(Donnees.diabetes$sex, Donnees.diabetes$diabetes),
                 margin = 1),4)*100
round(prop.table(table(Donnees.diabetes$hypertension, Donnees.diabetes$diabetes),
                 margin = 1),4)*100
round(prop.table(table(Donnees.diabetes$heart_disease, Donnees.diabetes$diabetes),
                 margin = 1),4)*100
round(prop.table(table(Donnees.diabetes$smoking_history, Donnees.diabetes$diabetes),
                 margin = 1),4)*100                  
#Test de Khi-Deux
##H0: Les deux variables sont indépendantes(si p-value>0.05)
##H1: Les deux variables sont dépendantes(si p-value<0.05)
chisq.test(Donnees.diabetes$sex, Donnees.diabetes$diabetes)
chisq.test(Donnees.diabetes$hypertension, Donnees.diabetes$diabetes)
chisq.test(Donnees.diabetes$heart_disease, Donnees.diabetes$diabetes)
chisq.test(Donnees.diabetes$smoking_history, Donnees.diabetes$diabetes)
#Les variables quantitatives
##Calcul des moyennes
tapply(Donnees.diabetes$age, Donnees.diabetes$diabetes,mean)
tapply(Donnees.diabetes$bmi, Donnees.diabetes$diabetes,mean)
tapply(Donnees.diabetes$AbA1c_level, Donnees.diabetes$diabetes,mean)
tapply(Donnees.diabetes$glycemia, Donnees.diabetes$diabetes,mean)


#Indépendance entre une variable qualitative et une variable quantitative
##Test de shapiro-wilk
###H0: L'échantillon suit une distribution normale(si p-value>0.05)
###H1: L'échantillon  ne suit pas une distribution normale(si p-value<0.05)
library(dplyr)
 
#Le test de shapiro wilk peut s'effectuer sur un échantillon de taille comprise entre 3 et 5000
#notre échantillon a une taille de plus 100 000 données.

#Le test de Mann-whitney
###H0: Il n'ya pas de différence significative entre la moyenne des 2 variables(si p-value>0.05)
###H1: Il ya de une différence significative entre la moyenne des 2 variables(si p-value<0.05)
wilcox.test(Donnees.diabetes$age~Donnees.diabetes$diabetes)
wilcox.test(Donnees.diabetes$bmi~Donnees.diabetes$diabetes)
wilcox.test(Donnees.diabetes$AbA1c_level~Donnees.diabetes$diabetes)
wilcox.test(Donnees.diabetes$glycemia~Donnees.diabetes$diabetes)
#Le test de Student
###H0: Il n'ya pas de différence significative entre la moyenne des 2 variables(si p-value>0.05)
###H1: Il ya de une différence significative entre la moyenne des 2 variables(si p-value<0.05)
t.test(Donnees.diabetes$glycemia~Donnees.diabetes$diabetes)
t.test(Donnees.diabetes$age~Donnees.diabetes$diabetes)
t.test(Donnees.diabetes$bmi~Donnees.diabetes$diabetes)
t.test(Donnees.diabetes$AbA1c_level~Donnees.diabetes$diabetes)

#Division du jeu de données en jeu d'entrainement et de test
set.seed(99)
library(caTools)
split=sample.split(Donnees.diabetes$diabetes, SplitRatio = 0.8)
train=subset(Donnees.diabetes, split==TRUE)
test=subset(Donnees.diabetes, split==FALSE)
#Création du modèle
regressionlogistique=glm(diabetes~., data =train,
                         family = "binomial")
summary(regressionlogistique)
#Optimisation de la regression logistique
#Optimisation du modèle de regression logistique pour ne conserver que les variables signif
regressionlogistique=update(regressionlogistique,.~.,-smoking_history)
summary(regressionlogistique)
#Prédiction
prediction=predict(regressionlogistique, test, type="response")
prediction
tableau_prediction=as.data.frame(prediction)
#Création de fonction
creation_fonction=function(x){
  return(ifelse(x>0.5,1,0))
  }
tableau_prediction=apply(tableau_prediction, 2, creation_fonction)
#Mesure des performances de notre modèle
levels(test$diabetes) <- c(0,1)
levels(test$diabetes)
library(caret)
confusionMatrix(as.factor(test$diabetes), as.factor(tableau_prediction))
#Taleau de comparaison
tableau_comparaison=cbind(test, tableau_prediction)
tableau_comparaison$prediction <- as.factor(tableau_comparaison$prediction)
levels(tableau_comparaison$diabetes) <- c("Non", "Oui")   
levels(tableau_comparaison$prediction) <- c("Non", "Oui")
#Test de Hosmer et Lemshow
##H0:L'ajustement du modèle aux données est bon(si p-value> 0.05)
##H1:L'ajustement du modèle aux données est mauvais(si p-value< 0.05)
library(performance)
performance_hosmer(regressionlogistique)
#La courbe ROC
library(pROC)
par(pty="s")
roc(train$diabetes, regressionlogistique$fitted.values, plot=TRUE,
    main="courbe de ROC du modèle de regression\nlogistique",
    col="green",
    lwd=4,
    xlab="Taux de faux positifs",
    ylab="Taux de vrais positifs",
    legacy.axes=TRUE)


 