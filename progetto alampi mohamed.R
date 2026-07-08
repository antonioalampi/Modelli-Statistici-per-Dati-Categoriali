## PROGETTO ALAMPI MOHAMED ##

library(car)
library(MASS)
library(ggplot2)
library(dplyr) # Per la funzione glimpse.
library(caret)
library(purrr)
library(epiDisplay)
library(sjPlot)
library(pROC)
library(tidyr)
library(mltest)
library(glmtoolbox)
library(knitr)  
library(kableExtra) 
library(ggpubr)
library(corrplot)
library(RColorBrewer)

# LETTURA DATI

dati<-read.csv(file="sleep_health.csv", sep=";", header=TRUE)
View(dati)
dim(dati)
attach(dati)

# controllo missing values
anyNA(dati)

# visualizzazione dati
glimpse(dati)
summary(dati)
str(dati)
head(dati, 15)

# RENDO ALCUNI VETTORI NUMERICI (DA VETTORI STRINGA)

# dim(dati)[1] == 374, numero di righe (cioe' n. di individui)
for (i in 1:374) {
  if (gender[i]=="Male"){
      gender[i] <- 0}
  if (gender[i]=="Female"){
      gender[i] <- 1}
}

gender <- as.numeric(as.character(gender))
dati$gender <- gender
  
for (i in 1:374) {
  if (occupation[i]=="Software Engineer") {
      occupation[i] <- 0}
  if (occupation[i]=="Doctor"){
      occupation[i] <- 1}
  if (occupation[i]=="Sales Representative"){
      occupation[i] <- 2}
  if (occupation[i]=="Teacher"){
      occupation[i] <- 3}
  if (occupation[i]=="Nurse"){
      occupation[i] <- 4}
  if (occupation[i]=="Engineer"){
      occupation[i] <- 5}
  if (occupation[i]=="Accountant"){
      occupation[i] <- 6}
  if (occupation[i]=="Scientist"){
      occupation[i] <- 7}
  if (occupation[i]=="Lawyer"){
      occupation[i] <- 8}
  if (occupation[i]=="Salesperson"){
      occupation[i] <- 9}
  if (occupation[i]=="Manager"){
      occupation[i] <- 10}
}

occupation <- as.numeric(as.character(occupation))
dati$occupation <- occupation

for (i in 1:374) {
  if (bmi[i]=="Normal"){
      bmi[i] <- 0}
  if (bmi[i]=="Normal Weight"){
      bmi[i] <- 0}
  if (bmi[i]=="Overweight"){
      bmi[i] <- 1}
  if (bmi[i]=="Obese"){
      bmi[i] <- 2}
}

bmi <- as.numeric(as.character(bmi))
dati$bmi <- bmi

for (i in 1:374) {
  if (sleepdisorder[i]=="None") {
      sleepdisorder[i] <- 0}
  if (sleepdisorder[i]=="Insomnia") {
      sleepdisorder[i] <- 1}
  if (sleepdisorder[i]=="Sleep Apnea") {
      sleepdisorder[i] <- 2}
}

sleepdisorder <- as.numeric(as.character(sleepdisorder))
dati$sleepdisorder <- sleepdisorder

# rendo binaria sd. Nel nostro caso, il successo e': dormire meno di 7 ore
# mean(sd) = 7.13, arrotondo la media delle ore dormite a 7 
# min(sd) = 5.8, max = 8.5

# modo più facile: sd <- ifelse(sd<=7,1,0)
for (i in 1:374){
  if (sd[i] <= 7){
    sd[i] <- 1}
  if (sd[i] > 7){
    sd[i] <- 0}
}

dati$sd <- sd

#barplot sd
ggplot(data=dati, aes(x=sd)) + 
  geom_bar() +
  geom_text(aes(label = scales::percent(..prop..), group = 1),
            fontface = "bold",colour = "#CE2929", size = 5,stat= "count") +
  theme_minimal()


# MODIFICO IL DATASET, RENDO CATEGORIALI LE VARIABILI RIDEFINITE PRIMA

dati1 <- with(dati, {
  gender <- factor(gender, labels = c("Uomo", "Donna"))
  age.cat <- NA # per inizializzare
  age.cat[age >= 27 & age < 30] <- "27-29 anni"
  age.cat[age >= 30 & age < 40] <- "30-39 anni"
  age.cat[age >= 40 & age < 50] <- "40-49 anni"
  age.cat[age >= 50 & age < 60] <- "50-59 anni"
  occupation <- factor(occupation, labels = c("Ingegnere del Software", "Dottore", 
    "Rappresentante", "Professore", "Infermiere",
    "Ingegnere", "Contabile", "Scienziato", 
    "Avvocato", "Venditore", "Manager"))
  sd <- factor(sd, labels = c(">7 ore", "<7 ore"))
  qos <- factor(qos)
  stress <- factor(stress)
  bmi <- factor(bmi, labels = c("Peso forma","Sovrappeso", "Obeso") )
  sleepdisorder <- factor(sleepdisorder, labels = c("Nessuno", "Insonnia", "Apnea Notturna"))
  data.frame(id, gender, age.cat, occupation, sd, qos, pal, stress, bmi, 
             bpress_syst, bpress_dias, hearthrate, dailysteps, sleepdisorder)
})
View(dati1)
attach(dati1)

#  l'opzione per indicare che la prima categoria e' di riferimento
options(contrasts = c("contr.treatment", "contr.poly"))

## MODELLO CON GENDER COME ESPLICATIVA

mod_gen<-glm(sd ~ gender, family = binomial, data = dati1)
summary(mod_gen)
# la sola variabile genere risulta non significativa. Era auspicabile

# OR per commentare l'effetto gender
exp(cbind(coef(mod_gen), confint.default(mod_gen)))
# la propensione a dormire meno di sette ore per le donne e' quasi 
# uguale a quella degli uomini (1.06, tecnicamente leggermente piu' grande).

pchisq(mod_gen$deviance, mod_gen$df.residual, lower.tail = F)
# non c'e' un buon adattamento del modello, ho un p quasi pari a 0, i dati non sono 
# coerenti.

## AGGIUNGO LA VARIABILE AGE.CAT

mod_gen_age<-glm(sd ~ gender + age.cat,  family = binomial, data = dati1)
summary(mod_gen_age)

# facciamo un test anova per comparare i modelli
anova(mod_gen,mod_gen_age,test="Chisq")
# ho un p piccolo, rifiuto H0, dove sotto H0 ho il primo modello.
# Ho 3 gdl, sotto H0 ho posto i beta delle categorie di age = 0 (modello piu' piccolo).
# accetto il modello migliore.
Anova(mod_gen_age)
# anche dal questo altro test si nota come e' preferibile l'ultimo modello.

# verifichiamo l'adattamento:
pchisq(mod_gen_age$deviance, mod_gen_age$df.residual, lower.tail = F)
# il modello non si adatta bene ai dati.

## MODELLO CON GENDER, AGE E SLEEPDISORDER

mod_gen_sleep_age <- glm(sd ~ gender + age.cat + sleepdisorder, 
                         family = binomial, data = dati1)
summary(mod_gen_sleep_age)

# compariamo modelli
Anova(mod_gen_sleep_age)
# questo ultimo modello e' migliore rispetto al precedente.

# OR per commentare l'effetto delle variabili esplicative
exp(cbind(coef(mod_gen_sleep_age), confint.default(mod_gen_sleep_age)))
# La propensione a dormire meno di 7 ore e' 36 volte superiore per chi 
# soffre di insonnia! (condizionatamente alle altre variabili)

# verifichiamo l'adattamento
pchisq(mod_gen_sleep_age$deviance, mod_gen_sleep_age$df.residual, lower.tail = F)
# Ho buon adattamento

## MODELLO CON GENDER, SLEEPDISORDER, AGE E BMI
mod4 <- glm(sd ~ gender + sleepdisorder + age.cat + bmi, 
            family = binomial, data = dati1)
summary(mod4)
anova(mod_gen_sleep_age,mod4, test="Chisq")
Anova(mod4)

# verifichiamo l'adattamento
pchisq(mod4$deviance, mod4$df.residual, lower.tail = F)
# questo modello si adatta bene ai dati

## MODELLO CON GENDER, SLEEPDISORDER, AGE, BMI, OCCUPATION
mod5 <- glm(sd ~ gender + sleepdisorder + age.cat + bmi +occupation, 
                         family = binomial, data = dati1)
summary(mod5)
# nessun lavoro e' significativo
anova(mod4,mod5, test="Chisq")
Anova(mod5)

# Vediamo che succede se faccio iterazioni con occupation

mod5_int1 <- glm(sd ~ gender + sleepdisorder + age + bmi + occupation + occupation*gender
                  + occupation*age, 
            family = binomial, data = dati1)
summary(mod5_int1)
mod5_int2 <- glm(sd ~ gender + sleepdisorder + age + bmi + occupation + occupation*gender,
                 family = binomial, data = dati1)
summary(mod5_int2)
# occupation ha troppe categorie, l'iterazione mi provoca un effetto confounding.

## MODELLO CON GENDER, SLEEPDISORDER, AGE, BMI, QOS

mod6 <- glm(sd ~ gender + sleepdisorder + age.cat + bmi + qos, 
                             family = binomial, data = dati1)
summary(mod6)

# qos mi genera collinearita' perfetta. 

mod6a <- glm(sd ~ gender + sleepdisorder + age.cat + bmi + dati$qos, 
            family = binomial, data = dati1)
summary(mod6a)

# anche considerando qos come quantitativa continua, ho collinearita' perfetta.

## MODELLO CON GENDER, SLEEPDISORDER, AGE, BMI, STRESS

mod7 <- glm(sd ~ gender + sleepdisorder + age.cat + bmi + stress, 
            family = binomial, data = dati1)
summary(mod7)

# stress mi genera collinearita' perfetta

mod7a <- glm(sd ~ gender + sleepdisorder + age.cat + bmi + dati$stress, 
            family = binomial, data = dati1)
summary(mod7a)

# considerando stress come quantitativa continua, ho significativita' di stress
# ma mi restituisce p alti per le restanti variabili.

hltest(mod7a)
# Statistica bassa, p alto -> ho un buon modello

fit7a <- fitted(mod7a)
fit7a

# le probabilità stimate sono tutte piccole!

## MODELLO CON GENDER, SLEEPDISORDER, AGE, BMI, BPRESS SYST

mod8 <- glm(sd ~ gender + sleepdisorder + age.cat + bmi + bpress_syst, 
            family = binomial, data = dati1)
summary(mod8)

# verifichiamo l'adattamento:

hltest(mod8)
# il modello non si adatta bene.

## MODELLO CON GENDER, SLEEPDISORDER, AGE, BMI, DAILYSTEPS

mod9 <- glm(sd ~ gender + sleepdisorder + age.cat + bmi + dailysteps, 
             family = binomial, data = dati1)
summary(mod9)

hltest(mod9)
# il modello non si adatta bene.

## MODELLO CON GENDER, SLEEPDISORDER, AGE, BMI, HEARTHRATE

mod10 <- glm(sd ~ gender + sleepdisorder + age.cat + bmi + hearthrate, 
            family = binomial, data = dati1)
summary(mod10)

hltest(mod10)
# il modello non si adatta bene.

## MODELLO CON TUTTI I REGRESSORI

mod_full <- glm(sd ~ . , family=binomial, data=dati1)
summary(mod_full)

# Ho perfetta collinearita' tra i regressori! Questo modello e' da scartare.

## MODELLO FINALE

mod_fin <- glm(sd ~ gender + sleepdisorder + age.cat + bmi, 
             family = binomial, data = dati1)
summary(mod_fin)
Anova(mod_fin)
pchisq(mod_fin$deviance, mod_fin$df.residual, lower.tail = F)
fit <- fitted(mod_fin)
fit


## PLOT

# grafico OR:
plot_model(mod_fin, sort.est = TRUE)

# valutiamo l'adattamento tramite analisi dei residui
residualPlots(mod_fin)

# Valutiamo la bonta' della classificazione
pred <- predict(mod_fin,type="response") # sono le y capuccio
pred0.5 <- ifelse(pred<=0.5,0,1) # il cut-off che ho messo e' 0.5
# se la prop e' < 0.5, y cappuccio = 0  altrimenti se p>0.5, y cappuccio=1
xtab <- table(pred0.5, sd)
cm <- caret::confusionMatrix(xtab)
cm
# Ho accuracy 0.84, ho sbagliato in 57 casi. Ho sensibilita' elevata, 
# specificita' di meno. 

# curva di ROC, il potere classificatorio al variare del cut off
pred <- predict(mod_fin,type="response")  
roc(sd,pred)
rocplot<-roc(sd~pred, col = "red", plot=TRUE, print.auc=T, legacy.axes = T,
             main = "ROC curve")
# Ho un AUC del 91.5% !

# previsione

# probabilita' stimata per una uomo obeso che ha tra i 50 e 59 anni e che soffre di insonnia
newdata=data.frame(age.cat="50-59 anni", sleepdisorder="Insonnia",
                   bmi = "Obeso", gender = "Uomo")
prob_newdata <- predict(mod_fin, type="response", newdata)
prob_newdata
