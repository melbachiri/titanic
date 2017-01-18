library(randomForest)
setwd("C:\\Dev\\DataScience\\titanic")

# Ne pas convertir les chaines de caractères en factors
train <- read.csv("train.csv", stringsAsFactors = FALSE) # survived
test <- read.csv("test.csv", stringsAsFactors = FALSE)   # non survived

# Median of Age
median(train, na.rm = TRUE)
median(test, na.rm = TRUE)

train$isTrainSet <- TRUE
test$isTrainSet <- FALSE

test$Survived <- NA

# combine the train/test
full <- rbind(train,test)

# rbind is done correctly?
table(full$isTrainSet)

# cleaning

# Port d'embarquement
# Deux personnes sans port d'embarcation
# Répartir de façon intelligemment
table(full$Embarked) 

# La valeur de la colonne Embarked 
# Afficher les valeurs de cette colonne uniquement
# Remplacer le port d'embarquation duquel a été embarqué le plus grand nombre
full[full$Embarked == '', "Embarked"] <- 'S'

# Age

table(is.na(full$Age))
median.age <- median(full$Age, na.rm = TRUE)
full[is.na(full$Age)==TRUE, "Age"] <- median.age*(2/3)
table(is.na(full$Age))


# Fare

table(is.na(full$Fare))
median.fare <- median(full$Fare, na.rm = TRUE)
full[is.na(full$Fare)==TRUE, "Fare"] <- median.fare
table(is.na(full$Fare))

# Categorial casting
# Do not cast Survived because you will lose the binary classification (3)

full$Pclass <- as.factor(full$Pclass)
full$Sex <- as.factor(full$Sex)
full$Embarked <- as.factor(full$Embarked)



# Séparation des deux ensembles

train = full[full$isTrainSet==TRUE,]
test = full[full$isTrainSet==FALSE,]



# Cast the Survived variable (0/1)
train$Survived <- as.factor(train$Survived)


survived.eqn <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"

survived.formula <- as.formula(survived.eqn)

#install.packages("randomForest")


model1 <- randomForest(survived.formula, data = train)

model2 <- glm(survived.formula,data=train, family = binomial())

features.eqn <-"Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived1 <- predict(model1,newdata=test)
Survived2 <- predict(model2,newdata=test, type="response")
Survived2[Survived2>0.50] <- 1
Survived2[Survived2<=0.50] <- 0


table(Survived1 == Survived2)


PassengerId <- test$PassengerId
output.df1 <- as.data.frame(PassengerId)
output.df1$Survived <- Survived1

output.df2 <- as.data.frame(PassengerId)
output.df2$Survived <- Survived2

write.csv(output.df1, file="kaggle_Survived1.csv", row.names = FALSE)
write.csv(output.df2, file="kaggle_Survived2.csv", row.names = FALSE)



