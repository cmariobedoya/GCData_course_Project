
#ARCHIVO:run_analysis.r
#AUTOR:Carlos Mario Bedoya M
#that does the following.
#STEP 1:Merges the training and the test sets to create one data set.
#STEP 2:Extracts only the measurements on the mean and standard deviation for each measurement.
#STEP 3:Uses descriptive activity names to name the activities in the data set
#STEP 4:Appropriately labels the data set with descriptive variable names.
#STEP 5:From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(dplyr)
library(stringr)

#Establecemos nuestro directorio de trabajo
setwd("D:/SpecializationR")


#LEEMOS NUESTROS DATOS 

# Leemos datos de train
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")

# Leemos nuestros datos de test
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")

#leemos características y actividades
features <- read.table("UCI HAR Dataset/features.txt", as.is = TRUE)
activities <- read.table("UCI HAR Dataset/activity_labels.txt")
colnames(activities) <- c("activityId", "activityLabel")



# Step 1 

# concatenatenamos los datasets de test y de train

dataset_actividades <- rbind(
  
  cbind(subject_train, x_train, y_train),
  
  cbind(subject_test, X_test, y_test)
  
)


# Nombres de las columnas

colnames(dataset_actividades) <- c("subject", features[, 2], "activity")


# Step 2 -

# Extraemos sólo las columnas que necesitamos haciendo una extracción por el nomre

columnas <- grepl("subject|activity|mean|std", colnames(dataset_actividades))

dataset_actividades <- dataset_actividades[, columnas]



# Step 3 
# replace cada actividad con su nombre correspondiente
dataset_actividades$activity <- factor(dataset_actividades$activity,levels = activities[, 1], labels = activities[, 2])


# Step 4 
# Obtenemos el nombre de las columnas y lego realizamos limpieza
dataset_actividadesCols <- colnames(dataset_actividades)
# removemos caracteres especiales y corrige los nombres
gsub(" ", "", dataset_actividadesCols, fixed = TRUE)
str_replace_all(dataset_actividadesCols, "[[:punct:]]", " ")
dataset_actividadesCols<-str_replace_all(dataset_actividadesCols, "[^[:alnum:]]", " ")
colnames(dataset_actividades) <- dataset_actividadesCols




# Step 5 
#agrupamos por tema y actividad
dataset_actividades_sum <- dataset_actividades %>%
  
  group_by(subject, activity) %>%
  
  summarise_each(funs(mean))



# GENERAMOS NUESTRO ARCHIVO FINAL 

write.table(dataset_actividades_sum, "d:/tidy_data.txt", row.names = FALSE,
            
            quote = FALSE)