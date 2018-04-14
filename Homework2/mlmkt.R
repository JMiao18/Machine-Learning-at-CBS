mlmkt = read.csv(file = "M:/A Master of Science in Marketing Sciences/MS Machine Learning/Homework2/bank-additional-full.csv",sep=";",header = TRUE)

## Remove the variables duration, date_of_week, month and nr.employed and explain why removing these variables makes sense. Print summary() for your data and brie y discuss the imported dataset.

drops <- c("month","day_of_week","duration","nr.employed")
mlmkt = mlmkt[ , !(names(mlmkt) %in% drops)]

summary(mlmkt)

x <- model.matrix(y~.,mlmkt)[,-1]
y <- mlmkt$y
## Remove any rows with "unknown" observations.

mlmkt$default[mlmkt$default == "unknown"] = NA
mlmkt$housing[mlmkt$housing == "unknown"] = NA
mlmkt$loan[mlmkt$loan == "unknown"] = NA
mlmkt$job[mlmkt$job == "unknown"] = NA
mlmkt$marital[mlmkt$marital == "unknown"] = NA
mlmkt$loan[mlmkt$loan == "unknown"] = NA
mlmkt$education[mlmkt$education == "unknown"] = NA

mlmkt = na.omit(mlmkt)

# Recall that tree methods are not great at handling multiple unordered categorical predictors; hence change the "job" attribute to have only two values (employed and unemployed) and marital to have only ("single" and "married"). Similarly change the "education variable" to a numeric ordered dummy variable taking 6 increasing values (hint: use as.numeric() to ensure this is not a character in R).

mlmkt$job = 1 - (mlmkt$job == "unemployed")
mlmkt$marital = mlmkt$marital == "married"
mlmkt$marital = as.numeric(mlmkt$marital)

edu = as.character(mlmkt$education)

edu[edu == "illiterate"] = 0
edu[edu == "basic.4y"] = 1
edu[edu == "basic.6y"] = 2
edu[edu == "basic.9y"] = 3
edu[edu == "high.school"] = 4
edu[edu == "professional.course"] = 5
edu[edu == "university.degree"] = 6
edu = as.numeric(edu)
mlmkt$education = edu

## Now split the sample into two equal sub-samples, for training and testing. Use set.seed(1) and the sample() command like in the the R Lab to create a training set and a test set. Then recall that the tree() function in R takes either numeric or factor inputs; hence transform any character variables in the data frame into factors (use the as.factor() command to do this).

set.seed(1)
train <- sample( 1: nrow(mlmkt),nrow(mlmkt)/2)
test <- -train


