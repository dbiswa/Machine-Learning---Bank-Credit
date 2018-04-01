# Get data from UCI machine learning data repository
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data"
download.file(url, "credit.txt", method = "curl")

# create a vector to name the variables (based on attribute information provided)
col_names <- c("checking.balance", "months.loan.duration", "credit.history", "purpose", "credit.amount", "savings.balance", "present.employment.status", "installment.rate.in.percentage.of.disposable.income", "gender.and.marital.status", "other.debtors.guarantor", "current.residencetime", "property.type", "age", "other.installment.plans", "housing", "number.of.existing.credits.at.this.bank", "job.type", "number.of.dependents", "telephone", "foreign.worker", "default")

# import data using read.table
credit <- read.table("credit.txt", header = FALSE, sep = "", col.names = col_names, stringsAsFactors = FALSE)

# examine the structure of the credit data frame
str(credit)

# recode symbolic/categorical variables as factors (based on information available of documentation)
credit$checking.balance <- factor(credit$checking.balance, levels = c("A11", "A12", "A13", "A14"),labels = c("< 0 DM", "0<=...< 200 DM", " > 200 DM", "no checking account"))
credit$credit.history <- factor(credit$credit.history, levels = c("A30", "A31", "A32", "A33", "A34"),labels = c("no credits taken/all credits paid", "all credits paid at this bank", "existing credits paid duly till now", "delay in paying off in past", "critical account/other credits existing (not at this bank)"))

credit$purpose <- factor(credit$purpose, levels = c("A40", "A41", "A42", "A43", "A44", "A45", "A46", "A47", "A48", "A49", "A410"), labels = c("car(new)", "car(old)", "furniture/equipment","radio/television", "domestic appliance", "repairs", "education", "vacation", "retraining", "business", "others"))
credit$savings.balance <- factor(credit$savings.balance, levels = c("A61", "A62", "A63", "A64", "A65"), labels = c("< 100 DM", "100 <= ... < 500 DM", "500 <= ... < 1000 DM", "> 1000 DM", "unknown/no savings account"))

credit$present.employment.status <- factor(credit$present.employment.status, levels = c("A71", "A72", "A73", "A74", "A75"), labels = c("unemployed", "< 1 year", "1 <=...< 4 year", "4 <=...< 7 year", "> 7 year"))
credit$gender.and.marital.status <- factor(credit$gender.and.marital.status, levels = c("A91", "A92", "A93", "A94", "A95"), labels = c("male.divorced/separated", "female.divorced/separated/married", "male.single", "male.married/widowed", "female.single"))
credit$other.debtors.guarantor <- factor(credit$other.debtors.guarantor, levels = c("A101", "A102", "A103"), labels = c("none", "co-applicant", "guarantor"))
credit$property.type <- factor(credit$property.type, levels = c("A121", "A122", "A123", "A124"), labels = c("real estate", "society savings agreement/life insurance", "car/other", "unknown/no property"))
credit$other.installment.plans <- factor(credit$other.installment.plans, levels = c("A141", "A142", "A143"), labels = c("bank", "stores", "none"))
credit$housing <- factor(credit$housing, levels = c("A151", "A152", "A153"), labels = c("rent", "own", "for free"))
credit$job.type <- factor(credit$job.type, levels = c("A171", "A172", "A173", "A174"), labels = c("unemployed/nonresident", "unskilled/resident", "skilled employee","management/self-employed/highly-qualified-employee/officier"))
credit$telephone <- factor(credit$telephone, levels = c("A191", "A192"), labels = c("none", "yes"))
credit$foreign.worker <- factor(credit$foreign.worker, levels = c("A201", "A202"), labels = c("yes", "no"))

# recode "default" (outcome) as a factor with two levels: no (able to meet agreed payment terms), yes (unable to meet agreed payment terms and went to default)
credit$default <- factor(credit$default, levels = c("1", "2"), labels = c("no", "yes")) # 30 % went into default

# check the structure of credit dataframe to ensure variables are labelled appropriately
str(credit)

# save the dataframe for future use
write.csv(credit, file = "credit.csv")

#**************Check for missing values********************
# domentation informs that there are no missing values in dataset; confirm the same

# write a function to check missing values in each column of a dataframe
checkEachcolumn = function(df,column) {
  Data = df[[column]]
  Missing = max(sum(is.na(Data)|is.nan(Data)|Data==''),0)
  if (class(Data) == 'numeric' | class(Data) == 'integer'){
    list('col' = column,'class' = class(Data), 'num' = length(Data) - Missing, 'Missing' = Missing)
  } else{
    list('col' = column,'class' = class(Data), 'num' = length(Data) - Missing, 'Missing' = Missing)
  }
}  

# write a function to check all columns for missing values
checkAllcolumns = function(df){
  DF = data.frame()
  for (column in names(df)){
    DF = rbind(DF,as.data.frame(checkEachcolumn(df=df, column=column)))
  }
  DF
}

# use checkAllcolumns() to summarise missing values in credit dataframe
checkAllcolumns(credit) 
# the result summary shows that there are no missing values in the dataframe aligned with the documentation

