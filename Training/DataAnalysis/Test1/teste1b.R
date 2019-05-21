########################################
# Teste 1B          
# Nome(s): Felipe de Melo Furtado
########################################

#ATENÇÃO: Você precisa fazer o download do arquivo chustomer_churn.csv e
#           deixá-lo na mesma pasta que o arquivo teste1b.R
#         Depois, Fornecer o caminho completo até o arquivo csv.
#         Exemplos:
#              -Windows:
#                  "C:/Users/Andre/Downloads/customer_churn.csv"
#              -Ubuntu
#                  "/home/andre/Downloads/customer_churn.csv"
#              -Mac OS
#                  "/Users/andre/Downloads/customer_churn.csv"

customer_churn = read.table("C:/Curso/Analise de dados/customer_churn.csv", sep=",", header = TRUE, stringsAsFactors= FALSE)

########################################
# Item 1 (0.5 ponto)
########################################
customer_churn <- customer_churn[!duplicated(customer_churn$customerID),]
########################################
# Item 2 (0.5 ponto)
########################################
Dependents <- as.logical((as.numeric(factor(customer_churn$Dependents))) - 1)
Partner <- as.logical((as.numeric(factor(customer_churn$Partner))) - 1)
Churn <- as.logical((as.numeric(factor(customer_churn$Churn))) - 1)
customer_churn$Partner <- Partner
customer_churn$Dependents <- Dependents
customer_churn$Churn <- Churn
########################################
# Item 3 (0.5 ponto)
########################################
sort(customer_churn$tenure, decreasing = TRUE)
dmax_tenure <- customer_churn[(customer_churn$tenure == 72),]
max_tenure <- dmax_tenure$customerID

########################################
# Item 4 (1.0 ponto)
########################################
a <- customer_churn[customer_churn$MonthlyCharges >= 50,]
max_tenure_50 <- a[a$tenure == 72,]$customerID

########################################
# Item 5 (1.0 ponto)
########################################
b <- customer_churn[customer_churn$Contract == "Month-to-month",]
b[b$tenure == 1,]
min_tenure_mtm <- b[b$tenure == 1,]$customerID

########################################
# Item 6a (1.0 ponto)
########################################

churnFalse <- customer_churn[customer_churn$Churn == FALSE,]
mom <- churnFalse[churnFalse$Contract == "Month-to-month",]
total_mtm <- sum(mom$MonthlyCharges)
oney <- churnFalse[churnFalse$Contract == "One year",]
total_year <- sum(oney$MonthlyCharges)
twoy <- churnFalse[churnFalse$Contract == "Two year",]
total_two_year <- sum(twoy$MonthlyCharges)

########################################
# Item 6b (0.5 ponto)
########################################

regular_customers <- length(customer_churn[churnFalse$tenure > 12,]$customerID)

########################################
# Item 7a (0.5 ponto)
########################################
churnTrue <- customer_churn[customer_churn$Churn == TRUE,]
customers_with_dependents <- length(churnTrue[churnTrue$Partner == TRUE & churnTrue$Dependents == TRUE,]$customerID)

########################################
# Item 7b (0.5 ponto)
########################################

customers_mtm <- length(churnTrue[churnTrue$Contract == "Month-to-month",]$customerID)
customers_year <- length(churnTrue[churnTrue$Contract == "One year",]$customerID)
customers_two_year <- length(churnTrue[churnTrue$Contract == "Two year",]$customerID)

########################################
# Item 7c (0.5 ponto)
########################################

customers_two_years <- length(churnTrue[churnTrue$tenure > 24,]$customerID)

########################################
# Item 7d (0.5 ponto)
########################################

accumulated_discount <- (churnTrue$MonthlyCharges * 0.05)*12
