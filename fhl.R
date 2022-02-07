library(MASS) ## For step AIC

data <- read.csv('C:/users/james/Desktop/6A/BU 495R/Project/fhl.csv')

data$FIPSStateNumericCode <- as.factor(data$FIPSStateNumericCode)
data$FIPSCountyCode <- as.factor(data$FIPSCountyCode)
data$CoreBasedStatisticalAreaCode <- as.factor(data$CoreBasedStatisticalAreaCode)
data$LoanPurposeType <- as.factor(data$LoanPurposeType)
data$MortgageType <- as.factor(data$MortgageType)
data$MortgageLoanSellerInstType <- as.factor(data$MortgageLoanSellerInstType)
data$BorrowerFirstTimeHomebuyer <- as.factor(data$BorrowerFirstTimeHomebuyer)
data$Borrower1Race1Type <- as.factor(data$Borrower1Race1Type)
data$Borrower2Race1Type <- as.factor(data$Borrower2Race1Type)
data$Borrower1GenderType <- as.factor(data$Borrower1GenderType)
data$Borrower2GenderType <- as.factor(data$Borrower2GenderType)
data$PropertyUsageType <- as.factor(data$PropertyUsageType)
data$EmploymentBorrowerSelfEmployed <- as.factor(data$EmploymentBorrowerSelfEmployed)
data$PropertyType <- as.factor(data$PropertyType)

independent_vars_X <-c('Bank',
                     'FIPSStateNumericCode',
                     'FIPSCountyCode',
                     'CoreBasedStatisticalAreaCode',
                     'CensusTractIdentifier',
                     'CensusTractMinorityRatioPercent',
                     'CensusTractMedFamIncomeAmount',
                     'LocalAreaMedianIncomeAmount',
                     'TotalMonthlyIncomeAmount',
                     'HUDMedianIncomeAmount',
                     'LoanAcquisitionActualUPBAmt',
                     'LTVRatioPercent',
                     'NoteDate',
                     'LoanAcquistionDate',
                     'LoanPurposeType',
                     'MortgageType',
                     'ScheduledTotalPaymentCount',
                     'LoanAmortizationMaxTermMonths',
                     'MortgageLoanSellerInstType',
                     'BorrowerCount',
                     'BorrowerFirstTimeHomebuyer',
                     'Borrower1Race1Type',
                     'Borrower2Race1Type',
                     'Borrower1GenderType',
                     'Borrower2GenderType',
                     'Borrower1AgeAtApplicationYears',
                     'Borrower2AgeAtApplicationYears',
                     'PropertyUsageType',
                     'PropertyUnitCount',
                     'NoteAmount',
                     'HousingExpenseRatioPercent',
                     'TotalDebtExpenseRatioPercent',
                     'Borrower1CreditScoreValue',
                     'Borrower2CreditScoreValue',
                     'PMICoveragePercent',
                     'EmploymentBorrowerSelfEmployed',
                     'PropertyType',
                     'MarginRatePercent',
                     'Borrower1EthnicityType',
                     'Borrower1Race2Type',
                     'Borrower1Race3Type',
                     'Borrower1Race4Type',
                     'Borrower1Race5Type',
                     'Borrower2EthnicityType',
                     'Borrower2Race2Type',
                     'Borrower2Race3Type',
                     'Borrower2Race4Type',
                     'Borrower2Race5Type',
                     'HOEPALoanStatusType')

independent_vars <-c('CensusTractMinorityRatioPercent',
                       'CensusTractMedFamIncomeAmount',
                       'LocalAreaMedianIncomeAmount',
                       'TotalMonthlyIncomeAmount',
                       'LTVRatioPercent',
                       'LoanPurposeType',
                       'MortgageType',
                       'ScheduledTotalPaymentCount',
                       'LoanAmortizationMaxTermMonths',
                       'MortgageLoanSellerInstType',
                       'BorrowerCount',
                       'BorrowerFirstTimeHomebuyer',
                       'Borrower1Race1Type',
                       'Borrower1GenderType',
                       'Borrower1AgeAtApplicationYears',
                       'Borrower2AgeAtApplicationYears',
                       'PropertyUsageType',
                       'PropertyUnitCount',
                       'NoteAmount',
                       'HousingExpenseRatioPercent',
                       'TotalDebtExpenseRatioPercent',
                       'Borrower1CreditScoreValue',
                       'Borrower2CreditScoreValue',
                       'EmploymentBorrowerSelfEmployed')

data<-data[,c('NoteRatePercent',independent_vars)]
model.data.basic<-lm(NoteRatePercent~1,data=data) # Null model
model.data.full<-lm(NoteRatePercent~.,data=data)  # Full model
# Both forward and backward selection using AIC
select.both.AIC<-stepAIC(model.data.basic,scope=formula(model.data.full),direction='both')
select.both.AIC$anova

model<- lm(NoteRatePercent ~ 
             ScheduledTotalPaymentCount + 
             CensusTractMinorityRatioPercent + 
             TotalDebtExpenseRatioPercent + 
             CensusTractMedFamIncomeAmount + 
             LoanPurposeType + 
             Borrower1CreditScoreValue + 
             MortgageType + 
             HousingExpenseRatioPercent + 
             Borrower1GenderType + 
             Borrower1AgeAtApplicationYears + 
             LTVRatioPercent + 
             MortgageLoanSellerInstType + 
             TotalMonthlyIncomeAmount + 
             BorrowerFirstTimeHomebuyer + 
             Borrower1Race1Type + 
             LocalAreaMedianIncomeAmount + 
             EmploymentBorrowerSelfEmployed + 
             Borrower2AgeAtApplicationYears + 
             Borrower2CreditScoreValue + 
             NoteAmount
           ,
           data)

summary(model)

library(car)
vif(model)

data <- read.csv('C:/users/james/Desktop/6A/BU 495R/Project/FHL2.csv')

model2 <- lm (NoteRatePercent ~ Borrower1CreditScoreValue, data)
summary(model2)
