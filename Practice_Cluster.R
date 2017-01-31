library(caret)
library(pls)

clin = readRDS("RA_MAP_CLIN.rds")
test = readRDS("RA_MAP_TEST.rds")

EULAR = na.omit(clin[c(1,65)])

colnames(clin)

eulartest = merge.default(EULAR, test)
eulartest = eulartest[,-c(3)]

red_data = eulartest[,2:ncol(eulartest)]

smp_size <- floor(0.63 * nrow(red_data))

set.seed(123)

train_ind <- sample(seq_len(nrow(red_data)), size = smp_size)

train <- red_data[train_ind,]
test <- red_data[-train_ind,]

ctrl1=trainControl(method="repeatedcv",  number=50, repeats = 1) 

Fit2=train(T6M_SDAIscore~., 
           data=as.data.frame(train), 
           method="pls", 
           trControl=ctrl1, 
           metric="Rsquared",
           tuneLength = 10,
           preProc=c("scale")) 



save(Fit2, file = "Fit2_full.RData")




