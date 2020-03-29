library(data.table)
library(readr)
library(highcharter)
library(tidyverse)
library(scorecard)
library(inspectdf)
library(h2o)
credit <- read_csv('C:/Users/credit.csv')
colnames(credit)
summary(credit)
glimpse(credit)
hchart(cor(credit %>% 
             mutate_if(is.character,as.factor) %>% 
             mutate_if(is.factor,as.numeric)) %>% 
         round(.,2),label = T)

#----------------BINNING ----
credit <- credit %>% mutate(creditability = ifelse(as.numeric(as.factor(creditability))>1,1,0))  
iv <- 
  iv(credit, y = 'creditability') %>%as_tibble() %>%
  mutate( info_value = round(info_value, 3) ) %>%
  arrange( desc(info_value) )

# Exclude not important variables ---
ivars <- iv %>% 
  filter(info_value>0.02) %>% 
  select(variable) %>% 
  .[[1]] 

credit.iv <- credit %>% 
  select(ivars,creditability)

credit.iv %>% dim()


# breaking dt into train and test ---
dt_list <- split_df(credit.iv, "creditability", ratio = 0.8, seed=123)
train <- dt_list$train 
test <- dt_list$test


# woe binning ---
bins <- credit.iv %>% woebin("creditability")

# converting train and test into woe values
train_woe <- train %>% woebin_ply(bins) 
test_woe <- test %>% woebin_ply(bins)


names <- train_woe %>% names()                     
names <- gsub("_woe","",names)                     
names(train_woe) <- names                          ; names(test_woe) <- names
train_woe %>% inspect_na(show_plot = F) %>% head(3); test_woe %>% inspect_na(show_plot = F) %>% head(3)


#---------------- Logistic Linear Regression Diagnostics ----
outcome <- 'creditability'
features <- train_woe %>% select(-creditability) %>% names()


f <- as.formula(paste(outcome, paste(features, collapse = " + "), sep = " ~ "))
glm <- glm(f, data = train_woe)
glm %>% summary()

# Select a formula-based model by AIC
step <- glm %>% step()
step$call # copy past

glm2 <- glm(formula = creditability ~ status.of.existing.checking.account + 
              duration.in.month + credit.history + age.in.years + savings.account.and.bonds + 
              purpose + property + present.employment.since + other.installment.plans + 
              credit.amount + other.debtors.or.guarantors + installment.rate.in.percentage.of.disposable.income, 
            data = train_woe)

glm2 %>% summary()

step <- glm2 %>% step()
step$call

glm3 <- glm (formula = creditability ~ status.of.existing.checking.account + 
              duration.in.month + credit.history + age.in.years + savings.account.and.bonds + 
              purpose + property + present.employment.since + other.installment.plans + 
              credit.amount + installment.rate.in.percentage.of.disposable.income, 
            data = train_woe)
glm3 %>% summary()



glm3 %>% 
  coefficients() %>% 
  as.data.frame() %>%
  rownames() %>% 
  .[-1] %>% 
  as.factor() -> all.vars
all.vars %>% length()

glm3 %>% vif() %>% arrange(desc(gvif)) %>% 
  pull(variable) -> selected


# Multicollinrarity
hchart(cor(
  train_woe %>% 
    select(creditability,selected)) %>%
    round(.,2),label = T)


#-------------------Modeling with GLM ----
h2o.init()
train_h2o <- as.h2o(train_woe %>% select(creditability,selected)) 
test_h2o <- as.h2o(test_woe %>% select(creditability,selected))

outcome <- "creditability"
features <- train_woe %>% select(selected) %>% 
  names()

model <- h2o.glm(
  x = features,
  y = outcome,
  training_frame = train_h2o,
  family = "binomial", 
  seed = 123,
  nfolds = 10, 
  remove_collinear_columns = T,
  max_runtime_secs = 180
)

model %>% h2o.auc() %>% round(2)

model %>% h2o.performance(newdata = test_h2o) %>% h2o.auc() %>% round(2)

model %>% h2o.std_coef_plot()

model@model$coefficients %>% as.data.frame() %>% 
  mutate(names = rownames(model@model$coefficients %>% as.data.frame())) %>%
  `colnames<-`(c('coefficients','names')) %>% 
  select(names,coefficients) %>%
  filter(coefficients != 0) %>%
  arrange(desc(coefficients))

h2o.varimp(model) %>% as.data.frame() %>% 
  pull(percentage) %>% sum()

h2o.varimp(model) %>% as.data.frame() %>% .[.$percentage>0,] %>%
  pull(variable) -> imp.vars
imp.vars %>% length()

h2o.varimp(model) %>% as.data.frame() %>% .[.$percentage != 0,] %>%
  select(variable, percentage) %>%
  hchart("pie", hcaes(x = variable, y = percentage)) %>%
  hc_colors(colors = 'orange') %>%
  hc_xAxis(visible=T) %>%
  hc_yAxis(visible=T)

model %>% h2o.performance(newdata = test_h2o) %>% 
  h2o.find_threshold_by_max_metric('f1')
pred <- model %>% h2o.predict(newdata = test_h2o) %>% as.data.frame()
pred %>% select(predict) %>% table()


#########   model auc = 0.79