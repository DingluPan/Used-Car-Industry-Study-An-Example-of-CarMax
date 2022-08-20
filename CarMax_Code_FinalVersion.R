################################################## 调用包 ##################################################
#install.packages('terra')
#install.packages('stringr')
library(dplyr)
library(terra)
library(stringr)
#library(plyr)
library(corrplot)

################################################## 设置路径 ##################################################
getwd()
setwd("/Users/Lou/Desktop/USC/22 Summer/MKT 402/Assignments/Group Project/Part IV & Final Code")
getwd()
file_name = 'CarMax_Survey_Data.csv'


################################################## 1. 导入 & 预处理 ##################################################
car_df = read.csv(file_name)  # 导入数据
View(car_df)
length(car_df$Q1)
car_df = car_df[-1, -1:-9]  # 删除无关行和列
obs_num = length(car_df$Q1)
obs_num
car_df = dplyr::rename(car_df, id = UserLanguage, age = Q29, gender = Q30, edu = Q31, job = Q32)  #重命名demographic
car_df$id = seq(1:obs_num)  #创建user id
rownames(car_df) =  seq(1:obs_num)
head(car_df)
str(car_df)
car_df[, c(2:3, 6, 8:23, 28:29)] = lapply(car_df[, c(2:3, 6, 8:23, 28:29)], as.numeric)  # 更改数据结构为numeric
car_df[, c(4:5, 7, 24:27, 30:33)] = lapply(car_df[, c(4:5, 7, 24:27, 30:33)], as.factor)  # 更改数据结构为factor
str(car_df)
head(car_df)

########## 创建应变量（被调查者潜在的在CarMax购买的intention=(Q1/5)*(Q5/5)*100）：分值>36即为潜在客户 (high intention/more likely)
car_df$intention = car_df$Q1/5 * car_df$Q5/5 * 100
head(car_df)
str(car_df$intention)
print(paste("High intention to buy a used car from CarMax:", length(car_df$intention[car_df$intention>36]), 'out of', obs_num))

########## 创建luxury和economy俩dummy变量
car_df$luxury = 0
for (i in 1:obs_num) {
  if (car_df$Q24[i]==1)
    car_df$luxury[i] = 1 else
      car_df$luxury[i] = 0
}

car_df$economy = 0
for (i in 1:obs_num) {
  if (car_df$Q24[i]==2)
    car_df$economy[i] = 1 else
      car_df$economy[i] = 0
}
head(car_df)

##################################################  2. Factor Analysis ##################################################

############# Reg with all questions ---- Reg的结果更好
#car_df = select(car_df, -c(Factor1, Factor2, Factor3, Factor4))
corrplot(cor(car_df[, c(8:23)]), order="hclust")  # 相关关系图
all_eigen = eigen(cor(car_df[, c(8:23)]))  # 求eigen value判断factor个数
all_eigen$value
plot(all_eigen$values, type="b", xlab="Number of components", ylab="Eigen value", 
     main="Eigen value part by number of components")
abline(h = 1, col = 'red')  # 则可以取......个factor
all_fa_num = length(all_eigen$value[all_eigen$value>=1])
all_fa_num
# 读取loading判断各属于哪个factor并命名factor
all_fa = factanal(car_df[, c(8:23)], factors = all_fa_num, scores = "regression", rotation = "varimax")
print(all_fa, cutoff = 0)
print(all_fa, cutoff = 0.144)
# 分类命名后存入主数据框
all_fa_scores = as.data.frame(all_fa$scores)
head(all_fa_scores)
all_fa_scores = dplyr::rename(all_fa_scores, 
                              online_experience = Factor1, dealer_service = Factor2, 
                              guarantee = Factor3, online_efficiency = Factor4)
head(all_fa_scores)
car_df = cbind(car_df, all_fa_scores)
head(car_df)


################################################## 3. Hypothesis test ##################################################
car_df_new = car_df[c('intention', 'gender', 'luxury', 'economy', 
                      'online_experience', 'dealer_service', 'guarantee', 'online_efficiency')]  # 根据有几个factor调整
head(car_df_new)
table(car_df_new$gender)
#which(car_df_new$gender == 2)
car_df_new = car_df_new[-which(car_df_new$gender == 2), ]
length(car_df_new$gender)

### 1) t-test看男女的intention有没有差别
t.test(intention~gender, data = car_df_new, alternative = "two.sided")
t.test(intention~gender, data = car_df_new, alternative = "less")  # 则男性在CarMax购买二手车的intention比女性高

### 2) t-test看购买luxury与否的intention有无差别
t.test(intention~luxury, data = car_df_new, alternative = "two.sided")  # 则买不买luxury和在CarMax买二手车的intention无关

### 3) t-test看购买economy与否的intention有无差别
t.test(intention~economy, data = car_df_new, alternative = "two.sided")  # 则买不买economy和在CarMax买二手车的intention无关


### 4) t-test看男女对于几个factors有无差别
for (i in c('online_experience', 'dealer_service', 'guarantee', 'online_efficiency')){
  print("======================================")
  print(paste("T-test of", i, "by Gender"))
  print(t.test(car_df_new[[i]]~car_df_new[['gender']],  alternative = "two.sided"))
}



################################################## 4. Linear Reg ##################################################

########## 1) main reg: 以intention为应变量reg on factor scores
head(car_df)
main_ols_reg = lm(intention~online_experience+dealer_service+guarantee+online_efficiency, data = car_df)  # 根据factor数量更改回归模型 
summary(main_ols_reg)

########## 2) Linear Reg加入依据感兴趣的因素设置的dummy
## 针对luxury和dealer_service，看是否有significant luxury-preference diff. between the effectiveness of dealer service on the intention value
## model中加入luxury选项作为dummy来和4个factor来interact (发现和dealer_service的interaction部分是显著的，其他则不显著，即无sig. diff. between ...)
main_ols_reg_luxury = lm(intention~online_experience+dealer_service+guarantee+online_efficiency
                         +online_experience:luxury+dealer_service:luxury+guarantee:luxury+online_efficiency:luxury, data = car_df)
summary(main_ols_reg)
summary(main_ols_reg_luxury)


########## 3) 由Hypothesis检验只性别影响intention，加入gender变量看影响程度 (基于car_df_new这个dataset，不包含gender选项为2的人) 
head(car_df_new)
dim(car_df)
dim(car_df_new)
main_ols_reg_gender = lm(intention~online_experience+dealer_service+guarantee+online_efficiency+gender, data = car_df_new)
summary(main_ols_reg_gender)  # 则男性在CarMax买二手车的intention比女性更高


################################################## 5. Preference部分展示 ##################################################
body_type = table(car_df$Q23)
body_type = as.data.frame(body_type)
body_type$Var1 = c('SUVs', 'Trucks', 'Sedans', 'Coupes', 'Compact')
body_type = dplyr::rename(body_type, Type = Var1)
body_type

car_model = table(car_df$Q24)
car_model = as.data.frame(car_model)
car_model$Var1 = c('Luxury', 'Economy', 'Electric', 'Hybrid', 'Sports')
car_model = dplyr::rename(car_model, Model = Var1)
car_model

brand_region = table(car_df$Q25)
brand_region = as.data.frame(brand_region)
brand_region$Var1 = c('Domestic', 'Japanese', 'Korean', 'European', 'Others')
brand_region = dplyr::rename(brand_region, Region = Var1)
brand_region

#### 制作Q26的table
aspect_preference = data.frame(Price = 0, Exterior = 0, Interior = 0, Space = 0,
                               FuelEconomy = 0, EnginePerformance = 0, Mileage = 0, Safety = 0, DamageReport = 0,
                               ServiceRecord = 0, ProductionYear = 0, PastUsage = 0, DealerService = 0, Brand = 0)
rownames(aspect_preference) = 'Frequency'
aspect_preference

for (i in 1:obs_num) {
  tmp = as.factor(unlist(strsplit(as.character(car_df$Q26[i]), ",")))
  for (k in 1:14) {
    if (k %in% tmp) {
      aspect_preference[[k]] = aspect_preference[[k]] + 1
    } else {
      aspect_preference[[k]] = aspect_preference[[k]]
    }
  }
}
aspect_preference

########## Preference部分的table展示以及barplot展示 
########## & Q3(How will you get the used car info)和Q6(How do you know about CarMax)的table和barplot
body_type
barplot(table(car_df$Q23), xlab = 'Body Type', ylab = 'Frequency', main = 'Preference of Used-Car Body Type', 
        axes = T, names.arg = body_type$Type, beside = F, col = 'light blue', horiz = F)

car_model
barplot(table(car_df$Q24), xlab = 'Model Type', ylab = 'Frequency', main = 'Preference of Used-Car Model Type', 
        axes = T, names.arg = car_model$Model, beside = F, col = 'light blue', horiz = F)

brand_region
barplot(table(car_df$Q25), xlab = 'Brand Region', ylab = 'Frequency', main = 'Preference of Used-Car Brand Region', 
        axes = T, names.arg = brand_region$Region, beside = F, col = 'light blue', horiz = F)

aspect_preference
barplot(as.matrix(aspect_preference), xlab = 'Aspects', ylab = 'Frequency', main = 'Preference of Used-Car Aspects', 
        axes = T, names.arg = NULL, beside = F, col = 'light blue', horiz = F)

Q3_table = table(car_df$Q3)
Q3_table
barplot(Q3_table, xlab = 'Source', ylab = 'Frequency', main = 'How people get the used car info', 
        axes = T, names.arg = c('Newspaper', 'TV', 'Website', 'Social Media', 'Friends'), beside = F, col = 'light blue', horiz = F)

Q6_table = table(car_df$Q6)
Q6_table
barplot(Q6_table, xlab = 'Source', ylab = 'Frequency', main = 'How people know about CarMax', 
        axes = T, names.arg = c('Ads', 'Big-data', 'Email', 'Friends', 'Dont Know CarMax'), beside = F, col = 'light blue', horiz = F)

