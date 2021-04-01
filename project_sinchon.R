### 1. 데이터 불러오기 및 처리
# 데이터 불러오기
sinchon = read.csv("D:/R/project/신촌.csv")
head(sinchon)

# 인덱스 없애기
sinchon <- sinchon[, -1]
head(sinchon)

# column명 바꾸기
names(sinchon) <- c('날짜', '역명', '요일', '승하차전체')
head(sinchon)

# total 숫자로 바꾸기
sinchon$승하차전체 <- gsub(",", "", as.character(sinchon$승하차전체)) 
sinchon$승하차전체 <- as.numeric(sinchon$승하차전체)
str(sinchon)
boxplot(승하차전체 ~ 요일, data =sinchon)

#----------------------------------------------------------------------------------------
### 2. 요일 별로 평균 내기 
sinchon_mean = aggregate(승하차전체~요일, sinchon, mean)
sinchon_mean = as.data.frame(sinchon_mean)
str(sinchon_mean)
sinchon_mean

plot(승하차전체~요일, sinchon_mean)
#----------------------------------------------------------------------------------------
### 3. 평일만 남기기
sinchon_weekday <- subset(sinchon, 요일 %in% c("월", "화", "수", "목", "금"))
head(sinchon_weekday)
str(sinchon_weekday)

# (평일 중)요일 별로 평균 내기
weekday_mean = aggregate(승하차전체~요일, sinchon_weekday, mean)
weekday_mean = as.data.frame(weekday_mean)
str(weekday_mean)
weekday_mean


plot(승하차전체~요일, weekday_mean)

### column명을 영어로 바꾸기
names(sinchon_weekday)[3] <- 'weekday'
names(sinchon_weekday)[4] <- 'on_off_total'
head(sinchon_weekday)

# 월->1, 화->2, 수->3, 목->4, 금->5
sinchon_weekday$weekday <- as.character(sinchon_weekday$weekday)
str(sinchon_weekday)
sinchon_weekday$weekday[sinchon_weekday$weekday == '월'] <- 1
sinchon_weekday$weekday[sinchon_weekday$weekday == '화'] <- 2
# sinchon_weekday[sinchon_weekday$weekday == "화", 'weekday'] <- 2
sinchon_weekday$weekday[sinchon_weekday$weekday == '수'] <- 3
sinchon_weekday$weekday[sinchon_weekday$weekday == '목'] <- 4
sinchon_weekday$weekday[sinchon_weekday$weekday == '금'] <- 5
head(sinchon_weekday)

sinchon_weekday$weekday <- as.factor(sinchon_weekday$weekday)
str(sinchon_weekday)

# 이상치 제거하기####
del_outlier <- function(data, value) {
  df <- data[data$weekday == value,]
  Q1 <- quantile(df$on_off_total, 0.25)
  Q3 <- quantile(df$on_off_total, 0.75)
  LC = Q1 - 1.5 * (Q3 - Q1) # 아래 울타리
  UC = Q3 + 1.5 * (Q3 - Q1) # 위 울타리
  
  df2 = subset(df, on_off_total > LC & on_off_total < UC)
  return(df2)
}

sinchon_weekday <- rbind(del_outlier(sinchon_weekday, '1'), del_outlier(sinchon_weekday, '2'), del_outlier(sinchon_weekday, '3')
      ,del_outlier(sinchon_weekday, '4'), del_outlier(sinchon_weekday, '5'))
head(sinchon_weekday)
tail(sinchon_weekday)
boxplot(on_off_total ~ weekday, sinchon_weekday)
####

head(sinchon_weekday)
# write.csv(sinchon_weekday, file = 'C:/download/sinchon_weekday.csv')

library('rjags')
#-------------------------------------------------------------------------------------
### 4. 모델 만들기
## model1: pois - uniform
mod_string = " model {
  for (i in 1:length(on_off_total)) {
    on_off_total[i] ~ dpois(lam[weekday[i]])
  }


  for (j in 1:max(weekday)) {
    lam[j] ~ dunif(alpha, beta)
  }

  alpha = 0
  beta = 1e5
  
} "

## model2: pois - uniform - uniform: herarchical model
mod_string = " model {
  for (i in 1:length(on_off_total)) {
    on_off_total[i] ~ dpois(lam[weekday[i]])
  }

  for (j in 1:max(weekday)) {
    lam[j] ~ dunif(alpha, beta)
  }

  mu ~ dunif(0, 1e6) # global mean
  sig ~ dunif(0, 1e6)

  alpha = (2 * mu - (12 * sig^2)^(1/2) ) / 2
  beta = (2 * mu + (12 * sig^2)^(1/2) ) / 2
  
} "


# model3(gamma)
mod_string = " model {
  for (i in 1:length(on_off_total)) {
    on_off_total[i] ~ dpois(lam[weekday[i]])
  }

  for (j in 1:max(weekday)) {
    lam[j] ~ dgamma(alpha, beta)
  }

  mu ~ dgamma(20.0, 0.001)
  sig ~ dexp(0.0001)

  alpha = mu^2 / sig^2
  beta = mu / sig^2
} "

# model6(exp)
mod_string = " model { 
  for (i in 1:length(total)) {
    total[i] ~ dpois(lam[day[i]]) 
  } 

  for (j in 1:max(day)) { 
    lam[j] ~ dexp(alpha) 
  } 

  alpha = 1/mu

  mu ~ dexp(1/10000) 

} "

set.seed(113)

data_jags = as.list(sinchon_weekday)

params = c("mu", "sig", "lam")

mod = jags.model(textConnection(mod_string), data = data_jags, n.chains = 3)
update(mod, 1e3)

mod_sim = coda.samples(model = mod, variable.names = params, n.iter = 5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

plot(mod_sim, ask = TRUE)

dic = dic.samples(mod, n.iter = 1e3)
dic
## convergence diagnostics

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

# 
# pm_params = colMeans(mod_csim)
# pm_params
pmed_coef = apply(mod_csim, 2, median)
pmed_coef

summary(mod_csim)
plot(mod_csim)
############################################################################
### yhat 구하기
# (1) 단순하게(variance도 확인해보기)
sinchon_weekday_sorted = sinchon_weekday[c(order(sinchon_weekday$weekday)), ]
head(sinchon_weekday_sorted)

num_mon = length(which(sinchon_weekday_sorted$weekday == 1)) #157 -> 148
num_tue = length(which(sinchon_weekday_sorted$weekday == 2)) #157 -> 147
num_wed = length(which(sinchon_weekday_sorted$weekday == 3)) #156 -> 146
num_thu = length(which(sinchon_weekday_sorted$weekday == 4)) #156 -> 149
num_fri = length(which(sinchon_weekday_sorted$weekday == 5)) #156 -> 149

yhat = rep(pmed_coef[3:7], c(num_mon, num_tue, num_wed, num_thu, num_fri)) # 간단하게 하기 위해서 이렇게 함.
yhat # 값 오직 5개

resid = sinchon_weekday_sorted$on_off_total - yhat
plot(resid) # 패턴 안 보이면 좋은 것.
plot(jitter(yhat), resid)

var(resid[yhat < 17500]) # 4169093 -> 971945.6
var(resid[yhat > 19500]) # 6485370 -> 1750258
# 말도 안 되는 분산이 나왔다: outlier를 제거해야겠다.
# outlier를 제거해도 분산이 여전히 너무 크다ㅠㅠ

# (2) prior predictive: location을 구분하지 않은 data만 구할 수 있을 듯하다.
n_sim = 10000

# alpha_pri, beta_pri
alpha_pri = runif(n_sim, 0, 1000)
beta_pri = runif(n_sim, 6000, 1e5)

mu_pri = (alpha_pri + beta_pri) / 2
sig_pri = (beta_pri - alpha_pri)^2 / 12

summary(mu_pri)
summary(sig_pri)

# for a prior predictive reconstruction of the original data set
lam_pri = runif(n_sim, alpha_pri, beta_pri)

summary(lam_pri)
plot(lam_pri)

y_pri = rpois(n_sim, lambda = lam_pri)
y_pri
hist(y_pri)
# prior임의로 정한 것이니 unrealistic predictive distribution이 나올 것 같다.
# prior predictive distribution은 굳이 필요가 없을 듯.

# (3) posterior predictive
### # posterior predictive simulation
# Now using the posterior distribution of mu and sigma
# we can draw Monte Carlo's samples for the predictive distribution of lambda.
n_sim = nrow(mod_csim)


# posterior samples of mu and sigma를 posterior samples of alpha, beta로 만들기
# post_alpha = (2 * mod_csim[, "mu"] - (12 * mod_csim[, "sig"]^2)^(1/2) ) / 2
# post_beta = (2 * mod_csim[, "mu"] + (12 * mod_csim[, "sig"]^2)^(1/2) ) / 2
post_alpha = mod_csim[, "alpha"]
post_beta = mod_csim[, "beta"]

# new location에서의 lambda
lam_pred = runif(n_sim, post_alpha, post_beta)
hist(lam_pred) # what lambda might look like if we were to draw a new lambda from a new location
mean(lam_pred > 18000) # new_weekday 

# posterior predictive distribution
# 전체적으로:)
y_pred = rpois(n_sim, lambda = lam_pred)
# new location에서의 data
# This distribution represents our posterior predictive distribution 
# of the number of chocolate chips per cookie from a new location.
hist(y_pred)

mean(y_pred > 18000)

hist(sinchon_weekday$on_off_total)
# not too different

## What is the posterior probability that the next cookie produced at location one will have fewer then seven chips?
y_pred1 = rpois(n_sim, lambda = mod_csim[, "lam[1]"]) # future on monday
hist(y_pred1)

mean(y_pred1 < 17000)

(var(y_pred))^(1/2)

### 질문에 대한 답
# 1. 요일 별 승객 수 예측
n_sim = nrow(mod_csim)
y_pred1 = rpois(n_sim, lambda = mod_csim[, "lam[1]"]) # future on monday
y_pred2 = rpois(n_sim, lambda = mod_csim[, "lam[2]"])
y_pred3 = rpois(n_sim, lambda = mod_csim[, "lam[3]"])
y_pred4 = rpois(n_sim, lambda = mod_csim[, "lam[4]"])
y_pred5 = rpois(n_sim, lambda = mod_csim[, "lam[5]"])

hist(y_pred1)
hist(y_pred2)
hist(y_pred3)
hist(y_pred4)
hist(y_pred5)

# 2. 요일끼리 비교 
mean(y_pred2 > y_pred1)
mean(y_pred3 > y_pred1)
mean(y_pred4 > y_pred1)
mean(y_pred5 > y_pred1)

mean(y_pred3 > y_pred2)
mean(y_pred4 > y_pred2)
mean(y_pred5 > y_pred2)

mean(y_pred4 > y_pred3)
mean(y_pred5 > y_pred3)

mean(y_pred5 > y_pred4)

# 3. 요일 별ㄹ* 명 이상이 지하철을 이용할 확률
mean(y_pred1 > 18000)
mean(y_pred2 > 18000)
mean(y_pred3 > 18000)
mean(y_pred4 > 18000)
mean(y_pred5 > 18000)
