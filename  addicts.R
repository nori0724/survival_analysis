library(readr)
library(survival)
addicts <- read_csv("Documents/講義/ゼミ/M2/空間生存時間/addicts.csv", col_types = cols(X1 = col_skip()))
head(addicts)

#生存時間object
Y = Surv(addicts$survt, addicts$status ==1)

# カプランマイヤー1
kmfit1 = survfit(Y ~ 1)
summary(kmfit1)

# 365日に対する生存推定値
summary(kmfit1, times = 365)

# カプランマイヤー2
kmfit2 = survfit(Y ~ addicts$clinic)
kmfit2

plot(kmfit2, lty = c("solid", "dashed"), col=c("black", "grey"), mark.t=TRUE,
     xlab="survival time in days", ylab = "survival probabilities")
legend("topright", c("Clinic1", "Clinic2"), lty = c("solid", "dashed"),col=c("black", "grey"))

# clinicに関するログランク検定(差があるかどうかの検定)
survdiff(Y ~ addicts$clinic)
# 層化
survdiff(Y ~ clinic + strata(prison), data=addicts)


# -----2比例ハザード性の検定-----
plot(kmfit2, fun="cloglog", xlab="time in days using logarithmic scale", ylab = "log-log survival",
     main="log-log curves by clinic")
# 自作loglogplot
kmfit3 = summary(kmfit2)
kmfit4 = data.frame(kmfit3$strata, kmfit3$time, kmfit3$surv) 
names(kmfit4) = c("clinic", "time", "survival")
clinic1 = kmfit4[kmfit4$clinic == "addicts$clinic=1",]
clinic2 = kmfit4[kmfit4$clinic == "addicts$clinic=2",]
plot(clinic1$time, log(-log(clinic1$survival)), xlab="time in days using logarithmic scale", 
     ylab = "log-log survival",xlim = c(0, 800), col="black", type="l", lty="solid", 
     main="log-log curves by clinic")
par(new=T)
plot(clinic2$time, log(-log(clinic2$survival)), axes=F, xlab="time in days using logarithmic scale", 
     ylab = "log-log survival",xlim = c(0, 800), col="grey50", type="l", lty="dashed", 
     main="log-log curves by clinic")
legend("bottomright", c("Clinic1", "Clinic2"), lty = c("solid", "dashed"),col=c("black", "grey50"))
par(new=F)

# -----3cox比例ハザードモデルの実行-----
coxph(Y ~ prison + dose + clinic, data = addicts)
summary(coxph(Y ~ prison + dose + clinic, data = addicts))

# 交互作用
mod1 = coxph(Y ~ prison + dose + clinic, data = addicts)
mod2 = coxph(Y ~ prison + dose + clinic + clinic*prison + clinic*dose, data = addicts)
names(mod2)
#  対数尤度
mod2$loglik
# 尤度比検定
(-2)*(mod1$loglik[2] - mod2$loglik[2])
LRT = (-2)*(mod1$loglik[2] - mod2$loglik[2])
Pvalue = 1 - pchisq(LRT, 2)
Pvalue # ?????????p=0.1638?????????????????????

# -----4層化coxモデルの実行-----
"2よりclinicに関しては比例ハザード性は成立しないが、prisonとdoseに関しては成立するならば,
clinicを層化変数に用いた層化coxモデルが可能"
# 層化cox
coxph(Y ~ prison + dose + strata(clinic), data = addicts)
# 交互作用込
coxph(Y ~ prison + dose + clinic:prison + clinic:dose + strata(clinic), data = addicts)
# 新しいいclinic2を定義
addicts$clinic2 = addicts$clinic-2
summary(coxph(Y ~ prison + dose + clinic2:prison + clinic2:dose + strata(clinic2), data = addicts))


# -----5統計的検定による比例ハザード仮定の評価-----
"Schoenfeld残差と生存時間との相関の有無を検定"
mod1 = coxph(Y ~ prison + dose + clinic, data = addicts)
cox.zph(mod1, transform=rank) #rankで生存時間の順位を使用
plot(cox.zph(mod1, transform=rank), se=F, var="clinic") #varでclinicの残差を指定

# -----9frailtyモデルの実行
coxph(Y ~ prison + dose + strata(clinic), data = addicts)
#frailty(p=0.31よりfrailty成分は有意ではない)
coxph(Y ~ prison + dose + strata(clinic) + frailty(id, distribution = "gamma") , data = addicts)
