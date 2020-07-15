# ファイル準備

caa  <- read.csv("/home/rstudio/data/setodai/caa.csv", row.names = 1)
waa  <- read.csv("/home/rstudio/data/setodai/waa.csv", row.names = 1)
maa  <- read.csv("/home/rstudio/data/setodai/maa.csv", row.names = 1)
Mdat <- read.csv("/home/rstudio/data/setodai/M.csv"  , row.names = 1)

dat <- data.handler(caa = caa, waa = waa, maa = maa, M = Mdat)

# 資源量推定(VPA)

res.pma <- vpa(dat,
               fc.year = 2013:2017,
               rec = NULL,
               tf.year = 2013:2017,
               term.F = "max",
               stat.tf = "mean",
               Pope = TRUE,
               tune = FALSE,
               p.init = 0.2,
               alpha = 1)

## 結果確認と出力

res.pma$Fc.at.age

plot(res.pma$Fc.at.age,type="b",xlab="Age",ylab="F",ylim=c(0,max(res.pma$Fc.at.age)))

out.vpa(res.pma, filename = "seto_madai")

# 再生産関係を仮定しない管理基準値の計算

byear <- c(1977:2017)

rres.pma <- ref.F(res.pma,
                  waa.year = byear,
                  maa.year = byear,
                  M.year = byear,
                  rps.year = c(2013:2017),
                  max.age = Inf,
                  pSPR = c(10, 20, 30, 35, 40))

# F_SPRのF管理基準値の初期値は与えられたFのもとでのSPR/目的のSPRを
# 初期値とするように調整されるので不要とのことです。

rres.pma$summary

# 再生産関係の推定
SRdata <- get.SRdata(res.pma) #VPAの結果から再生産関係推定用のデータを作成する
head(SRdata)

# モデルのフィット
HS.par <- fit.HS(SRdata,er.log=TRUE,gamma1=0.001,do.profile=TRUE)
HS.par$pars
BH.par <- fit.BH(SRdata,er.log=TRUE)
BH.par$pars
RI.par <- fit.RI(SRdata,er.log=TRUE)
RI.par$pars

