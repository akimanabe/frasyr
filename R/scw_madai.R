# ここでは例で使われていたres.pma(pacific maaji)のオブジェクト名をres.madaiに変更しています。

# 関数読み込み（rファイルのパスを入力してください）

source("../future-rvpa/rvpa1.9.2.r")
source("../future-rvpa/future1.11.r")


# ファイル読み込み(お手持ちのファイルのパスを入力してください。)

caa  <- read.csv("/home/rstudio/data/setodai/caa.csv", row.names = 1)
waa  <- read.csv("/home/rstudio/data/setodai/waa.csv", row.names = 1)
maa  <- read.csv("/home/rstudio/data/setodai/maa.csv", row.names = 1)
Mdat <- read.csv("/home/rstudio/data/setodai/M.csv"  , row.names = 1)

dat <- data.handler(caa = caa, waa = waa, maa = maa, M = Mdat)

# 資源量推定(VPA)


res.madai <- vpa(dat,
                 fc.year = 2013:2017,
                 rec = NULL,
                 tf.year = 2013:2017,
                 term.F = "max",
                 stat.tf = "mean",
                 Pope = TRUE,
                 tune = FALSE,
                 p.init = 0.2,
                 alpha = 1)

plot(res.madai$Fc.at.age, type = "b", xlab = "Age", ylab = "F",
     ylim = c(0, max(res.madai$Fc.at.age)))

# frasyrをインストールしている場合は↓の関数も使えます
# frasyr::plot_vpa(res.madai)

# VPA結果のアウトプット

out.vpa(res.madai)

#再生産関係を仮定しない管理基準値の計算

byear <- 1977:2017

rres.madai <-
  suppressWarnings(
    ref.F(res.madai,
          waa.year  = byear,
          maa.year  = byear,
          M.year    = byear,
          rps.year  = 2013:2017,
          max.age   = Inf,
          pSPR      = c(10,20,30,35,40),
          Fspr.init = 1)
  )

show(rres.madai$summary)

#再生産関係の推定

SRdata <- get.SRdata(res.madai)
head(SRdata)

#モデルのフィット

## Hockey-stick
HS.par <- fit.HS(SRdata,er.log = TRUE,
                 gamma1 = 0.001,
                 do.profile = TRUE)

show(HS.par$pars)

## Beverton-Holt
BH.par <- fit.BH(SRdata,
                 er.log = TRUE)

show(BH.par$pars)

## Ricker
RI.par <- fit.RI(SRdata,
                 er.log = TRUE)

show(RI.par$pars)

# AICの比較
compare_AIC <- c(HS.par$AICc,BH.par$AICc,RI.par$AICc)
show(compare_AIC)

plot.SRdata(SRdata)
points(HS.par$pred$SSB,HS.par$pred$R,col=2,type="l",lwd=3)
points(BH.par$pred$SSB,BH.par$pred$R,col=3,type="l",lwd=3)
points(RI.par$pred$SSB,RI.par$pred$R,col=4,type="l",lwd=3)

# 将来予測
fres.HS <- future.vpa(res.madai,
                      multi = 1,
                      nyear = 50,
                      start.year = 2018,
                      waa.year = 2013:2017,
                      maa.year = 2013:2017,
                      M.year = 2013:2017,
                      is.plot = TRUE,
                      seed = 1,
                      recfunc = HS.rec,
                      rec.arg = list(a = HS.par$pars$a,
                                     b = HS.par$pars$b,
                                     gamma = HS.par$gamma,
                                     sd = HS.par$pars$sigma,
                                     bias.correction = TRUE,
                                     resample = TRUE,
                                     resid = HS.par$resid))

# MSY管理基準値の計算
MSY.HS <- est.MSY(res.madai,
                  fres.HS$input,
                  nyear = 50,
                  N = 10,
                  PGY = c(0.9, 0.95),
                  B0percent = c(0.3, 0.4))

show(MSY.HS$summary)

# パフォーマンス指標のとりだし
MSY.index <-get.perform(fout0 = MSY.HS$fout.msy,
                        Blimit = HS.par$pars$b,
                        longyear = 50,
                        smallcatch = 0.5,
                        shortyear = c(3, 5, 10),
                        tmp.year = NULL
)

show(MSY.index)
