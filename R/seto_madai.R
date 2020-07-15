# ファイル準備

caa  <- read.csv("/home/rstudio/data/setodai/caa.csv", row.names = 1)
waa  <- read.csv("/home/rstudio/data/setodai/waa.csv", row.names = 1)
maa  <- read.csv("/home/rstudio/data/setodai/maa.csv", row.names = 1)
Mdat <- read.csv("/home/rstudio/data/setodai/M.csv"  , row.names = 1)

dat <- data.handler(caa = caa, waa = waa, maa = maa, M = Mdat)

# 資源量推定(VPA)

res.pma <- frasyr::vpa(dat,
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
HS.par <- frasyr::fit.SR(SRdata,
                         SR = "HS",
                         method = "L2",
                         AR = 1,
                         out.AR = TRUE)
BH.par <- frasyr::fit.SR(SRdata,
                         SR = "BH",
                         method = "L2",
                         AR = 1,
                         out.AR = TRUE)
RI.par <- frasyr::fit.SR(SRdata,
                         SR = "RI",
                         method = "L2",
                         AR = 1,
                         out.AR = TRUE)
HS.par$pars
BH.par$pars
RI.par$pars

# AIC比較

c(HS.par$AICc,BH.par$AICc,RI.par$AICc)
frasyr::plot_SRdata(SRdata)
points(HS.par$pred$SSB,HS.par$pred$R,col=2,type="l",lwd=3)
points(BH.par$pred$SSB,BH.par$pred$R,col=3,type="l",lwd=3)
points(RI.par$pred$SSB,RI.par$pred$R,col=4,type="l",lwd=3)

# 将来予測

## 将来予測用のデータフレーム作成

data_future_test <-
  frasyr::make_future_data(res_vpa = res.pma,
                           nsim = 1000,
                           nyear = 50,
                           future_initial_year_name = 2018,
                           start_F_year_name = 2019,
                           start_biopar_year_name = 2019,
                           start_random_rec_year_name = 2019,
                           waa_year = 1977:2018,
                           waa_catch_year = 1977:2018,
                           maa_year = 1977:2018,
                           M_year = 1977:2018,
                           faa_year = 1977:2018,
                           start_ABC_year_name=2019, # HCRを適用する最初の年
                           HCR_beta=1, # HCRのbeta
                           HCR_Blimit=-1, # HCRのBlimit
                           HCR_Bban=-1, # HCRのBban
                           HCR_year_lag=0, # HCRで何年遅れにするか
                           # SR setting
                           res_SR=res_sr_HSL2, # 将来予測に使いたい再生産関係の推定結果が入っているfit.SRの返り値
                           seed_number=1, # シード番号
                           resid_type="lognormal", # 加入の誤差分布（"lognormal": 対数正規分布、"resample": 残差リサンプリング）
                           resample_year_range=0, # リサンプリングの場合、残差をリサンプリングする年の範囲
                           bias_correction=TRUE, # バイアス補正をするかどうか
                           recruit_intercept=0, # 移入や放流などで一定の加入がある場合に足す加入尾数
                           # Other
                           Pope=res_vpa$input$Pope,
                           fix_recruit=list(year=c(2020,2021),rec=c(1000,2000)),
                           fix_wcatch=list(year=c(2020,2021),wcatch=c(1000,2000))
                           )

names(data_future_test$data)

# 単なる将来予測の場合
res_future_test <- future_vpa(tmb_data=data_future_test$data, # さっき作成した将来予測用のデータフレーム
                              optim_method="none", # "none": 単なる将来予測, "R" or "tmb": 以下、objective, obj_value等で指定した目的関数を満たすように将来のFに乗じる係数を最適化する
                              multi_init = 1) # 将来予測のさい、将来のFに乗じる乗数

# MSY計算の場合
res_msy <- future_vpa(tmb_data=data_future_test$data, # さっき作成した将来予測用のデータフレーム
                              optim_method="R",
                              multi_init  = 1,
                              multi_lower = 0.001, multi_upper = 5,
                              objective="MSY")
