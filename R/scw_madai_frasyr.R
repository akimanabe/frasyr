caa  <- read.csv("/home/rstudio/data/setodai/caa.csv", row.names = 1)
waa  <- read.csv("/home/rstudio/data/setodai/waa.csv", row.names = 1)
maa  <- read.csv("/home/rstudio/data/setodai/maa.csv", row.names = 1)
Mdat <- read.csv("/home/rstudio/data/setodai/M.csv"  , row.names = 1)

madai_data <- frasyr::data.handler(caa = caa, waa = waa, maa = maa, M = Mdat)

res.madai_frasyr <- frasyr::vpa(madai_data,
                                fc.year = 2013:2017,
                                rec = NULL,
                                tf.year = 2013:2017,
                                term.F = "max",
                                stat.tf = "mean",
                                Pope = TRUE,
                                tune = FALSE,
                                p.init = 0.2,
                                alpha = 1)

#ci0 <- suppressWarnings(frasyr::profile_likelihood.vpa(res.madai_frasyr)$ci)

# 将来予測やMSY計算で使うcurrent F
show(res.madai_frasyr$Fc.at.age)

plot(res.madai_frasyr$Fc.at.age, type = "b", xlab = "Age", ylab = "F",
     ylim = c(0, max(res.madai_frasyr$Fc.at.age)))

frasyr::plot_vpa(res.madai_frasyr)

frasyr::out.vpa(res.madai_frasyr)

byear <- 1977:2017

rres.madai <-
    frasyr::ref.F(res.madai_frasyr,
          waa.year  = byear,
          maa.year  = byear,
          M.year    = byear,
          rps.year  = 2013:2017,
          max.age   = Inf,
          min.age   = 0,
          pSPR      = c(10,20,30,35,40))


show(rres.madai$summary)


#再生産関係の推定

SRdata <- frasyr::get.SRdata(res.madai_frasyr)
head(SRdata)

#モデルのフィット

# L1の場合
res_L1 <-
  purrr::map(.x = c("HS", "BH", "RI"),
             .f = function(x) frasyr::fit.SR(SRdata = SRdata,
                                             SR = x,
                                             method = "L1",
                                             AR = 1))
# L2の場合
res_L2 <-
  purrr::map(.x = c("HS", "BH", "RI"),
             .f = function(x) frasyr::fit.SR(SRdata = SRdata,
                                             SR = x,
                                             method = "L2",
                                             AR = 1))

SRfit_AIC <- tibble::tibble(SR = rep(c("HS", "BH", "RI"), 2),
                            method = rep(c("L1", "L2"), each = 3),
                            AIC = c(purrr::map(c(1,2,3), function(x)res_L1[[x]]$AIC) %>% unlist(),
                                    purrr::map(c(1,2,3), function(x)res_L2[[x]]$AIC) %>% unlist()))

show(SRfit_AIC)

# Graph
frasyr::plot_SRdata(SRdata)
points(res_L2[[1]]$pred$SSB, res_L2[[1]]$pred$R, col=2, type="l", lty = 1, lwd=3) #HS
points(res_L2[[2]]$pred$SSB, res_L2[[2]]$pred$R, col=3, type="l", lty = 1, lwd=3) #BH
points(res_L2[[3]]$pred$SSB, res_L2[[3]]$pred$R, col=4, type="l", lty = 1, lwd=3) #RI
points(res_L1[[1]]$pred$SSB, res_L1[[1]]$pred$R, col=2, type="l", lty = 2, lwd=1) #HS_L1
points(res_L1[[2]]$pred$SSB, res_L1[[2]]$pred$R, col=3, type="l", lty = 2, lwd=1) #BH_L2
points(res_L1[[3]]$pred$SSB, res_L1[[3]]$pred$R, col=4, type="l", lty = 2, lwd=1) #RI_L3

# 将来予測
data_future_test <-
  frasyr::make_future_data(res_vpa = res.madai_frasyr,
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


res_future_madai <-
  frasyr::future_vpa(
    tmb_data=data_future_test$data,
    optim_method="none", # "none": 単なる将来予測, "R" or "tmb": 以下、objective, obj_value等で指定した目的関数を満たすように将来のFに乗じる係数を最適化する
    multi_init = 1) # 将来予測のさい、将来のFに乗じる乗数

res_future_madai$multi


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

