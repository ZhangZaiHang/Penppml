rm(list = ls())
require(haven)
require(penppml)


WB_TRADE_DATA <- read_dta("C:/Users/ZaihangZhang/OneDrive/MasterThesis/MachineLearninginAgriculturalTradeResearch/code/R/Some questions about overfitting in gravity model/Data/temp_trade_only.dta")
WB_LARGE   <- read_dta("C:/Users/ZaihangZhang/OneDrive/MasterThesis/MachineLearninginAgriculturalTradeResearch/code/R/Some questions about overfitting in gravity model/Data/temp_provisions_largedataset_essential_Jan302021.dta")

x_LARGE    <- merge(WB_TRADE_DATA,WB_LARGE,c("iso1","iso2","year"),all.x=TRUE,all.y=FALSE)
x_LARGE[is.na(x_LARGE)]       <- 0
x_LARGE <- x_LARGE[,-1:-9]
x_LARGE <- x_LARGE[,1:305]
x_LARGE  <- data.matrix(x_LARGE)

rta       <- ((rowSums(x_LARGE ))>0)*1
fta_eia   <- WB_TRADE_DATA$fta_eia
excluded  <- (rta==0 & fta_eia==1)

WB_TRADE_DATA <- WB_TRADE_DATA[which(!excluded),]
x_LARGE       <- x_LARGE[which(!excluded),]
rta           <- rta[which(!excluded)]

trade <- WB_TRADE_DATA$export

# classify FEs
exp  <- factor(WB_TRADE_DATA$iso1)
imp  <- factor(WB_TRADE_DATA$iso2)
time <- factor(WB_TRADE_DATA$year)
exp_time <- interaction(exp, time)
imp_time <- interaction(imp, time)
pair     <- interaction(exp, imp)

fes       <- list(exp_time,imp_time,pair)

# IDs
IDs <- WB_TRADE_DATA$id
IDs[is.na(IDs)] <- 0
IDs[rta==0] <- 0
IDs_orig <- IDs

# let's do something slightly different (cluster by agreement)
WB_TRADE_DATA$alt_id <- WB_TRADE_DATA$id
WB_TRADE_DATA$alt_id[is.na(WB_TRADE_DATA$alt_id)] <- 0
WB_TRADE_DATA$pair <- pair
WB_TRADE_DATA <- within(WB_TRADE_DATA, {alt_id2 = ave(alt_id,pair,FUN=max)} )
WB_TRADE_DATA$alt_id2[WB_TRADE_DATA$alt_id2==0] <- WB_TRADE_DATA$pair[WB_TRADE_DATA$alt_id2==0]

alt_id2 <- factor(WB_TRADE_DATA$alt_id2)

x_LARGE    <- merge(WB_TRADE_DATA,WB_LARGE,c("iso1","iso2","year"),all.x=TRUE,all.y=FALSE)
x_LARGE[is.na(x_LARGE)] <- 0
x_LARGE$exp_time=exp_time
x_LARGE$imp_time=imp_time
x_LARGE$pair=pair
x_LARGE$alt_id2=alt_id2

plugin_alltrade <- mlfitppml(data = x_LARGE,
                                      indep=13:317,
                                      dep = "export",
                                      penalty = "lasso",
                                      method = "plugin",
                                      cluster = "alt_id2",
                                      fixed = list("exp_time","imp_time","pair"),
                                      hdfetol=1e-2,
                                      tol=1e-6,
                                      colcheck_x = FALSE,
                                      colcheck_x_fes = FALSE,
                                      post=TRUE, phipost=TRUE)

results_alltrade <- data.frame(prov = rownames(plugin_alltrade$beta), b_pre = plugin_alltrade$beta_pre, b = plugin_alltrade$beta, se = 0)
results_alltrade$se <- plugin_alltrade$ses[1,]

#Takes a bit of time running; result depends on seed.
boot_alltrade <- bootstrap(data=x_LARGE, dep="export", cluster = "alt_id2",
                           fixed = list("exp_time","imp_time","pair"), indep=13:317,
                           bootreps=250, boot_threshold = 0.01,hdfetol=1e-2,tol=1e-6,colcheck_x = FALSE,
                           colcheck_x_fes = FALSE, post=FALSE, phipost = TRUE)

prop<-rowSums(boot_alltrade$betas_pre!=0, na.rm=T)/250
View(cbind(prop[which(prop>0.01)]))
boot_alltrade$selected
