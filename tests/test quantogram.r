source("access.r")

tr <- lload_DVF("tr_r5_2020")$EMP09 %>% iso2time(c(50000,100000)) %>% r2dt(resolution=200)
setkey(tr, idINS200)
c200 <- load_DVF("c200") %>% st_drop_geometry()
setDT(c200)
setkey(c200, idINS200)
tr <- c200[tr] %>% as_tibble()
tr
ggplot(tr)+geom_quantile(aes(x=to50k, y=Ind), method="rqss", lambda=1s)

ggplot(tr)+geom_quantogram(aes(x=to50k,  y=Ind, label=idINS200))

dv3f <- lload_DVF("dv3fv411")
ggplot(dv3f)+geom_quantogram(aes(x=to50k,  y=exp(lvm), label=idINS200))
ggplot(dv3f)+geom_quantogram(aes(x=to1M,  y=exp(lvm), label=idINS200))
ggplot(dv3f)+geom_quantogram(aes(x=dist275,  y=exp(lvm), label=idINS200, col=date_ma))+facet_wrap(~date_ma)
ggplot(dv3f)+geom_quantogram(aes(x=trdvf_emp09_50k,  y=exp(lvm), label=idINS200, col=date_ma))+facet_wrap(~date_ma)

