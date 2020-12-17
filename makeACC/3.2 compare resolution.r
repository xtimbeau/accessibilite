source("access.r")

r200 <- load_DVF("isoIDF200/tr_r5_200")$EMP09
r50 <- load_DVF("isoIDF50/isotrr550")$EMP09

d200 <- disaggregate(r200, 4) %>% r2dt()
d50 <- r50 %>% r2dt()
dd50 <- merge(d200, d50, by="idINS50", suffix=c(".200", ".50"))
a <- r200 %>% r2dt()
b <- r50 %>% aggregate(4) %>% r2dt()
dd200 <- merge(a, b, by="idINS200", suffix=c(".200", ".50"))


ggplot(dd)+geom_point(aes(x=iso60m.50, y=iso60m.200), alpha = 0.05)         
ggplot(dd200)+geom_point(aes(x=iso60m.50, y=iso60m.200), alpha = 0.05)         


tmap_arrange(tm_shape(r50$iso60m)+tm_raster(style="cont"), tm_shape(r200$iso60m)+tm_raster(style="cont"))

dd50[, idINS200 := idINS3035(x.200,y.200, 200)]
dda <- dd50[, .(m30200 = mean(iso30m.200), m3050=mean(iso30m.50), s30200=sd(iso30m.200), s3050=sd(iso30m.50)), by=idINS200]
ggplot(dda[sample.int(.N, 25000)], aes(x=m30200, y=m3050))+geom_pointrange(aes(ymin=m3050-2*s3050, ymax=m3050+2*s3050), alpha=0.2)+geom_smooth()
corr(dda$m30200, dda$m3050)
