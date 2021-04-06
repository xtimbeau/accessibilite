source("init.r")
library(vroom)
reg <- "r93"
uu <- "00759"
## locaux 2018 -----------------
locaux.ff2018 <- vroom("{localdata}/locaux.ff2018.{reg}.csv" %>% glue ,
                       col_types = cols_only(idbat="c", idlocal="c", idcom="c",
                                             dteloc="i", dteloctxt="c", ccodep="c", slocal="d", 
                                             cconlc="c", dnatlc="c", typeact="c", actvac="c", fburx="c", cconac="c", cconactxt="c",
                                             stoth="d", stotd="d", stotdsueic="d", sprincp="d", habitat="c", ccthp="c",
                                             ssecp="d", ssecncp="d", sparkp="d", sparkncp="d", X="d", Y="d"), n_max=Inf)

locaux.ff2018 <- locaux.ff2018 %>%
  drop_na(X,Y) %>% 
  mutate(idINS50=idINS3035(X,Y, resolution=50),
         idINS200=idINS3035(X,Y, resolution=200)) %>% 
  mutate(act = is.na(actvac), 
       cconac2 = str_sub(cconac,1,2),
       nonaf=is.na(cconac), 
       dep = str_sub(idcom,1,2), 
       typeact3 =str_sub(typeact, 1,3),
       typeact3=ifelse(is.na(typeact3), "AUTRES", typeact3))

activite.18 <- locaux.ff2018 %>%
  select(actvac, idINS50,idINS200, sprincp, typeact, dnatlc, cconac, cconactxt, fburx, idcom, ccthp, habitat, typeact3, X, Y) %>% 
  filter(sprincp>0) %>% 
  drop_na(X,Y) %>% 
  st_as_sf(coords=c("X", "Y"), crs=3035)

# flores ---------------
naf <- readxl::read_excel("{DVFdata}/sources/Flores/naf2008_5_niveaux.xls" %>% glue, sheet="naf") %>% 
  select(NAF=NIV2, NAF1=NIV1, NAFl1=label1, NAFl2=label2) %>% 
  group_by(NAF) %>% 
  summarise(across(c(NAF1, NAFl1, NAFl2), first))

naf1 <- naf %>% 
  group_by(NAF1) %>% 
  summarise(NAFl1=first(NAFl1)) %>% 
  mutate(NAFl1 = glue("({NAF1}) {NAFl1}"))

iris <- load_DVF("iris15") 

com <- iris %>%
  filter(UU2010==uu) %>% 
  group_by(COM) %>%
  summarize(DEP=first(DEP), P15_POP=sum(P15_POP, na.rm=TRUE), EMP09=sum(EMP09, na.rm=TRUE)) %>% 
  rename(idcom=COM)

flores <- fread("{DVFdata}/sources/flores/TD_FLORES2017_NA88_NBSAL.csv" %>% glue) %>%
  pivot_longer(cols=starts_with("EFF_"),names_to = "NAF", values_to = "EMP") %>% 
  mutate(NAF = str_remove(NAF, "EFF_")) %>% 
  rename(idcom=CODGEO) %>% 
  left_join(naf, by="NAF") %>% 
  filter(NAF!="TOT")

flores_agg <- flores %>%
  group_by(idcom) %>%
  summarise(EMP=sum(EMP))

ff <- flores_agg %>% 
  left_join(com, by="idcom") %>% 
  st_as_sf() %>%
  filter(!is.na(EMP09))

(map_emp <- tmap_arrange(tm_shape(ff)+tm_fill(col = "EMP", style="cont"),tm_shape(ff)+tm_fill(col="EMP09", style="cont")))

# allocation de l'emploi en fonction des surfaces de bureau par NAF, donc pour les surfaces qui ont une NAF ----------------

emp_flores <- locaux.ff2018 %>%
  filter(sprincp>0, !nonaf) %>% 
  transmute(sp = sprincp,
            sp_act = sprincp*act,
            NAF=cconac2,
            idINS200, idcom) %>% 
  group_by(idINS200, NAF) %>% 
  summarise(sp=sum(sp, na.rm=TRUE),
            sp_act=sum(sp_act, na.rm=TRUE), 
            idcom=first(idcom)) %>% 
  ungroup() %>% 
  left_join(naf %>% select(NAF,NAF1), by="NAF") %>% 
  left_join(flores %>%
              group_by(idcom, NAF1) %>%
              summarize(EMP=sum(EMP, na.rm=TRUE)),
            by=c("idcom", "NAF1")) %>% 
  group_by(idcom, NAF1) %>% 
  mutate(sp_com_naf = sum(sp, na.rm=TRUE),
         sp_com_naf_act = sum(sp_act, na.rm=TRUE)) %>% 
  ungroup()

emp_com_naf1 <- emp_flores %>% 
  group_by(idcom, NAF1) %>% 
  summarize(sp=sum(sp, na.rm = TRUE),
            EMP=first(EMP)) %>% 
  left_join(naf1, by="NAF1")

library(broom)
emp_reg_naf1 <- emp_com_naf1 %>% 
  filter(!NAF1%in%c("O", "T", "U", NA)) %>% 
  group_by(NAF1) %>% 
  summarise(mod = list(lm(log(EMP)~log(sp), data=cur_data() %>% filter(EMP>0)))) %>% 
  mutate(alpha = map_dbl(mod, ~.x$coefficients[[2]]),
         C = map_dbl(mod, ~.x$coefficients[[1]]),
         r2 = map_dbl(mod, ~glance(.x)$r.squared))

ggplot(emp_com_naf1, aes(x=log(EMP), y=log(sp), col=NAF1))+
  geom_point(alpha=0.1)+
  geom_smooth(method="lm")+
  facet_wrap(~NAFl1)+guides(fill=FALSE, color=FALSE, alpha=FALSE)

# essai à la NAF2, mais ca ne marche pas
# emp_reg_naf2 <- emp_flores %>% 
#   filter(!NAF1%in%c("O", "T", "U", NA)) %>% 
#   filter(str_sub(idcom,1,2)%in%depIdf) %>% 
#   group_by(NAF) %>% 
#   summarise(mod = list(if(nrow(cur_data())>5) lm(log(EMP)~log(sp), data=cur_data() %>% filter(EMP>0)))) %>% 
#   mutate(alpha = map_dbl(mod, ~ifelse(!is.null(.x), .x$coefficients[[2]], NA_real_)),
#          C     = map_dbl(mod, ~ifelse(!is.null(.x), .x$coefficients[[1]], NA_real_)),
#          r2    = map_dbl(mod, ~ifelse(!is.null(.x), glance(.x)$r.squared, NA_real_)))

emp_flores_aug <- emp_flores %>% 
  filter(!NAF1%in%c("O", "T", "U", NA)) %>% 
  left_join(emp_reg_naf1, by=c("NAF1")) %>% 
  mutate(emp_p = ceiling(exp(predict(mod[[1]], tibble("sp"=sp))))) %>% 
  group_by(idcom, NAF1) %>% 
  mutate(EMP_p=sum(emp_p, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(emp_p2 = emp_p*EMP/EMP_p,
         emp_r =  EMP*sp/sp_com_naf)

emp_flores_fin <- emp_flores_aug %>% 
  mutate(emp17_mG = ifelse(NAF1%in%c("G"), 0, emp_p2),
         emp17 = emp_p2) %>% 
  group_by(idINS200) %>% 
  summarise(across(c(emp17, emp17_mG), ~sum(.x, na.rm=TRUE)))

emp17 <- st_join(emp_flores_fin %>% idINS2sf("idINS200"), iris %>% select(EMP09, CODE_IRIS))
emp17 <- emp17 %>%
  group_by(CODE_IRIS) %>% 
  mutate(EMP09 = EMP09/n())

save_DVF(emp17, "emp17.{reg}")

tm_shape(emp17 %>% select(-EMP09) %>% dt2r(resolution = 200)) + tm_raster(style="kmeans", breaks=0:5*50)

emp_iris <- emp17 %>%
  st_drop_geometry() %>% 
  group_by(CODE_IRIS) %>% 
  summarize(emp17=sum(emp17, na.rm=FALSE),
            emp17_mG=sum(emp17_mG, na.rm=FALSE),
            EMP09=sum(EMP09)) %>% 
  left_join(iris %>% as_tibble() %>% select(CODE_IRIS, geometry), by="CODE_IRIS") %>% 
  st_as_sf()

tmap_arrange(tm_shape(emp_iris)+tm_fill(col="EMP09", style="kmeans"), 
             tm_shape(emp_iris)+tm_fill(col="emp17", style="kmeans"),
             tm_shape(emp_iris)+tm_fill(col="emp17_mG", style="kmeans"))

ggplot(emp_iris)+geom_point(aes(x=emp17, y=EMP09))

# Méthode 2: allocation de l'emploi en fonction des surfaces de bureau sans NAF, donc pour toutes les surfaces

emp_tot <- locaux.ff2018 %>%
  filter(sprincp>0) %>% 
  transmute(sp = sprincp,
            sp_act = sprincp*act,
            idINS200, idcom) %>% 
  group_by(idINS200) %>% 
  summarise(sp=sum(sp, na.rm=TRUE),
            sp_act=sum(sp_act, na.rm=TRUE), 
            idcom=first(idcom)) %>% 
  ungroup() %>% 
  left_join(flores %>%
              group_by(idcom) %>%
              summarize(EMP=sum(EMP, na.rm=TRUE)),
            by=c("idcom")) %>% 
  group_by(idcom) %>% 
  mutate(sp_com = sum(sp, na.rm=TRUE),
         sp_com_act = sum(sp_act, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(empt17 = sp/sp_com*EMP,
         empt17_act = sp_act/sp_com_act*EMP)

emp17 <- left_join(emp17, emp_tot %>% select(idINS200, empt17, empt17_act), by="idINS200")
emp17 %>% as_tibble() %>% summarize(across(where(is.numeric), ~sum(.x, na.rm=TRUE)))

tm_shape(emp17 %>% select(-EMP09, -empt17_act, -emp17_mG) %>% dt2r(resolution = 200)) + tm_raster(style="cont", breaks=0:5*1000/5)
ggplot(emp17, aes(x=emp17, y=empt17))+geom_point(alpha=0.1)+xlim(c(0,5000))+ylim(c(0,5000))
save_DVF(emp17, "emp17.{uu}")

emp_tot_reg <- emp_tot %>% 
  group_by(idcom) %>% 
  summarize(EMP=first(EMP),
            sp = sum(sp, na.rm=TRUE),
            sp_act = sum(sp, na.rm=TRUE))

lm(log(EMP)~log(sp), data = emp_tot_reg)

ggplot(emp_tot_reg, aes(x=log(EMP))) + 
  geom_point(aes(y=log(sp)), alpha=0.1, col="orange")+
  geom_point(aes(y=log(sp_act)), alpha=0.1, col="blue")+
  geom_smooth(aes(y=log(sp)), method="lm")

# export diu csv pour r5
source("init.r")
emp17 <- lload_DVF("emp17.r93")

emp17.csv <- emp17 %>%
  idINS2sf(idINS="idINS200") %>%
  st_transform(4326) %>%
  mutate(lon=st_coordinates(.)[,1], lat=st_coordinates(.)[,2]) %>% 
  st_drop_geometry() %>% 
  select(-idINS200, -CODE_IRIS)

fwrite(emp17.csv, "{DVFdata}/rda/emp17.csv" %>% glue)
