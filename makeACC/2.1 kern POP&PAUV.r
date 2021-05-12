source("init.r")


csvsources <- "{localdata}/csv sources" %>% glue
c200 <- load_DVF("c200")
iris15 <- load_DVF("iris15")
idf4km <- iris15 %>% filter(UU2010=="00851") %>% st_union() %>% st_buffer(4000)
idf <- iris15 %>% filter(UU2010=="00851") %>% st_union()
c200_idf <- c200 %>% filter(st_within(., idf, sparse=FALSE))
c200_75 <- c200 %>% filter(dep=="75")
c200_mtrl <- c200 %>% filter(Depcom=="93048")
rm(c200)
iris15 <- load_DVF("iris15")
idf <- iris15 %>% filter(UU2010=="00851") %>% st_union()
c200_idfdt <- c200_idf %>%
  st_drop_geometry() %>%
  as.data.table()

fdt_idf_50 <- lload_DVF("fdt_idf_50", rep="isochrones") %>% swap2tmp_routing()
plan("multiprocess", workers=8)

# population -> POP et PAUV ------------------------------------
c200_idf <- c200_idf %>% 
  select(Ind, Men, Men_pauv) %>% 
  mutate(cste = 1) %>% 
  st_agr_aggregate(Ind, Men, Men_pauv, cste)
pop <- iso_accessibilite(
  quoi=c200_idf %>% 
    select(Ind, Men, Men_pauv) %>% 
    mutate(cste = 1),
  ou=c200_idf,                       
  resolution=50,                    
  tmax=20,                         
  pdt=1,                          
  routing=fdt_idf_50)

pop200 <- map(pop, ~aggregate(.x, 4))
densite <- r2dt(pop200$Ind/pop200$cste/0.04)
txpauv <- r2dt(pop200$Men_pauv/pop200$Ind)

popisokernel <- merge(densite[,-c("x","y")], txpauv[,-c("x","y")], by="idINS200", suffix=c(".densite", ".txpauv"))

save_DVF(popisokernel)

# m² bâtis (ff2018)

locaux.ff2018 <- vroom("{csvsources}/locaux.ff2018.csv" %>% glue ,
                       col_types = cols_only(idbat="c", idlocal="c", idcom="c",
                                             dteloc="i", dteloctxt="c", ccodep="c", slocal="d", 
                                             cconlc="c", dnatlc="c", typeact="c", actvac="c",
                                             loghvac="c", fburx="c", cconac="c", cconactxt="c", logh="c",
                                             proba_rprs = "c", hlmsem="c", dnatlc = "c", loghlls = "c",
                                             stoth="d", stotd="d", stotdsueic="d", sprincp="d", habitat="c", ccthp="c",
                                             ssecp="d", ssecncp="d", sparkp="d", sparkncp="d", X="d", Y="d"), n_max=Inf)


locaux.ff2018 <- locaux.ff2018 %>%
  drop_na(X,Y) %>% 
  mutate(idINS50=idINS3035(X,Y, resolution=50),
         idINS200=idINS3035(X,Y, resolution=200))
locaux.18 <- locaux.ff2018 %>% 
  mutate(
    actnv = is.na(actvac), 
    lognv = is.na(loghvac),  
    cconac2 = str_sub(cconac,1,2),
    nonaf=is.na(cconac), 
    dep = str_sub(idcom,1,2), 
    typeact3 =str_sub(typeact, 1,3))

logcat <- c("Owner Occ.", "Sec. Home", "Tenant", "Low rent",
            "Vacant LR", "Vacant H.",
            "Others", "Others T.", "Outbuilding", "Business")
actcat <- c("Housing", "Outbuilding", "Health&Education",
            "Services", "Industrial",
            "Offices", "Vacant", "Others")
scat <- c("Housing", "Vacant H.", "Outbuilding", "Business",
          "Vacant B.", "Others")

locaux.18 <- locaux.18 %>% 
  mutate(
    hab    = stoth>0,
    dep    = stoth==0&sprincp==0&stotd>0,
    act    = sprincp>0,
    surface = case_when(
      hab ~ stoth,
      dep ~ stotd,
      act ~ sprincp),
    hlm    = hab&loghlls%in%c("OUI", "OUI PROBABLE"),
    ccthp  = if_else(is.na(ccthp), "NA", ccthp),
    hlmsem  = if_else(is.na(hlmsem), "NA", hlmsem),
    prop   = ccthp=="P",
    loc    = ccthp%in%c("L", "G", "F"),
    locaut = ccthp%in%c("B", "R", "U", "X")) %>% 
  mutate(
    type_s = case_when(
      hab&!lognv ~ "Vacant H.", 
      hab&lognv ~ "Housing",
      act&!actnv ~ "Vacant B.",
      act&actnv ~ "Business",
      dep ~ "Outbuilding",
      TRUE ~ "Others"
    ),
    type_a = case_when(
      hab ~ "Housing",
      dep ~ "Outbuilding",
      !actnv ~ "Vacant",
      typeact3%in%c("HOT", "MAG", "SPE") ~ "Services",
      typeact3%in%c("ATE", "IND", "DEP") ~ "Industrial",
      typeact3%in%c("CLI", "ENS") ~ "Health&Education",
      typeact3%in%c("BUR") ~ "Offices",
      TRUE ~ "Others"),
    type_h = case_when(
      hab&prop&proba_rprs!="RS" ~ "Owner Occ.",
      hab&!hlm&loc ~ "Tenant",
      hab&!hlm&locaut ~ "Others T.",
      hab&prop&proba_rprs=="Sec. Home" ~ "Sec. Home",
      hlm ~ "Low rent",
      hab&!lognv&!hlmsem%in%c(5,6) ~ "Vacant",
      hab&!lognv&hlmsem%in%c(5,6) ~ "Vacant LR",
      hab ~ "Others",
      dep ~ "Outbuilding",
      TRUE ~ "Business")) %>%
  mutate(
    type_s = factor(
      type_s,
      levels = scat),
    type_a = factor(
      type_a,
      levels = actcat),
    type_h = factor(
      type_h,
      levels = logcat))

locaux.18.50 <- locaux.18 %>%
  filter(hab) %>% 
  group_by(idINS50) %>% 
  summarize(surface=sum(surface)) %>% 
  idINS2sf() %>% 
  select(-idINS50)

surfaces_baties <- iso_accessibilite(
  quoi=locaux.18.50,
  ou=c200_idf,                       
  resolution=50,                    
  tmax=20,                         
  pdt=1,                          
  routing=fdt_idf_50)

save_DVF(surfaces_baties, "kurfaces_baties", rep="isochrones")
