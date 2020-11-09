source("dvf.r")
iris15 <- load_DVF("iris15")
iris15_75 <- iris15  %>% select(DEP, UU2010, EMP09, P15_POP) %>% filter(DEP=="75")
idf <- iris15 %>% filter(UU2010=="00851") %>% st_buffer(10000) %>% st_union
d75 <- iris15 %>% filter(DEP=="75") %>% st_union
montreuil <- iris15 %>% filter(COM=="93048") %>% st_union
iris_idf <- iris15 %>% select(DEP, UU2010, EMP09, P15_POP) %>% filter(st_within(.,idf, sparse=FALSE))
dv3f <- load_DVF("dv3fv4")
c200 <- load_DVF("c200idf") %>% st_transform(3035)  %>% st_as_sf
c200_75 <- c200 %>% filter(st_within(., d75, sparse=FALSE))
c200_mt <- c200 %>% filter(st_within(., montreuil, sparse=FALSE))

core_r5 <- setup_r5(data_path = "{DVFdata}/r5r_data/IDFM" %>% glue, verbose = FALSE)

carr550 <- accessibilite_r5(quoi=iris_idf, ou=c200_mt, 
                         resolution=50, r5_core=core_r5, temps_max=30, mode="CAR",
                         pas_de_temps=5, montecarlo = 1, time_window = 1, max_walk_dist = 500, chunk=100000, nthreads=4)
save_DVF(carr550)