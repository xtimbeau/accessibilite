c200 <- load_DVF("c200")
iris15 <- load_DVF("iris15")
uu851 <- iris15 %>% filter(UU2010=="00851") %>% st_union()
uu851plus20 <- uu851 %>% st_buffer(20000)
c200_851 <- c200 %>% filter(st_within(., uu851, sparse=FALSE))
c200_851_plus <- c200 %>% filter(st_within(., uu851 %>% st_buffer(10000), sparse=FALSE))

rmax <- sqrt(as.numeric(st_area(uu851))/pi)
idf <- map_dfr(c(-seq(0, rmax, 1000), seq(0, 25000, 1000)) %>% sort %>% unique,
    ~{
      uu851plus <- uu851 %>% st_buffer(.x)
      iris <- iris15 %>% filter(st_within(., uu851plus, sparse=FALSE)) %>% st_drop_geometry()
      iris <- iris %>% summarize(emp=sum(EMP09), pop=sum(P15_POP)) %>% mutate(dist = .x)
    }) %>%
  arrange(dist) %>% 
  mutate(demp = emp - lag(emp),
         dpop = pop-lag(pop)) %>% 
  pivot_longer(cols = c(emp, pop, demp, dpop), names_to="var", values_to="p")

idf_c200 <- map_dfr(c(-seq(0, 1000, 100), seq(0, 1000, 100)) %>% sort %>% unique,
               ~{
                 uu851plus <- uu851 %>% st_buffer(.x)
                 cccc <- c200_851_plus %>% filter(st_covered_by(., uu851plus, sparse=FALSE)) %>% st_drop_geometry()
                 cccc <- cccc %>% summarize(pop=sum(Ind)) %>% mutate(dist = .x)
               }) %>%
  arrange(dist) %>% 
  mutate(dpop = pop-lag(pop)) %>% 
  pivot_longer(cols = c(pop, dpop), names_to="var", values_to="p")

ggplot(idf_c200 %>% filter(var!="pop",var!="emp"), aes(x=dist, y=p, group=var, col=var))+ geom_line()
