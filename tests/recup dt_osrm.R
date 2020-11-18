access <- rbindlist(map(aa,~fread("E:/run/{.x}" %>% glue)))

ouetquoi <- iso_ouetquoi_4326(
  quoi=c200_idf4km %>% transmute(c=1), 
  ou=c200_idf,  
  res_ou=50, 
  res_quoi=Inf,
  opp_var=c(c="c"),
  fun_quoi="any",
  resolution=50)
