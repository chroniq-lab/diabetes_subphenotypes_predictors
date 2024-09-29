rm(list = ls());gc();source(".Rprofile")

clusters = read_csv(paste0(path_diabetes_subphenotypes_adults_folder,"/working/processed/dec_an02_clean_kmeans_5var_mi_knn_cluster.csv")) %>% 
  dplyr::select(-one_of("...1")) %>% 
  left_join(readRDS(paste0(path_diabetes_subphenotypes_adults_folder,"/working/cleaned/final_dataset_temp.RDS")) %>% 
              dplyr::select(study_id,original_study_id),
            by=c("study_id")) %>% 
  rename(cluster_study_id = study_id)

aric_longitudinal <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dsppre01a_aric.RDS")) %>% 
  dplyr::filter((age < dmagediag))

cardia_longitudinal <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dsppre01b_cardia.RDS")) %>% 
  dplyr::filter((age < dmagediag))

jhs_longitudinal <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dsppre01e_jhs.RDS")) %>% 
  dplyr::filter((age < dmagediag))

dppos_longitudinal <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dsppre01c_dppos.RDS")) %>% 
  dplyr::filter((age < dmagediag))

mesa_longitudinal <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dsppre01f_mesa.RDS")) %>% 
  dplyr::filter((age < dmagediag))


longitudinal_df = bind_rows(aric_longitudinal %>% mutate(study = "aric") %>% mutate(study_id = as.numeric(str_replace(study_id,"C",""))),
                            cardia_longitudinal %>% mutate(study = "cardia"),
                            jhs_longitudinal %>% mutate(study = "jhs"),
                            dppos_longitudinal %>% mutate(study = "dppos"),
                            mesa_longitudinal %>% mutate(study = "mesa")) %>% 
  left_join(clusters %>% 
              dplyr::select(cluster_study_id,original_study_id,cluster,study,female),
            by=c("study"="study","study_id" = "original_study_id")) %>% 
  dplyr::filter(!is.na(cluster_study_id)) %>% 
  mutate(t = age - dmagediag) %>% 
  dplyr::filter(t >= -15)

# EXPLORATORY -----------
ggplot(data=longitudinal_df,
       aes(x=t,y=bmi,col=cluster,group=cluster)) +
  # geom_point() +
  geom_smooth()

 ggplot(data=longitudinal_df,
       aes(x=t,y=hba1c,col=cluster,group=cluster)) +
  # geom_point() +
  geom_smooth()

ggplot(data=longitudinal_df,
       aes(x=t,y=glucosef,col=cluster,group=cluster)) +
  # geom_point() +
  geom_smooth(method = "loess")


ggplot(data=longitudinal_df,
       aes(x=t,y=insulinf,col=cluster,group=cluster)) +
  # geom_point() +
  geom_smooth()

# ADJUSTED MODELS -------------
library(geepack)
library(marginaleffects)
library(splines)
m1_bmi = geeglm(bmi ~ cluster*ns(t,3) + dmagediag + female + study,data = longitudinal_df,id = cluster_study_id)
m1_hba1c = geeglm(hba1c ~ cluster*ns(t,3) + dmagediag + female + study,data = longitudinal_df,id = cluster_study_id)
m1_glucosef = geeglm(glucosef ~ cluster*ns(t,3) + dmagediag + female + study,data = longitudinal_df,id = cluster_study_id)
m1_insulinf = geeglm(insulinf ~ cluster*ns(t,3) + dmagediag + female + study,data = longitudinal_df,id = cluster_study_id)

## BMI ---------
p1_bmi <- predictions(
  m1_bmi,
  newdata = datagrid(cluster = unique,t=seq(-15,-1,by=0.5))) 

f1_bmi <- ggplot(data=p1_bmi,aes(x=t,ymin=conf.low,ymax=conf.high,y=estimate,col=cluster,fill=cluster,group=cluster)) +
  geom_path() + 
  geom_ribbon(alpha=0.5) +
  xlab("Years before diagnosis") +
  ylab(bquote('BMI ( kg' /m^2~')')) +
  theme_bw() + 
  scale_x_continuous(limits=c(-15,0),breaks=seq(-15,0,by = 3)) +
  scale_color_manual("",values=cluster_colors) +
  scale_fill_manual("",values=cluster_colors)

## HbA1c ---------
p1_hba1c <- predictions(
  m1_hba1c,
  newdata = datagrid(cluster = unique,t=seq(-15,-1,by=0.5))) 

f1_hba1c <- ggplot(data=p1_hba1c,aes(x=t,ymin=conf.low,ymax=conf.high,y=estimate,col=cluster,fill=cluster,group=cluster)) +
  geom_path() + 
  geom_ribbon(alpha=0.5) +
  xlab("Years before diagnosis") +
  ylab("HbA1c (%)") +
  theme_bw() + 
  scale_x_continuous(limits=c(-15,0),breaks=seq(-15,0,by = 3)) +
  scale_color_manual("",values=cluster_colors) +
  scale_fill_manual("",values=cluster_colors)


## Fasting Glucose ---------
p1_glucosef <- predictions(
  m1_glucosef,
  newdata = datagrid(cluster = unique,t=seq(-15,-1,by=0.5))) 

f1_glucosef <- ggplot(data=p1_glucosef,aes(x=t,ymin=conf.low,ymax=conf.high,y=estimate,col=cluster,fill=cluster,group=cluster)) +
  geom_path() + 
  geom_ribbon(alpha=0.5) +
  xlab("Years before diagnosis") +
  ylab("Fasting Glucose (mg/dL)") +
  theme_bw() + 
  scale_x_continuous(limits=c(-15,0),breaks=seq(-15,0,by = 3)) +
  scale_color_manual("",values=cluster_colors) +
  scale_fill_manual("",values=cluster_colors)

## Fasting Insulin ---------
p1_insulinf <- predictions(
  m1_insulinf,
  newdata = datagrid(cluster = unique,t=seq(-15,-1,by=0.5))) 

f1_insulinf <- ggplot(data=p1_insulinf,aes(x=t,ymin=conf.low,ymax=conf.high,y=estimate,col=cluster,fill=cluster,group=cluster)) +
  geom_path() + 
  geom_ribbon(alpha=0.5) +
  xlab("Years before diagnosis") +
  ylab("Fasting Insulin (??)") +
  theme_bw() + 
  scale_x_continuous(limits=c(-15,0),breaks=seq(-15,0,by = 3)) +
  scale_color_manual("",values=cluster_colors) +
  scale_fill_manual("",values=cluster_colors)

## Combined Figure ----------
library(ggpubr)
ggarrange(f1_bmi,
          f1_hba1c,
          f1_glucosef,
          f1_insulinf,
          common.legend = TRUE,
          legend = "bottom",
          nrow = 1,
          ncol = 4) %>% 
  ggsave(.,filename=paste0(path_diabetes_subphenotypes_predictors_folder,"/figures/trajectory of biomarkers before diagnosis.jpg"),width=12,height = 5)

