library(prcr)
library(magrittr)
library(ggplot2)

setwd("~/Google Drive/DATA ARCHIVE/pisa")

s_q <- readr::read_rds("student_questionnaire.RDS")

s_q_ss <- dplyr::select(s_q,
                        country = CNT,
                        OECD,
                        student_efficacy = SCIEEFF,
                        student_joy = JOYSCIE,
                        student_broad_interest = INTBRSCI,
                        student_epistemic_beliefs = EPIST,
                        student_instrumental_motivation = INSTSCIE,
                        student_value_cooperation = CPSVALUE,
                        student_enjoy_cooperation = COOPERATE,
                        student_environmental_awareness = ENVAWARE,
                        
                        parent_views_science = PQGENSCI,
                        parent_enviro_concern = PQENPERC,
                        
                        teacher_directed = TDTEACH,
                        teacher_inquiry_based = IBTEACH,
                        teacher_choice_support = TEACHSUP,
                        teacher_perceived_feedback = PERFEED,
                        teacher_adaptation_of_instruction = ADINST,
                        
                        science_minutes_week = SMINS,
                        out_of_schools_study_hours = OUTHOURS,
                        
                        ACHIEVEMENT_overall = PV1SCIE,
                        ACHIEVEMENT_explain = PV1SCEP,
                        ACHIEVEMENT_do_inquiry = PV1SCED,
                        ACHIEVEMENT_interpret = PV1SCID)

rm(s_q)

df_us <- dplyr::filter(s_q_ss, country == "USA" | OECD == 1)
rm(s_q_ss)

df_us_ss <- dplyr::select(df_us,
                       student_efficacy,
                       student_broad_interest,
                       student_instrumental_motivation,
                       student_joy)

str(df_us_ss)
factor_df

out1 <- prepare_data(df_us_ss)

df <- data.frame(efficacy = out1[[1]], interest = out1[[2]], utility_value = out1[[3]], joy = out1[[4]])
db <- dbscan::dbscan(df, eps = .4, minPts = 4)
?dbscan

table(db$cluster)

# readr::write_csv(df, "~/downloads/to_cluster.csv")

str(df)

?fastcluster::hclust

out <- fastcluster::hclust(dist(df))

out2 <- create_profiles_lpa(out1)

out2 <- create_profiles(out1, 6)
out3 <- calculate_stats(out2)
out3[[4]]
out3[[8]] + scale_fill_discrete("")
ggsave("usa_korea.png", width = 8, height = 8)

the_filter <- attributes(out1)$cases_to_keep
all_df <- data.frame(cluster = out3[[4]], factor_df[the_filter, ])

str(all_df)

library(dplyr)

p <- all_df %>% 
    group_by(cluster, country) %>% 
    summarize(n = n()) %>% 
    group_by(country) %>% 
    mutate(n_prop = n / sum(n))

p

ggplot(p, aes(fill = country, y = n_prop, x = cluster)) +
    geom_col(position = "dodge") +
    scale_x_continuous(breaks = 1:6) +
    ylab("Proportion of Profiles by Country") +
    xlab("Cluster")

ggsave("profiles_by_country.png", width = 10, height = 7.5)

compare_cluster_statistics(out1, attributes(out2)$args_attr, 3, 10)

explore_factors(out3, df_us, "ACHIEVEMENT_overall")
