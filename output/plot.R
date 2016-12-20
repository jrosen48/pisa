setwd("~/Google Drive/DATA ARCHIVE/pisa")
s_q_ss <- dplyr::select(s_q,
                        country = CNT,
                        student_efficacy = SCIEEFF,
                        #student_joy = JOYSCIE,
                        student_interest = INTBRSCI,
                        #student_epistemic_beliefs = EPIST,
                        student_utility = INSTSCIE,
                        student_value_cooperation = CPSVALUE,
                        student_enjoy_cooperation = COOPERATE,
                        #student_environmental_awareness = ENVAWARE,
                        
                        #parent_views_science = PQGENSCI,
                        #parent_enviro_concern = PQENPERC,
                        
                        teacher_directed = TDTEACH,
                        teacher_inquiry_based = IBTEACH,
                        teacher_choice_support = TEACHSUP,
                        teacher_perceived_feedback = PERFEED,
                        teacher_adaptation_of_instruction = ADINST,
                        
                        #science_minutes_week = SMINS,
                        #out_of_schools_study_hours = OUTHOURS,
                        
                        ACHIEVEMENT_overall = PV1SCIE,
                        ACHIEVEMENT_explain = PV1SCEP,
                        ACHIEVEMENT_do_inquiry = PV1SCED,
                        ACHIEVEMENT_interpret = PV1SCID)

s_q_ss <- dplyr::filter(s_q_ss, country == "USA")

get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
}

s_q_ss <- dplyr::select(s_q_ss, -country)
cormat <- round(cor(s_q_ss, use = "pairwise.complete"), 2)
upper_tri <- get_upper_tri(cormat)
melted_cormat <- reshape2::melt(upper_tri, na.rm = T)
melted_cormat$Var1 <- factor(melted_cormat$Var1, 
                             levels = names(s_q_ss)
                                 )

ggheatmap <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed()
ggheatmap
ggheatmap + 
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
    theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))
ggsave("pisa_plot_usa.png", width = 12, height = 12)

