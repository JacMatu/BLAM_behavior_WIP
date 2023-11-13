
blam_accuracy_points_sum <- function (data) {
    
    #Load color palets
    dark_colors <- brewer.pal(n=3, name = "Dark2") 
    light_colors <- brewer.pal(n=3, name = "Pastel2") 
    
    #LEGEND generated automatically, (re)move it with function call
    # e.g. plot <- beh_reading_speed_points(data = DF) + 
    # theme(legend.position = "none")
    
    n_rows <- data %>% nrow()
    
    data %>% 
        ggplot(aes(x = group, y = accu_percent, 
                   group = group, 
                   color = group)) +
        geom_point(position = position_jitterdodge(jitter.width=0.15,
                                                   dodge.width = 0.5),
                   size = 2.5) +
       # geom_text(aes(label=subject), color = 'black', size=3, hjust=1, vjust=1) +
        stat_summary(fun = "mean", 
                     geom = "crossbar", 
                     position = position_dodge(width = 0.5),
                     width = .45, 
                     #color = rep(c(dark_colors[1], dark_colors[2]),1)) +
                     color = c(dark_colors[1], dark_colors[2])) +
        stat_summary(fun.max = function(x) mean(x) + (sd(x)/sqrt(n_rows)),
                     fun.min = function(x) mean(x) - (sd(x)/sqrt(n_rows)),
                     geom = "errorbar",
                     position = position_dodge(width = 0.5),
                     width = .15,
                     size = 0.8,
                     #color = rep(c(dark_colors[1], dark_colors[2]),1),
                     color = c(dark_colors[1], dark_colors[2]),
                     show.legend = FALSE) +
        scale_color_manual(values = c(light_colors[1], light_colors[2])) +
        scale_y_continuous(name = "Accuracy [%]") +
        scale_x_discrete(name = "Group") +
        #labs(color="Group") +
        theme_cowplot(font_size = 16, font_family = "Arial") +
        theme(axis.line = element_line(colour = 'black', size = 1),
              axis.ticks = element_line(colour = 'black', size = 1),
              axis.text = element_text(face="bold"))
}

blam_accuracy_points_sum_group <- function (data,colour) {
    
    #Load color palets
    #dark_colors <- brewer.pal(n=3, name = "Dark2") 
    #light_colors <- brewer.pal(n=3, name = "Pastel2") 
    
    #LEGEND generated automatically, (re)move it with function call
    # e.g. plot <- beh_reading_speed_points(data = DF) + 
    # theme(legend.position = "none")
    
    n_rows <- data %>% nrow()
    
    data %>% 
        ggplot(aes(x = group, y = accu_percent, colour = group)) +
        geom_point(#position = position_jitterdodge(jitter.width=0.15,
                    #                               dodge.width = 0.5),
                   size = 2.5) +
        #geom_text(aes(label=subject), color = 'black', size=3, hjust=1, vjust=1) +
        stat_summary(fun = "mean", 
                     geom = "crossbar", 
                     position = position_dodge(width = 0.5),
                     width = .45)+
                     #color = rep(c(dark_colors[1], dark_colors[2]),1)) +
                     #color = c(dark_colors[1], dark_colors[2])) +
        stat_summary(fun.max = function(x) mean(x) + (sd(x)/sqrt(n_rows)),
                     fun.min = function(x) mean(x) - (sd(x)/sqrt(n_rows)),
                     geom = "errorbar",
                     position = position_dodge(width = 0.5),
                     width = .15,
                     size = 0.8,
                     #color = rep(c(dark_colors[1], dark_colors[2]),1),
                     #color = c(dark_colors[1], dark_colors[2]),
                     show.legend = FALSE) +
        scale_color_manual(values = colour) +
        scale_y_continuous(name = "Accuracy [%]") +
        scale_x_discrete(name = "Group") +
        #labs(color="Group") +
        theme_cowplot(font_size = 16, font_family = "Arial") +
        theme(axis.line = element_line(colour = 'black', size = 1),
              axis.ticks = element_line(colour = 'black', size = 1),
              axis.text = element_text(face="bold"))
}


blam_accuracy_points_sum_visloc <- function (data) {
    
    #Load color palets
    dark_colors <- brewer.pal(n=3, name = "Dark2") 
    light_colors <- brewer.pal(n=3, name = "Pastel2") 
    
    #LEGEND generated automatically, (re)move it with function call
    # e.g. plot <- beh_reading_speed_points(data = DF) + 
    # theme(legend.position = "none")
    
    n_rows <- data %>% nrow()
    
    data %>% 
        ggplot(aes(x = group, y = accu_percent, 
                   group = group, 
                   color = group)) +
        geom_point(position = position_jitterdodge(jitter.width=0.15,
                                                   dodge.width = 0.5),
                   size = 2.5) +
        geom_text(aes(label=subject), color = 'black', size=3, hjust=1, vjust=1) +
        stat_summary(fun = "mean", 
                     geom = "crossbar", 
                     position = position_dodge(width = 0.5),
                     width = .45, 
                     #color = rep(c(dark_colors[1], dark_colors[2]),1)) +
                     color = c(dark_colors[2])) +
        stat_summary(fun.max = function(x) mean(x) + (sd(x)/sqrt(n_rows)),
                     fun.min = function(x) mean(x) - (sd(x)/sqrt(n_rows)),
                     geom = "errorbar",
                     position = position_dodge(width = 0.5),
                     width = .15,
                     size = 0.8,
                     #color = rep(c(dark_colors[1], dark_colors[2]),1),
                     color = c( dark_colors[2]),
                     show.legend = FALSE) +
        scale_color_manual(values = c(light_colors[2])) +
        scale_y_continuous(name = "Accuracy [%]") +
        scale_x_discrete(name = "Group") +
        #labs(color="Group") +
        theme_cowplot(font_size = 16, font_family = "Arial") +
        theme(axis.line = element_line(colour = 'black', size = 1),
              axis.ticks = element_line(colour = 'black', size = 1),
              axis.text = element_text(face="bold"))
}


blam_accuracy_points_run <- function (data) {
    
    #Load color palets
    dark_colors <- brewer.pal(n=3, name = "Dark2") 
    light_colors <- brewer.pal(n=3, name = "Pastel2") 
    
    #LEGEND generated automatically, (re)move it with function call
    # e.g. plot <- beh_reading_speed_points(data = DF) + 
    # theme(legend.position = "none")
    
    gr = data$group[1]
    
    data %>% 
        ggplot(aes(x = run_num, y = accu_percent, 
                   group = group, 
                   color = group)) +
        geom_point(position = position_jitterdodge(jitter.width=0.15,
                                                   dodge.width = 0.5),
                   size = 2.5) +
        geom_text(aes(label=run_lab), color = 'black', size=3, hjust=1, vjust=1) + 
        #scale_color_manual(values = dark_colors[1]) +
        #try to color by group
        scale_color_manual(values = ifelse(gr == "EB",
                                           dark_colors[1],
                                           dark_colors[2])) +
        scale_y_continuous(name = "Accuracy [%]") +
        scale_x_discrete(name = "Run") +
        
        #labs(color="Group") +
        theme_cowplot(font_size = 16, font_family = "Arial") +
        theme(axis.line = element_line(colour = 'black', size = 1),
              axis.ticks = element_line(colour = 'black', size = 1),
              axis.text = element_text(face="bold"),
              legend.position="none",
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        facet_wrap(~subject, nrow = 2)
}

blam_accuracy_points_run_cond <- function (data) {
    
    #Load color palets
    dark_colors <- brewer.pal(n=3, name = "Dark2") 
    light_colors <- brewer.pal(n=3, name = "Pastel2") 
    
    #LEGEND generated automatically, (re)move it with function call
    # e.g. plot <- beh_reading_speed_points(data = DF) + 
    # theme(legend.position = "none")
    
    gr = data$group[1]
    
    data %>% 
        ggplot(aes(x = run_num, y = accu_percent, 
                   group = group, 
                   color = group,
                   shape = cond)) +
        geom_point(position = position_jitterdodge(jitter.width=0.15,
                                                   dodge.width = 0.5),
                   size = 4.5) +
        geom_text(aes(label=run_lab), color = 'black', size=3, hjust=1, vjust=1) + 
        #scale_color_manual(values = dark_colors[1]) +
        #try to color by group
        scale_color_manual(values = ifelse(gr == "EB",
                                           dark_colors[1],
                                           dark_colors[2])) +
        scale_y_continuous(name = "Accuracy [%]") +
        scale_x_discrete(name = "Run") +
        
        #labs(color="Group") +
        theme_cowplot(font_size = 16, font_family = "Arial") +
        theme(axis.line = element_line(colour = 'black', size = 1),
              axis.ticks = element_line(colour = 'black', size = 1),
              axis.text = element_text(face="bold"),
              legend.position="none",
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        facet_wrap(~subject, nrow = 2)
}

blam_accuracy_points_sum_conditions <- function (data) {
    
    #Load color palets
    dark_colors <- brewer.pal(n=3, name = "Dark2") 
    light_colors <- brewer.pal(n=3, name = "Pastel2") 
    
    #Get the list of eperimental conditions from the dataframe
    # CAUTION: ASSUMED COLUMN NAME = cond
    conditions <- unique(data$cond)
    
    #LEGEND generated automatically, (re)move it with function call
    # e.g. plot <- beh_reading_speed_points(data = DF) + 
    # theme(legend.position = "none")
    
    n_rows <- data %>% nrow()
    
    data %>% 
        ggplot(aes(x = cond, y = accu_percent, 
                   group = interaction(cond, group), 
                   color = group)) +
        geom_point(position = position_jitterdodge(jitter.width=0.15,
                                                   dodge.width = 0.5),
                   size = 2.5) +
        # Subject labels?
        # geom_text(aes(label=subject), color = 'black', size=3, hjust=1, vjust=1) +
        stat_summary(fun = "mean", 
                     geom = "crossbar", 
                     position = position_dodge(width = 0.5),
                     width = .45)+#, 
                     #color = rep(c(dark_colors[1], dark_colors[2]),1)) +
                     #color = rep(c(dark_colors[1], dark_colors[2]),times =4)) +
        stat_summary(fun.max = function(x) mean(x) + (sd(x)/sqrt(n_rows)),
                     fun.min = function(x) mean(x) - (sd(x)/sqrt(n_rows)),
                     geom = "errorbar",
                     position = position_dodge(width = 0.5),
                     width = .15,
                     size = 0.8,
                     #color = rep(c(dark_colors[1], dark_colors[2]),1),
                     #color = rep(c(dark_colors[1], dark_colors[2]),times =4),
                     show.legend = FALSE) +
        scale_color_manual(values = rep(c(dark_colors[1], dark_colors[2]),times =4)) +
        scale_y_continuous(name = "Accuracy [%]") +
        scale_x_discrete(name = "Conditions") +
        #labs(color="Group") +
        theme_cowplot(font_size = 16, font_family = "Arial") +
        theme(axis.line = element_line(colour = 'black', size = 1),
              axis.ticks = element_line(colour = 'black', size = 1),
              axis.text = element_text(face="bold"))
}


blam_accuracy_points_sum_conditions_group <- function (data, lab, colour) {
    
    
    #Get the list of eperimental conditions from the dataframe
    # CAUTION: ASSUMED COLUMN NAME = cond
    conditions <- unique(data$cond)
    
    #LEGEND generated automatically, (re)move it with function call
    # e.g. plot <- beh_reading_speed_points(data = DF) + 
    # theme(legend.position = "none")
    
    n_rows <- data %>% nrow()
    
    data %>% 
        ggplot(aes(x = cond, y = accu_percent, 
                   #group = subject, 
                   color = cond)) +
        geom_point(#position = position_jitterdodge(jitter.width=0.15,
                    #                               dodge.width = 0.5),
                   size = 2.5,
                   alpha = 0.8) +
        # Subject lines? 
        geom_line(aes(group = subject), 
                 # position = position_jitterdodge(jitter.width=0.15,
                 #                                 dodge.width = 0.5), 
                  size = 0.2) +
        # Subject labels?
        #geom_text(aes(label=subject), color = 'black', size=3, hjust=1, vjust=1) +
        stat_summary(fun = "mean", 
                     geom = "crossbar", 
                     position = position_dodge(width = 0.5),
                     width = .45)+#, 
        #color = rep(c(dark_colors[1], dark_colors[2]),1)) +
        #color = rep(c(dark_colors[1], dark_colors[2]),times =4)) +
        stat_summary(fun.max = function(x) mean(x) + (sd(x)/sqrt(n_rows)),
                     fun.min = function(x) mean(x) - (sd(x)/sqrt(n_rows)),
                     geom = "errorbar",
                     position = position_dodge(width = 0.5),
                     width = .15,
                     size = 0.8,
                     #color = rep(c(dark_colors[1], dark_colors[2]),1),
                     #color = rep(c(dark_colors[1], dark_colors[2]),times =4),
                     show.legend = FALSE) +
        scale_color_manual(values = rep(colour,times =4)) +
        scale_y_continuous(name = "Accuracy [%]") +
        scale_x_discrete(name = "Conditions", 
                         labels = lab) +
        #labs(color="Group") +
        theme_cowplot(font_size = 16, font_family = "Arial") +
        theme(axis.line = element_line(colour = 'black', size = 1),
              axis.ticks = element_line(colour = 'black', size = 1),
              axis.text = element_text(face="bold"),
              legend.position= "none")
}

blam_accuracy_points_bimodal_axis <- function (data, lab, colour) {
    
    
    #Get the list of eperimental conditions from the dataframe
    # CAUTION: ASSUMED COLUMN NAME = cond
    conditions <- unique(data$cond)
    
    #LEGEND generated automatically, (re)move it with function call
    # e.g. plot <- beh_reading_speed_points(data = DF) + 
    # theme(legend.position = "none")
    
    n_rows <- data %>% nrow()
    
    data %>% 
        ggplot(aes(x = interaction(axis,cond), y = accu_percent, 
                   shape = axis, 
                   color = cond)) +
        geom_point(#position = position_jitterdodge(jitter.width=0.15,
            #                               dodge.width = 0.5),
            size = 2.5,
            alpha = 0.8) +
        # Subject lines? 
        geom_line(aes(group = subject), 
                  # position = position_jitterdodge(jitter.width=0.15,
                  #                                 dodge.width = 0.5), 
                  size = 0.2) +
        # Subject labels?
        #geom_text(aes(label=subject), color = 'black', size=3, hjust=1, vjust=1) +
        stat_summary(fun = "mean", 
                     geom = "crossbar", 
                     position = position_dodge(width = 0.5),
                     width = .45)+#, 
        #color = rep(c(dark_colors[1], dark_colors[2]),1)) +
        #color = rep(c(dark_colors[1], dark_colors[2]),times =4)) +
        stat_summary(fun.max = function(x) mean(x) + (sd(x)/sqrt(n_rows)),
                     fun.min = function(x) mean(x) - (sd(x)/sqrt(n_rows)),
                     geom = "errorbar",
                     position = position_dodge(width = 0.5),
                     width = .15,
                     size = 0.8,
                     #color = rep(c(dark_colors[1], dark_colors[2]),1),
                     #color = rep(c(dark_colors[1], dark_colors[2]),times =4),
                     show.legend = FALSE) +
        scale_color_manual(values = rep(colour,times =4)) +
        scale_y_continuous(name = "Accuracy [%]") +
        scale_x_discrete(name = "Conditions", 
                         labels = lab) +
        #labs(color="Group") +
        theme_cowplot(font_size = 16, font_family = "Arial") +
        theme(axis.line = element_line(colour = 'black', size = 1),
              axis.ticks = element_line(colour = 'black', size = 1),
              axis.text = element_text(face="bold"),
              legend.position= "none")
}