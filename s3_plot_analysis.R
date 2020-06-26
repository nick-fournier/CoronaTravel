library(ggplot2)
library(ggpubr)
library(ggrepel)



#
load("./data/merged_covidtrips.RData")

#### Plots

#Covid by voting, region
ggplot(data = covidtrips.division, aes(x = date_val, y = 1e6*new_cases/pop, color = divlabel)) +
  geom_point(size=0.1, alpha = .5) +
  geom_smooth(method = "loess", formula = y~x, se = F, span = 0.5) +
  scale_y_continuous("Cases per 1,000,000") +
  scale_x_date(NULL,limits = as.Date(c('2020-01-01',Sys.Date())), breaks = "month", date_labels = "%b") +
  coord_cartesian(xlim = as.Date(c('2020-03-01',Sys.Date()))) +
  scale_color_manual(NULL, values = rev(RColorBrewer::brewer.pal(9, name = "RdYlBu")),
                     limits = unique(covidtrips.division[order(per_gop), divlabel]),
                     labels = unique(covidtrips.division[order(per_gop), 
                                                         paste0(round(100*per_gop),"% GOP in 2016, ", divlabel)])) +
  theme_bw() +  
  theme(legend.position = "right")


#Covid vs voting

#rate of growth in last 30 days
rates.state <- rbindlist(lapply(split(covidtrips.state, covidtrips.state$state), function(div) {
  data.table("state" = div$state[1], "per_gop" = div$per_gop[1], 
             "rate" = coef(lm((1e6*new_cases/(pop-cases)) ~ date_val, div[ date_val > Sys.Date()-30, ]))[2])
}))

ggplot(rates.state, aes(x = per_gop, y = rate, color = per_gop, label = state)) + 
  geom_point() + geom_text_repel() +
  geom_hline(yintercept = 0,linetype = "dashed") + geom_vline(xintercept = 0.5, linetype = "dashed") +
  scale_x_continuous("Percent Trump voters in 2016", labels = scales::percent_format(accuracy = 1), breaks = seq(0, 1, by = 0.1)) +
  scale_y_continuous("30-day average growth per 1,000,000 persons", labels = scales::percent) +
  geom_smooth(method = "lm", formula = y~x, se = F, color = "black") +
  scale_color_gradient2("Percent Trump voters in 2016", low = "#2b83ba", mid = "#5e3c99", high = "#d7191c", midpoint = 0.5) +
  theme_bw()



#National trips and covid comparison
ggplot(data = melt(covidtrips.nation[ , list(date_val, 
                                             "Daily cases per 100,000" = 100000*new_cases/pop, 
                                             "Daily trips per capita" = trips/pop)], id.var = "date_val"),
       aes(x = date_val, y = value, color = variable)) + 
  geom_point(size = 0.1) +
  geom_smooth(method = "loess", formula = y~x, se = F, span = 0.25, size = 0.5) +
  scale_x_date(NULL,limits = as.Date(c('2020-01-01',Sys.Date())), breaks = "1 weeks", date_labels = "%m/%d") +
  scale_y_continuous("Number per capita") +
  scale_color_brewer(NULL, palette = "Set1") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position = "bottom")


#Regional division trips and covid comparison
plot.divs <- lapply(split(covidtrips.division, covidtrips.division$divlabel), function(div) {
  ggplot(data = melt(div[, list(date_val, divlabel,
                                "Daily cases per 100,000" = 100000*new_cases/pop, 
                                "Daily trips per capita" = trips/pop)], id.var = c("date_val","divlabel")),
         aes(x = date_val, y = value, color = variable)) + 
    geom_point(size = 0.1) +
    geom_smooth(method = "loess", formula = y~x, se = F, span = 0.25, size = 0.5) +
    scale_x_date(NULL,limits = as.Date(c('2020-01-01',Sys.Date())), breaks = "1 weeks", date_labels = "%m/%d") +
    scale_y_continuous("Number per capita") +
    scale_color_brewer(NULL, palette = "Set1") +
    ggtitle(div$divlabel[1]) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
})
ggarrange(plotlist = plot.divs, ncol = 3, nrow = 3, common.legend = T, legend = "bottom")


#national trips vs covid correlation
ggplot(data = covidtrips.nation[cases > 1000 & trips > 0 , ],
       aes(x = trips/pop, y = 100000*new_cases/pop)) + 
  geom_point(size = 0.1) +
  geom_smooth(method = "lm", formula = y ~ x, se=F) +
  #scale_x_log10() + scale_y_log10() +
  scale_color_brewer(palette = "Set1") +
  theme_bw()

#Regional trips vs covid correlation
plot.tripvscovid <- lapply(split(covidtrips.division, covidtrips.division$divlabel), function(div) {
  ggplot(data = div[cases>1000], aes(x = trips/pop, y = 100000*new_cases/pop)) + 
    geom_point(size = 0.1) +
    geom_smooth(method = "lm", formula = y ~ x, se=F) +
    scale_x_continuous("Trips per capita") +
    scale_y_continuous("Cases per 100,000 people")
  scale_color_brewer(NULL, palette = "Set1") +
    ggtitle(div$divlabel[1]) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
})
ggarrange(plotlist = plot.tripvscovid, ncol = 3, nrow = 3, common.legend = T, legend = "bottom")





