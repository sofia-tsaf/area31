## Prepare plots and tables for report

## Before: current_status.csv, results.rds, stock_timeseries.csv (output)
## After:  bbmsy.png, cpue_1.png, driors_1.png, posterior_1.png,
##         status_by_year.png, status_sofia.png, status_sraplus.png,
##         stock_posterior.pdf, stock_timeseries.pdf (report)

library(icesTAF)
library(dplyr)        # mutate
library(egg)          # ggarrange
library(ggplot2)
library(purrr)        # map2, walk2
library(sraplus)      # plot_driors, plot_prior_posterior, plot_sraplus
source("utilities.R") # plotProp

mkdir("report")

stocks <- readRDS("output/results.rds")

## Plot data and priors, posteriors, and CPUE
plot_driors(stocks$driors[[1]]) # stock 1 is Sardinella aurita
ggsave("report/driors_1.png")
plot_prior_posterior(stocks$sraplus_fit[[1]], stocks$driors[[1]])
ggsave("report/posterior_1.png")
taf.png("cpue_1")
with(stocks$driors[[1]], plot(catch[years %in% effort_years] / effort))
dev.off()

## Barplots of stock status
current_status <- read.taf("output/current_status.csv")
taf.png("status_sraplus")
current_status$status <- ordered(current_status$status,
                               c("underfished","fully fished","overfished"))
barplot(prop.table(table(current_status$status)), col=c("green","yellow","red"))
dev.off()
taf.png("status_sofia")
results_sofia <- c(0.025, 0.35, 0.625)
names(results_sofia) <- c("underfished", "fully fished", "overfished")
barplot(results_sofia, col=c("green","yellow","red"))
dev.off()

## Plot posteriors and time series for each stock
stocks <- stocks %>%
  mutate(plot_prior_posterior_plot=
           map2(sraplus_fit,driors,plot_prior_posterior))
savefoo <- function(stock, plot) print(plot + labs(title=stock))
pdf("report/stock_posterior.pdf")
walk2(stocks$stock, stocks$plot_prior_posterior_plot, savefoo)
dev.off()
stocks <- stocks %>%
  mutate(sraplus_fit_plot = map(sraplus_fit, plot_sraplus))
savefoo <- function(stock, plot) print(plot + labs(title=stock))
pdf("report/stock_timeseries.pdf")
walk2(stocks$stock, stocks$sraplus_fit_plot, savefoo)
dev.off()

## Plot time series for each stock
newResTab <- read.taf("output/stock_timeseries.csv")
taf.png("status_by_year")
p1 <- plotProp(newResTab, method="effEdepP", cats=3, type="prop")
p2 <- plotProp(newResTab, method="effEdepP", cats=3, type="all")
ggarrange(p1, p2, ncol=1)
dev.off()

## Overlay B/Bmsy time series of all stocks in a single plot
ggplot(newResTab, aes(x=yr, y=bbmsy, colour=Stock, group=Stock)) +
  geom_line(show.legend = TRUE) +
  geom_hline(yintercept=0.8, linetype="dashed", color = "red", size=2) +
  geom_hline(yintercept=1.2, linetype="dashed", color = "green", size=2)
ggsave("report/bbmsy.png")
