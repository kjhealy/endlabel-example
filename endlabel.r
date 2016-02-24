library(ggplot2)
library(scales)
library(dplyr)

### --------------------------------------------------
### Cosmetics
### --------------------------------------------------
makeFootnote <- function(footnoteText=
                         format(Sys.time(), "%d %b %Y"),
                         size= .7, color= grey(.5)) {
   require(grid)
   pushViewport(viewport())
   grid.text(label= footnoteText ,
             x=unit(1,"npc") - unit(2, "mm"),
             y=unit(2, "mm"),
             just=c("right", "bottom"),
             gp=gpar(cex= size, col=color))
   popViewport()
}

credit <- function() {
  return(makeFootnote("\nSource: CDC WONDER. Age-adjusted. Excludes DC. Kieran Healy. http://kieranhealy.org"))
}


my.colors <- function (palette = "cb")
{
    cb.palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
        "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    rcb.palette <- rev(cb.palette)
    bly.palette <- c("#E69F00", "#0072B2", "#999999", "#56B4E9",
        "#009E73", "#F0E442", "#D55E00", "#CC79A7")
    if (palette == "cb")
        return(cb.palette)
    else if (palette == "rcb")
        return(rcb.palette)
    else if (palette == "bly")
        return(bly.palette)
    else stop("Choose cb, rcb, or bly ony.")
}

###--------------------------------------------------
### Data
###--------------------------------------------------

theme_set(theme_minimal())

state.regions <- read.csv("data/states-and-regions.csv")

data <- read.csv("data/CDC-Wonder–Assault-Rates-by-State-1999-2013.csv")

ind <- match(data$State, state.regions$State)
data$Region <- state.regions$Region[ind]
data$Region <- factor(data$Region,
                      levels = c("Northeast", "Midwest", "West", "South"),
                      ordered = TRUE)
data$Abbr <- state.regions$State.Abbr[ind]

## Set up regions, get rid of DC
data.reg <- data %>% filter(Abbr != "DC") %>%
    group_by(Region, Year) %>%
    summarize(Deaths = mean(Deaths, na.rm =TRUE),
              Population = mean(Population),
              Crude = mean(Crude, na.rm =TRUE),
              Adjusted = mean(Adjusted, na.rm = TRUE))

## We create a tiny data frame for the regions, consisting of one year
## of "data"
data.lab <- data %>% filter(Year == 2013)

## Then increment the year slightly to create a gap for the labels
data.lab$Year <-  data.lab$Year

pdf(file = "figures/assault-deaths-state-level-lineplots-by-region-facet.pdf",
    height = 6, width = 13)
p0 <- ggplot(subset(data, Abbr !="DC"),
            aes(x=Year,
                y=Adjusted,
                color = Region,
                fill = Region,
                group = State))

p1 <- p0 + geom_line(alpha = 0.7, lty = 3) +
    geom_text(data=subset(data.lab, Abbr !="DC"),
              aes(x=Year+0.3,
                  y=Adjusted+0.15, # add a little space
                  label=Abbr),
              size = 1.8) +
    geom_smooth(data=data.reg,
                aes(x=Year,
                y=Adjusted,
                color = Region,
                fill = Region,
                group = Region),
                method = "loess",
                se = FALSE,
                lwd = 1.5) +
    facet_grid(~ Region) +
    scale_fill_manual(values = my.colors()) +
    scale_color_manual(values = my.colors()) +
    labs(x = "",
         y = "Rate per 100,000 population") +
    guides(color = FALSE,
           fill = FALSE) +
    theme(axis.text.x = element_text(size=rel(0.7))) +
    ggtitle("State-Level Assault Death Rates by Census Region, 1999-2013\n")

print(p1)
credit()
dev.off()

png(file="figures/assault-deaths-us-ts-region.png", height=1600, width=3200,
    res=300, pointsize=11)
print(p1)
credit()
dev.off()
