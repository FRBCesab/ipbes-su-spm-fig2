#' IPBES Sustainable Use Assessment - Figure SPM.1
#' 
#' This R script reproduces the Figure SPM.1 of the IPBES Sustainable Use 
#' Assessment. This figure shows the percentage of Sustainable Development 
#' Goals (SDG) targets underpinning the sustainable use of of wild species.
#' 
#' @author Nicolas Casajus <nicolas.casajus@fondationbiodiversite.fr>
#' @date 2022/01/18



## Install `remotes` package ----

if (!("remotes" %in% installed.packages())) install.packages("remotes")


## Install required packages (listed in DESCRIPTION) ----

remotes::install_deps(upgrade = "never")


## Load project dependencies ----

devtools::load_all(".")


## Load data ----

infos <- readxl::read_xlsx(here::here("data", "data-fig_spm1.xlsx"), 
                           sheet = "Infos")

tab   <- readxl::read_xlsx(here::here("data", "data-fig_spm1.xlsx"), 
                           sheet = "Targets")
tab <- as.data.frame(tab)


## Create categories (color bars) ----

niveaux <- c("0--0", "1--2", "0--1", "0--2", "2--2")

tab$"State" <- paste(tab$"Current", tab$"Recommendation", sep = "--")


## Summarize data ----

dat <- tapply(tab$"State", list(tab$"SDG", tab$"State"), length, default = 0)
dat <- as.data.frame(dat)
dat <- dat[ , niveaux]
dat$"SDG" <- rownames(dat)


## Append information ----

dat <- merge(dat, infos, by = "SDG", all = TRUE)
dat <- dat[ , c(niveaux, "SDG", "Label")]
dat$ordre <- as.numeric(gsub("SDG", "", dat$SDG))
dat <- dat[order(dat$ordre), ]

dat[ , "0--1"] <- dat[ , "0--1"] + dat[ , "1--2"]
dat <- dat[ , -2]

niveaux <- c("0--2", "0--1", "2--2", "0--0")

dat <- data.frame(dat[ , niveaux], dat[ , 5:ncol(dat)])
colnames(dat)[1:length(niveaux)] <- niveaux

dat[ , niveaux] <- t(apply(dat[ , niveaux], 1, function(x) round(100 * x / sum(x), 2)))

tab <- dat[nrow(dat):1, ]
colnames(tab) <- tolower(colnames(tab))


## Plot ----

png(here::here("figures", "ipbes_su-spm_fig1.png"), width = 12, height = 6, 
    res = 600, pointsize = 14, units = "in")

par(family = "serif", mar = c(2, 14, 0.5, 0.5), yaxs = "i", mgp = c(1, 0, 0), 
    col = "#333333")

plot(0, xlim = c(0, 100), ylim = c(.5, nrow(tab) + .5), type = "n", bty = "n", 
     axes = FALSE, ann = FALSE)

couleurs <- c("#673400", "#B5651D", "#cccccc", "white")

for (i in 1:nrow(tab)) {
  
  for (j in 1:length(niveaux)) {
    
    if (j == 1) {
      
      rect(0, i - 0.5, tab[i, j], i + 0.5, border = NA, col = couleurs[j])
      
    } else {
      
      rect(sum(tab[i, 1:(j - 1)]), i - 0.5, sum(tab[i, 1:j]), i + 0.5, 
           border = NA, col = couleurs[j])
    }
  }
  
  rect(0, i - 0.5, 100, i + 0.5, col = NA, border = "white", lwd = 10)
  lines(y = rep(i - 0.5, 2), x = c(-44, 100), xpd = TRUE)
  
  text(0, i, paste(tab[i, "sdg"], tab[i, "label"]), pos = 2, xpd = TRUE,
       cex = .8, font = 2)
  
  axis(1, seq(0, 100, 20), paste0(seq(0, 100, 20), "%"), cex.axis = .8, lwd = 0)
}

lines(y = rep((i + 1) - 0.5, 2), x = c(-44, 100), xpd = TRUE)

x_lab <- paste0("Percentage of targets (by SDG) underpinning sustainable use ",
                "of wild species")

text(50, -0.5, x_lab, font = 2, cex = .9, xpd = TRUE)

dev.off()
