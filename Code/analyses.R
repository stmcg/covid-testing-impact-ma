library("readxl")
library('meta')
library('stringr')

dat_uptake <- data.frame(read_excel("../Data/Extraction Sheet WHO SR.xlsx", sheet = 'Study Characteristics'))
dat_pos <- data.frame(read_excel("../Data/Extraction Sheet WHO SR.xlsx", sheet = 'Testing Characteristics'))

colnames(dat_uptake) <- dat_uptake[1,]; colnames(dat_uptake)[1] <- 'ID'
colnames(dat_pos) <- dat_pos[1,]; colnames(dat_pos)[1] <- 'ID'

dat_uptake <- dat_uptake[-1,]
dat_pos <- dat_pos[-1,]

dat_uptake_red <- data.frame(ID = c('a02', 'a05', 'a07', 'a12', 'a12'), 
                             Sub_1 = c(1, 0, 0, 1, 2))
dat_pos_red <- data.frame(ID = c('a02', 'a02', 'a03', 'a05', 'a11', 'a14', 'a14', 'a14'), 
                          Sub_1 = c(1, 2, 0, 0, 1, 1, 2, 3))

dat_uptake_red <- merge(dat_uptake_red, dat_uptake)
dat_pos_red <- merge(dat_pos_red, merge(dat_pos, dat_uptake))

dat_uptake_red$`Setting test uptake` <- str_replace_all(dat_uptake_red$`Setting test uptake`, '\r\n', '\n')
dat_uptake_red$events <- as.numeric(dat_uptake_red$Tested)
dat_uptake_red$n <- as.numeric(dat_uptake_red$Included)
dat_uptake_red <- dat_uptake_red[order(dat_uptake_red$events / dat_uptake_red$n, decreasing = TRUE), ]

dat_pos_red$events <- as.numeric(dat_pos_red$Positive)
dat_pos_red$n <- as.numeric(dat_pos_red$`Total number of tests performed`)
dat_pos_red <- dat_pos_red[order(dat_pos_red$events / dat_pos_red$n, decreasing = TRUE), ]

get_study_name <- function(df){
  split_author <- function(author){
    Author_name <- NULL
    for(i in 1:length(author)){
      Author_name <- c(Author_name, strsplit(author[i], split = ',')[[1]][1])
    }
    return(Author_name)
  }
  return(paste(split_author(df$Author), 
               paste(df$ID, unlist(df$Sub_1), sep = '.'), 
               sep = ', '))
}

# Uptake
res_uptake <- metaprop(event = events, n = n, method = 'GLMM', fixed = FALSE,
                       pscale = 100, studlab = get_study_name(dat_uptake_red),
                       data = dat_uptake_red)

# Positives
res_pos <- metaprop(event = events, n = n, method = 'GLMM', fixed = FALSE,
                    pscale = 100, studlab = get_study_name(dat_pos_red), 
                    data = dat_pos_red)

pdf('../Results/Forest_Uptake.pdf', width = 15, height = 7)
forest(res_uptake, 
       xlab = 'Uptake (%)', digits = 1, pooled.events = TRUE, smlab = '', hetstat	= F, spacing = 4,
       colgap.forest = '5mm', colgap.left = '5mm',
       leftcols = c('studlab', 'Setting test uptake', 'Location', 'event', 'n'), 
       leftlabs = c('Data Set', 'Setting', 'Country', 'Tested', 'Total'), 
       just.addcols.left = c('left', 'left', 'left', 'center'),
       rightlabs = c('Uptake (%)', '95% CI'), xlim = c(20, 80), addrow = F, addrow.overall = F)
dev.off()

pdf('../Results/Forest_Pos.pdf', width = 15.5, height = 3.75)
forest(res_pos, 
       xlab = 'Test Positivity (%)', digits = 1, pooled.events = TRUE, smlab = '', hetstat	= F, 
       colgap.forest = '5mm', colgap.left = '5mm',
       leftcols = c('studlab', 'Setting_test positivity', 'Symptoms_test positivity', 'event', 'n'), 
       leftlabs = c('Data Set', 'Setting', 'Symptoms', 'Positives', 'Total'), 
       just.addcols.left = c('left', 'left', 'left', 'center'),
       rightlabs = c('Test Positivity (%)', '95% CI'), xlim = c(0, 1))
dev.off()
