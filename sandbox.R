
library(tidyverse)
library(readxl)
library(lpSolveAPI)

# the aim of this script is to find the optimal repartition of 
# vegetable in a market gardenning setup, based on different 
# targets. 

### PARAMETERS
# Vegetable data
veg <- read_xlsx("www/vegetable_data.xlsx")

# Total surface, in m2
surf_tot <- 500 

# number of different vegetables
veg_min <- length(unique(veg$vegetable))

# min surface for one parcel, in m2
surf_min <- 10
surf_max <- (surf_tot / veg_min ) * 1.3
if(surf_min > surf_max) surf_min <- surf_max

# Max cost
cost_max <- 1400
if(cost_max < sum(surf_min * veg$price)){
  cost_max <- sum(surf_min * veg$price)
}



### OPTIMISATION

#define the datasets
lprec <- make.lp(0, veg_min)
lp.control(lprec, sense="max")

# Objective function
set.objfn(lprec, veg$yield * veg$price) # optimize on price
#set.objfn(lprec, veg$yield) # optimize on production
#set.objfn(lprec, veg$calorie) # optimize on calories

#lp.control(lprec, sense="min")
#set.objfn(lprec, veg$water) # optimize on water (minimal)

# Total surface contrains
add.constraint(lprec, rep(1, veg_min), "=", surf_tot) 

# price constrains
add.constraint(lprec, veg$cost, "<=", cost_max)

# Individual surface contrains
for(i in c(1:veg_min)){
  inds <- rep(0, veg_min)
  inds[i] <- 1
  add.constraint(lprec, inds, ">=", surf_min)
  add.constraint(lprec, inds, "<=", surf_max)

}

# Individual production constrain. Same production for each veg
for(i in c(2:veg_min)){
  inds <- rep(0, veg_min)
  inds[i] <- -1
  inds[1] <- 1
  inds <- inds * veg$yield
  add.constraint(lprec, inds, "=", 0)
}



lprec
solve(lprec)
get.objective(lprec)
round(get.variables(lprec))

# total surface
opt_surface <- sum(round(get.variables(lprec)))

# total production
opt_prod <- sum(get.variables(lprec) * veg$yield)

# total price
opt_revenue <- sum(get.variables(lprec) * veg$yield * veg$price)

# total calories
opt_cal <- sum(get.variables(lprec) * veg$yield * veg$calorie)


dat <- data.frame(name = veg$vegetable, 
                  surface = round(get.variables(lprec)))


ggplot(dat, aes(x="", y=surface, fill=name)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(surface, " m2")), 
            position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) 



11*0.1 + 11*0.9*0.2
11*0.25 + 11*0.75*0.2




