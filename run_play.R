## EDIT HERE
## ------------------------------------------------------------------------
skip <- c(99)
for (i in 1:nrow(lids)) {
  row_of_this <- i
  if (i %in% skip) next
  comp_no <- 1
  tab_number <- 1+1 # tab 3 is the second conference in the league, etc.
  source("play2.R", echo = TRUE)  
}
beepr::beep(4)
