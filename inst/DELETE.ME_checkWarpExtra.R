library(tibble)
library(dplyr)

# make sure to load revealedPrefsExtra to get access to the function checkWarpExtra

#-------------------------------------------------------------------------
# checkWarpExtra (with spread_data included within the function)
#-------------------------------------------------------------------------
  # Pros: one step. grouping works (ish)
  # Cons: unsure of how to include optional arguments in an inutive way
    # (afriat.par and method options)


experiment_data<-
  tibble(subject = c(1, 1, 1, 2, 2),
         x1 = c(1:5),
         x2 = c(5:9),
         p1 = c(5:9),
         p2 = c(5:9))

checkWarpExtra(experiment_data, x1, x2, p1, p2)

experiment_data %>%
  checkWarpExtra(x1, x2, p1, p2)

# For some reason, when using summarise function, "." is passing in the ungrouped data, 
# and so, the results are not correct. 
experiment_data %>%
  group_by(subject) %>%
  summarise(violates_warp = checkSarpExtra(., x1, x2, p1, p2)$violation)

# however, from seaching online, I found that using do(data.frame()) works similarly
# but the grouped data gets correctly passed in. 

#Despite my online research, I don't really understand why "do" works with grouping
# and "summarise" doesn't...I've been meaning to ask Professor Kabacoff, but he's not
# been on campus for a lot of this week due to a family emergency. 
experiment_data %>%
  group_by(subject) %>%
  do(data.frame(violates_warp = checkSarpExtra(., x1, x2, p1, p2)$violation))


