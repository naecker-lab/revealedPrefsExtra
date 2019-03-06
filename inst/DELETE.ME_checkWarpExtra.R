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

# not sure what this is doing since there should be two values
# if the grouping worked properly
experiment_data %>%
  group_by(subject) %>%
  checkWarpExtra(x1, x2, p1, p2)


# this seems to be working as we would want
# Note: checkWarp returns a list of 3 values:
  # violation
  # type
  # afrait.par
# We basically only care about violation, although having
# the function return TRUE for violating warp seems a little
# unintutive to me

experiment_data %>%
  group_by(subject) %>%
  summarise(n = n(), # just checking to make sure the grouping is happening correctly
            violates_warp = checkWarpExtra(., x1, x2, p1, p2)$violation)


# if we went with writing the function this way, it would be pretty easy
# to clean up what gets returned to make the output a bit nicer


#-------------------------------------------------------------------------
# With checkWarpExtra built without spread
#-------------------------------------------------------------------------
  # Pros: can have optional agruments passed to checkWarp
  # Cons: can't figure out grouping

checkWarpExtra2 <- function(matrix_list, ...){
  if(!is.list(matrix_list) | is.data.frame(matrix_list))
  {stop("matrix_list must be a list object containing x, a matrix of quantities
        and p, a matrix of prices")}
  
  if(is.null(matrix_list$x) | is.null(matrix_list$p))
  {stop("matrix_list must be a list object containing x, a matrix of quantities
        and p, a matrix of prices")}
  checkWarp(matrix_list$x, matrix_list$p, ...)
  }

experiment_data %>%
  spread_data(x1, x2, p1, p2) %>%
  checkWarpExtra2()


experiment_data %>%
  spread_data(x1, x2, p1, p2) %>%
  checkWarpExtra2(afriat.par = .7)


# this is weird to me since there should be two outputs... don't know where the
# other value is coming from. same issue as above
experiment_data %>%
  group_by(subject) %>%
  spread_data(x1, x2, p1, p2) %>%
  checkWarpExtra2()

# issue with grouping b/c input is a list and not a df
experiment_data %>%
  group_by(subject) %>%
  spread_data(x1, x2, p1, p2) %>%
  summarise(warp_violation = checkWarpExtra2())

