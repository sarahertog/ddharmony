# this function assesses age heaping and accordingly applies smoothing
# as described in flowchart XX


census_workflow_adjust_smooth <- function(popM,
                                          popF,
                                          Age,
                                          bachi_age_child = 3:17, # age range for bachi index for children
                                          bachi_age_adult = 23:77, # age range for bachi index for adults
                                          age_ratio_age_child = c(0,10), # age range for age ratio score for children
                                          age_ratio_age_adult = c(15,70), # age range for age ratio score for adults
                                          EduYrs) { # average years of education (used as a criterion for level of smoothing) 


# intialize bachi
bachi_child <- NA
bachi_adult <- NA

# if inputs are by single year of age
if (is_single(Age)) {
  
  # assess single year age heaping for children and smooth accordingly 
  pop_smooth_child <- getSmoothedPop1 (Age = Age,
                                       popF = popF,
                                       popM = popM, 
                                       bachi_age = bachi_age_child, 
                                       age_ratio_age = age_ratio_age_child,
                                       EduYrs = EduYrs, 
                                       subgroup = "child") 
  bachi_child <- pop_smooth_child$bachi
  
  # assess single year age heaping for adults and smooth accordingly 
  pop_smooth_adult <- getSmoothedPop1 (Age = Age,
                                       popF = popF,
                                       popM = popM,  
                                       bachi_age = bachi_age_adult,
                                       age_ratio_age = age_ratio_age_adult,
                                       EduYrs = EduYrs, 
                                       subgroup = "adult") 
  bachi_adult <- pop_smooth_adult$bachi
  
} else {
  
  # assess grouped age heaping for children and smooth accordingly 
  pop_smooth_child <- getSmoothedPop5(Age = Age,
                                      popF = popF,
                                      popM = popM, 
                                      age_ratio_age = age_ratio_age_child,
                                      EduYrs = EduYrs, 
                                      subgroup = "child")  
  
  # assess grouped age heaping for adults and smooth accordingly 
  pop_smooth_adult <- getSmoothedPop5(Age = Age,
                                      popF = popF,
                                      popM = popM,  
                                      age_ratio_age = age_ratio_age_adult,
                                      EduYrs = EduYrs, 
                                      subgroup = "adult") 
  
}

# blend the smoothed child and adult series, with transition at ages 15-19
wts <- c(rep(1,16),0.8, 0.6, 0.4, 0.2, rep(0, max(Age)-19))

popM_smoothed <- (pop_smooth_child$popM_smooth * wts) + (pop_smooth_adult$popM_smooth * (1-wts))
popF_smoothed <- (pop_smooth_child$popF_smooth * wts) + (pop_smooth_adult$popF_smooth * (1-wts))

# re-adjust to ensure that after smoothing we are still matching the total
popM_smoothed <- popM_smoothed * sum(popM)/sum(popM_smoothed)
popF_smoothed <- popF_smoothed * sum(popF)/sum(popF_smoothed)

pop_smoothed <- list(Age = 0:105,
                     popF_smoothed = popF_smoothed,
                     popM_smoothed = popM_smoothed,
                     bachi_child = bachi_child,
                     bachi_adult = bachi_adult,
                     ageRatio_adult_orig = pop_smooth_adult$AgeRatioScore_orig,
                     ageRatio_child_orig = pop_smooth_child$AgeRatioScore_orig,
                     ageRatio_adult_mav2 = pop_smooth_adult$AgeRatioScore_mav2,
                     ageRatio_child_mav2 = pop_smooth_child$AgeRatioScore_mav2,
                     best_smooth_adult   = pop_smooth_adult$best_smooth_method,
                     best_smooth_child   = pop_smooth_child$best_smooth_method)


return(pop_smoothed)

}

