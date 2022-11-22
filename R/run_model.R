#Script to transfer the data currently use it

run_results = ("C:/git/PacificCommunity/ofp-sam/ofp-sam-skj-2022-synthesis/model_ouput/8_regions_results")

input_data <- r4ss::copy_SS_inputs(
  dir.old = "C:/git/PacificCommunity/ofp-sam/ofp-sam-skj-2022-synthesis/model_input/8_regions",
  dir.new = run_results,
  verbose = FALSE,
  overwrite = TRUE
)

setwd(run_results)
#run with no hessian
system("ss.exe -nohess") #version 3.20

cat <- SS_output(run_results, covar=FALSE)
SS_plots(cat)

SS_plots(cat,plot = c(1:20,22:26))
SS_plots(cat,plot = c(21))
cat$parameters

cat$tagdbase1 %>% head
cat$parameters %>% dplyr::filter(grepl("Size", Label, ignore.case = TRUE))
class(cat)

cat[["cpue"]] %>% filter(Fleet == 33) %>% mutate(penalty = (1/SE_input^2)/2)%>%select(Obs, SE_input,penalty)

cat$Dirichlet_Multinomial_pars