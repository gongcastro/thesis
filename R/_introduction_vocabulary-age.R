library(dplyr)
library(tidyr)
library(brms)
library(tidybayes)

bvq_data <- readRDS(file.path("data-raw", "bvq.rds"))

fits <- bvq_data$vocabulary |> 
	inner_join(select(bvq_data$logs, child_id, response_id, lp, age, version),
			   by = join_by(child_id, response_id)) |> 
	drop_na() |> 
	filter(!grepl("long", version),
		   between(age, 10, 35),
		   lp!="Other") |> 
	select(child_id, response_id, type, age, lp, matches("count")) |> 
	pivot_longer(matches("count"),
				 names_to = "measure",
				 values_to = "count") %>%
	group_by(type, measure) |> 
	nest() |> 
	mutate(model = map(data,
					   function(x) {
					   	suppressMessages({
					   		brm(count ~ age * lp + (1 + age | child_id),
					   			data = x,
					   			family = poisson(),
					   			iter = 500,
					   			chains = 2, 
					   			cores = 2,
					   			seed = 1234)
					   	})
					   }, .progress = TRUE))

saveRDS(fits, file.path("results", "introduction", "fit_vocabulary-age-list.rds"))

nd <- expand_grid(age = seq(10, 35, length.out = 100),
				  lp = c("Monolingual", "Bilingual"))
epreds <- fits |> 
	mutate(epreds = map(fits, 
						function(x) {
							add_epred_draws(nd, x, re_formula = NA)
						}, .progress = TRUE)) |> 
	unnest(epreds, .id = "model_name")
