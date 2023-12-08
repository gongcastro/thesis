library(dplyr)
library(tidyr)

# stimuli lists-----------------------------------------------------------------

study_1 <- targets::tar_read(stimuli) |> 
	filter(test_language=="English") |> 
	distinct(prime, target, distractor, condition = trial_type) |> 
	mutate(condition = if_else(condition!="Unrelated",
							   "related", tolower(condition))) |> 
	pivot_longer(-condition,
				 names_to = "role",
				 values_to = "word")

mp <- tibble::tribble(
	~target, ~distractor, ~prime_related, ~prime_unrelated,
	"Ball (97%)", "Truck (61%)", "Bee (61%)", "Comb (30%)",
	"Bear (52%)", "Duck (91%)", "Boat (62%)", "Pen (51%)",
	"Bike (70%)", "Hand (76%)", "Bowl (57%)", "Cat (93%)",
	"Bird (88%)", "Sheep (70%)", "Bib (75%)", "Cake (55%)",
	"Book (95%)", "Foot (70%)", "Bath (93%)", "Cow (85%)",
	"Boot (53%)", "Fork (44%)", "Bed (83%)", "Pig (78%)",
	"Bus (98%)", "Sock (91%)", "Bin (70%)", "Deer (11%)",
	"Car (96%)", "Eye (85%)", "Cake (55%)", "Bed (83%)",
	"Coat (78%)", "Tree (66%)", "Cow (85%)", "Bin (70%)",
	"Cot (71%)", "Train (66%)", "Comb (30%)", "Bee (61%)",
	"Cup (79%)", "Shoe (98%)", "Cat (93%)", "Teeth (75%)",
	"Dog (98%)", "Hen (60%)", "Door (87%)", "Boat (62%)",
	"Doll (60%)", "Chair (81%)", "Deer (11%)", "Bib (75%)",
	"Peas (47%)", "Hat (88%)", "Pig (78%)", "Bath (93%)",
	"Pup (20%)", "Mouse (54%)", "Pen (51%)", "Door (87%)",
	"Toe (69%)", "Key (73%)", "Teeth (75%)", "Bowl (57%)"
) |> 
	pivot_longer(matches("prime_"),
				 names_to = "condition",
				 values_to = "prime",
				 names_prefix = "prime_") |> 
	pivot_longer(-condition,
				 names_to = "role",
				 values_to = "word",
				 values_transform = tolower) |> 
	separate(word, c("word", "aoa"), sep = " ") |> 
	mutate(aoa = as.numeric(gsub("\\(|\\)|%", "", aoa))*0.01)

stim <- list("M&P (2010, 2011)" = mp,
			 "Study 1" = study_1) |> 
	bind_rows(.id = "study")

# stimuli stats ----------------------------------------------------------------

stats <- readxl::read_xlsx(file.path("data-raw", "stimuli", "stim-stats-oxf.xlsx")) |>
	select(word = item, ocdi18_comp)

childes_corp <- get_childes_corpora(token = unique(stim$word),
									languages = "eng",
									load_previous = FALSE) 

childes_freq <- get_frequency_childes(childes_corp,
									  token = unique(stim$word),
									  languages = "eng") |> 
	rename(word = childes_lemma)


out <- left_join(stim, stats, by = join_by(word)) |> 
	left_join(childes_freq, by = join_by(word)) |> 
	mutate(aoa = if_else(is.na(aoa), ocdi18_comp, aoa)) |> 
	select(study, role, condition, word, freq, aoa) |> 
	distinct(study, role, condition, word, .keep_all = TRUE)


write_csv(out, file.path("manuscript", "_assets", "data", "mp-stimuli.csv"))
