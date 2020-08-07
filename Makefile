r:
	open -na Rstudio team-a-final-proj.Rmd

r1:
	open -na Rstudio First-look-at-dataset.Rmd
proposal:
	open Project-Proposal.html

cmd:
	@echo 'conda activate stat420'

zip: 
	zip team-a-final-proj.zip team-a-final-proj.* autos_test.csv autos_train.csv misc_functions.R

clean:
	-rm team-a-final-proj.zip
	-rm -rf *_cache *_files