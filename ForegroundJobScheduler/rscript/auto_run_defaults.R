echo Which model to run?
read varname
Rscript run_sim.R --action file --sim offline --sample 100 --schedule dynamic --file default --result True --adjust TRUE --model $varname
Rscript run_sim.R --action file --sim offline --sample 100 --schedule dynamic --file default --result True --adjust FALSE --model $varname
Rscript run_sim.R --action file --sim offline --sample 100 --schedule disjoint --file default --result True --adjust TRUE --model $varname
Rscript run_sim.R --action file --sim online --sample 100 --schedule dynamic --file default --result True --adjust TRUE --model $varname
Rscript run_sim.R --action file --sim online --sample 100 --schedule dynamic --file default --result True --adjust FALSE --model $varname
Rscript run_sim.R --action file --sim online --sample 100 --schedule disjoint --file default --result True --adjust TRUE --model $varname
