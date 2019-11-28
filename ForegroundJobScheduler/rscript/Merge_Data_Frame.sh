cp ../results/online\ results/summary\ dynamic.csv ../results/online\ results/temp_dynamic1.csv
cp ../results/online\ results/summary\ dynamic\ post\ adj.csv ../results/online\ results/temp_dynamic_post1.csv
cp ../results/online\ results/summary\ disjoint.csv ../results/online\ results/temp_disjoint1.csv
cp ../results/offline\ results/summary\ dynamic.csv ../results/offline\ results/temp_dynamic1.csv
cp ../results/offline\ results/summary\ dynamic\ post\ adj.csv ../results/offline\ results/temp_dynamic_post1.csv
cp ../results/offline\ results/summary\ disjoint.csv ../results/offline\ results/temp_disjoint1.csv
git stash
git pull
Rscript MergeResultForm.R --from ../results/online\ results/temp_dynamic1.csv --to ../results/online\ results/summary\ dynamic.csv
Rscript MergeResultForm.R --from ../results/online\ results/temp_dynamic_post1.csv --to ../results/online\ results/summary\ dynamic\ post\ adj.csv
Rscript MergeResultForm.R --from ../results/online\ results/temp_disjoint1.csv --to ../results/online\ results/summary\ disjoint.csv
Rscript MergeResultForm.R --from ../results/offline\ results/temp_dynamic1.csv --to ../results/offline\ results/summary\ dynamic.csv
Rscript MergeResultForm.R --from ../results/offline\ results/temp_dynamic_post1.csv --to ../results/offline\ results/summary\ dynamic\ post\ adj.csv
Rscript MergeResultForm.R --from ../results/offline\ results/temp_disjoint1.csv --to ../results/offline\ results/summary\ disjoint.csv
git status
git add ../results/online\ results/summary\ dynamic.csv
git add ../results/online\ results/summary\ dynamic\ post\ adj.csv
git add ../results/online\ results/summary\ dynamic.csv