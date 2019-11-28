cp ../results/online\ results/summary\ dynamic.csv ../results/online\ results/temp_dynamic.csv
cp ../results/online\ results/summary\ dynamic\ post\ adj.csv ../results/online\ results/temp_dynamic_post.csv
cp ../results/online\ results/summary\ disjoint.csv ../results/online\ results/temp_disjoint.csv
cp ../results/offline\ results/summary\ dynamic.csv ../results/online\ results/temp_dynamic.csv
cp ../results/offline\ results/summary\ dynamic\ post\ adj.csv ../results/online\ results/temp_dynamic_post.csv
cp ../results/offline\ results/summary\ disjoint.csv ../results/online\ results/temp_disjoint.csv
git stash
git pull
Rscript MergeResultForm.R --from ../results/online\ results/temp_dynamic.csv --to ../results/online\ results/summary\ dynamic.csv
Rscript MergeResultForm.R --from ../results/online\ results/temp_dynamic_post.csv --to ../results/online\ results/summary\ dynamic\ post\ adj.csv
Rscript MergeResultForm.R --from ../results/online\ results/temp_disjoint.csv --to ../results/online\ results/summary\ dynamic.csv
Rscript MergeResultForm.R --from ../results/online\ results/temp_dynamic.csv --to ../results/offline\ results/summary\ dynamic.csv
Rscript MergeResultForm.R --from ../results/online\ results/temp_dynamic_post.csv --to ../results/offline\ results/summary\ dynamic\ post\ adj.csv
Rscript MergeResultForm.R --from ../results/online\ results/temp_disjoint.csv --to ../results/offline\ results/summary\ disjoint.csv