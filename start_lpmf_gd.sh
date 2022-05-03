for dim in "$@"
do
    cd ~/nPMF/nohup/lpmf/$dim
    nohup Rscript ~/nPMF/main/lpmf_gd.R $dim
done