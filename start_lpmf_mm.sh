for dim in "$@"
do
    cd ~/nPMF/nohup/lpmf/$dim
    nohup Rscript ~/nPMF/main/lpmf_mm.R $dim
done