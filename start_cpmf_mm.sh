for dim in "$@"
do
    cd ~/nPMF/nohup/cpmf/$dim
    nohup Rscript ~/nPMF/main/cpmf_mm.R $dim
done