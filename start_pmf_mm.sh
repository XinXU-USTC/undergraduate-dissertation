for dim in "$@"
do
    cd ~/nPMF/nohup/pmf/$dim
    nohup Rscript ~/nPMF/main/pmf_mm.R $dim
done