for dim in "$@"
do
    cd ~/nPMF/nohup/ws_lpmf/$dim
    nohup Rscript ~/nPMF/main/ws_lpmf_mm.R $dim
done