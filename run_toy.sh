for func in quad gaussian half_step ripples
do
    Rscript R/toy_functions.R $func &
    BACK_PID=$!
done

wait $BACK_PID

echo "Now plotting"
for func in quad gaussian half_step ripples
do
    Rscript R/figure_3.R $func
    BACK_PID=$!
done
