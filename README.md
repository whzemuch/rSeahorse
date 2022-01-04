# rSeahorse


rSeahorse is an R package implementing a complete workflow for analyzing data produced by Seahourse's mitochondria stress test, a widely recognized, well-accepted standard assay for assessing mitochondrial function. Currently it only support seahorse XFe24 Analyzer. 
 <p><img style="float:right;margin-right:10px" src="images/rSeahorse_hexsticker.png" width=150/></p>

rSeahorse can generate a figure showing OCR consumption rate like figure 5f in the paper as below:
     Haythorne, E. et al. Diabetes causes marked inhibition of mitochondrial metabolism in pancreatic β-cells. Nature Communications 10, 2474 (2019).

rSerhorce also can calculate and visualize those items listed as below:

1. __Basal Respiration__
2. __Maximal Respiration__
3. __Proton Leak__
4. __ATP Production__
5. __Non-Miochondrial Respiration__
6. __Spare Respiratory Capacity__
<br>


##  Install rSeahorse

```r
library(devtools)
devtools::install_github("whzemuch/rSeahorse/")
```
