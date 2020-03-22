ThanoEmpirical
==============

This repo includes R code and tex files for a study in progress, provisionally titled "Time-to-death patterns in markers of age and dependency" with [Pil H. Chung](http://www.paulchung.org/), [Jeroen Spijker](http://ced.uab.es/en/directori/jeroen-spijker/) and [John MacInnes](http://www.sps.ed.ac.uk/staff/sociology/macinnes_john). 

The manuscript is citable as:

Riffe T., Chung P.H., Spijker J., and MacInnes J. (2016) 'Time-to-death patterns in markers of age and dependency' Vienna Yearbook of Population Research. V14, pp 229-254.

Here's a bibtex entry:
```
@Article{riffe2015ttd,
  Title                    = {Time-to-death patterns in markers of age and dependency},
  Year                     = {2016},
  Author                   = {{Riffe, T.} and Chung, P. H. and Spijker, J. and MacInnes, J.},
  Journal                  = {Vienna Yearbook of Population Research},
  Volume                   = {14},
  Pages                    = {229--254}
}
```
<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" property="dct:title">Time-to-death patterns in markers of age and dependency</span> by <a xmlns:cc="http://creativecommons.org/ns#" href="https://sites.google.com/site/timriffepersonal/" property="cc:attributionName" rel="cc:attributionURL">Timothy L. M. Riffe</a> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.

Data
========
Instructions to get the data needed to reproduce this study:

In order to obtain the RAND HRS data file, you must register for an HRS account at:

https://ssl.isr.umich.edu/hrs/reg_pub2.php 

Once in possession of an HRS username and password, the RAND HRS data file may be downloaded at:

https://ssl.isr.umich.edu/hrs/start.php

Under the 'RAND Contributed Files' section, click the link for 'RAND HRS Data File (v.M)'; this will take you to the download section. Here, click the link for 'randmstata.zip' - this file contains all the data and documentation that is needed. Unzip the downloaded file into your research directory.

Reproducing
===========
All code needed to extract and process these data are contained in this repository, and these are found in the scripts are found in the R/ folder. The scripts expect to see some other folders too, so set up a structure something like this.

 
     MainFolder/
       R/
       Data/             <-put the extracted RAND file, rndhrs_m.dta, in here
       Figures/  
         HeatTables/
           1905/
           1910/
           1915/
           1920/
           1925/
         PanelCoh5/     
       Appendix/
         Results/

In the header of each R script you'll see how TR's working directory is set automatically. On your system, you can just replace all that with your own setwd() command. Having done this, execute the following scripts in this order:

1. HRS_Rand_extract.R
2. PreProcessing.R
3. CreateMatrices.R
4. loessSmoothing.R
5. Correlations.R
6. PaperFigures.R
7. AppendixHeatTables.R
8. (optional) SurfaceCompare.R   <-some diagnostic surfaces, not so pretty, but interpretable.

Several objects will be created in the Data, Figures, and Appendix folders. Notably, the csv produced in the Appendix folder can be analyzed and/or visualized further, and the large list of loess-smoothed surfaces (with visual diagnostics produced in Figures/PanelCoh5/) is also ripe for further analysis (better than correlations?), or to be compared with alternative smoothing methods (See note below). One could also check whether the M pattern appears to respect some rule of proportionality by length of life (i.e. morbidity kicking in at 9/10 through life). Ergo, there are many empirical things one could do to improve or complement the work presented here. Have at it!

R scripts that start with zzz are deprecated from earlier stages of this project. These are either exploratory or discarded ways of doing things. The repository also contains our award-winning poster from the 2015 PAA, as well as a presentation given at the VID in December, 2014.

Note
========
We (with help from Maarten J. Bijlsma) have since compared the loess smoothing method with a better-suited GLM with binomial link (where appropriate) over age, time-to-death, and cohort (as with the loess used in the paper). This produces results that are nearly the same. We then modified this procedure to re-run results on sampled person-ids in the data, producing ca 1000 smooth fits, and then took median and means of these as the best-we-can-do-right-now fits. Again, the rough loess fits are super close to these. If we re-run the correlation analyses you end up with distributions that are essentially the same (slightly jittered as one would expect). To see this analyses, see the script R/ns_glm_smoothing.R .

