ThanoEmpirical
==============

This repo includes R code and tex files for a study in progress, provisionally titled "Time-to-death patterns in markers of age and dependency" with [Pil H. Chung](http://www.paulchung.org/), [Jeroen Spijker](http://www.ced.uab.es/index.php?module=pagesetter&func=viewpub&tid=12&pid=21) and [John MacInnes](http://www.sps.ed.ac.uk/staff/sociology/macinnes_john). It is likely too early to cite, and we are still just messing around. Still, it is freely available, and can be used with proper attribution:

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

 *MainFolder/
  *    R/
  *    Data/             
  *    Figures/
   *        HeatTables/
    *            1905/
    *            1910/
    *            1915/
    *           1920/
    *            1925/
  *    Appendix/
   *        Results

In the header of each R script you'll see how TR's working directory is set automatically. On your system, you can just replace all that with your own setwd() command. Having done this, execute the scripts in this order:

1. HRS_Rand_extract.R
2. PreProcessing.R
3. CreateMatrices.R
4. loessSmoothing.R
5. Correlations.R
6. PaperFigures.R
7. AppendixHeatTables.R
8. (optional) SurfaceCompare.R   <-some diagnostic surfaces, not so pretty, but interpretable.



Note
========
The loess smoothing procedure used in this paper is rough around the edges, literally, as it may have edge-effects. The loess smoothing also ignores within-individual autocorrelation, and it also appears to have some undesirable edge-effects that may even tilt results somewhat. Don't fret, though, the major finds are robust, since we used very rough methods. We were not able to identify the perfect method prior to finalizing this paper, so this statistical question still remains open. Since we'll be using such results in the future, and eventually measurement will matter a lot, it'd be nice to receive a tip. Like, maybe a glm with fixed effects? Would that smooth as a side-effect? Because that's all we want now. Any statisticians out there?

