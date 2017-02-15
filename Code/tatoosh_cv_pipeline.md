0. Set up data for structure and parameter learning with leave-one-out
crossvalidation
**RCode/TatooshAllDataCutoff.R**

1. structure learning with banjo
**submit jobs using
../Data/tatoosh-control-exp-cv/dyamic.tatoosh-control-exp-cv.settings.txt**

2. build convergence dictionary, clear out bad adjlist files
**PythonCode/build_convergence_list.py**

3. build consensus adjlist file
**PythonCode/build_consensus_adjlist.py**

4. fit parameters and null models
**RCode/TatooshCVParameterAndPrediction.R**

5. Calculate precision/recall and p-values
**RCode/PrecisionRecall.R**
