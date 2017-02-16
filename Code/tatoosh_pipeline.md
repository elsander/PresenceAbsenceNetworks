0. Set up data for structure and parameter learning with leave-one-out
crossvalidation
**RCode/TatooshAllDataCutoff.R**

1. structure learning with banjo
**submit jobs using
../Data/tatoosh-control-exp-cv/dyamic.tatoosh-control-exp-cv.settings.txt**

2. build adjlists from xml files, and convert to consensus networks
**PythonCode/parse_banjo_xml.py**
**PythonCode/build_consensus.py**

3. fit parameters and null models, and predict out-of-sample data
**RCode/TatooshCVParameterAndPrediction.R**

4. Fit Lasso and Pearson models
**RCode/FitLasso.R**
**RCode/FitPearson.R**

5. Calculate precision/recall and p-values
**RCode/PrecisionRecall.R**
