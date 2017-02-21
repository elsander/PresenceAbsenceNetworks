0. Set up data for structure and parameter learning with training, CV,
and test sets
**RCode/SubsampleComte.R**

1. structure learning with banjo
**submit jobs using Data/Comte-training/dynamic.Comte.settings.txt**

2. build adjlists from xml files, and convert to consensus networks
**PythonCode/parse_banjo_xml.py**
**PythonCode/build_consensus.py**

3. fit parameters and null models
**RCode/ParameterInference.R**

4. predict out-of-sample data
**RCode/PredictionAccuracy.R**

5. Fit Lasso and Pearson models
**RCode/FitLasso.R**
**RCode/FitPearson.R**

6. Calculate precision and recall with p-values
**RCode/PrecisionRecall.R**


## Other files
**RCode/CleanComteData.R**
Converts data from Comte *et al.* 2015 from transition data (each row
represents a presence-absence transition between two time periods) to
presence-absence data (each row represents presence/absence at one
time period).
