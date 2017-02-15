0. Set up data for structure and parameter learning with training, CV,
and test sets
**RCode/SubsampleComte.R**

1. structure learning with banjo
**submit jobs using ../Data/Comte-training/dynamic.Comte.settings.txt**

2. build convergence dictionary, clear out bad adjlist files
**PythonCode/parse_banjo_xml.py**

3. fit parameters and null models
**RCode/ParameterInference.R**

4. predict out-of-sample data
**RCode/PredictionAccuracy.R**

5. Calculate precision and recall with p-values
**RCode/PrecisionRecall.R**
