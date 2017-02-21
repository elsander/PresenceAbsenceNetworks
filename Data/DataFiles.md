## Comte-data
Contains the following files:

**Comte-adjlist.csv**
The empirical food web for the France river dataset, based on
literature review of gut content analyses. Contains columns "Parent"
(source of the interaction), "Child" (recipient of the interaction),
and "Sign" (the sign of the effect of the parent on the
child). Species names in Parent and Child columns are given as full
Latin names, with genus and species separated by an underscore.

**Comte-adjlist-abbrev.csv**
Another file of the empirical food web for the France river dataset,
but with species names given as three-letter species abbreviations.

**data_Species_dynamics.txt**
Data file from Comte *et al.* 2015 from Dryad:
datadryad.org/resource/doi:10.5061/dryad.55758. Reproduced here so
that others can replicate how the data were processed to create the
presence-absence dataset used in this analysis. For details on this
dataset, see the information in the Dryad URL given above. If you use
these data, please cite Comte *et al.* 2015.

**Comte-Presence-Absence.txt**
Presence-absence data for the France river dataset. Creating by
processing **data_Species_dynamics.txt** with
**../../Code/RCode/CleanComteData.R**.

**SpeciesCodes-final.csv**
Species names for the France river dataset. Contains columns
"Latin_name" (species Latin name) and "SpecCode" (species
abbreviation).

**Comte-Species-final.txt**
Abbreviated species names, separated by newlines.

**ComteSpNames-final.txt**
Full Latin species names, separated by newlines.

## Comte-training-final
Contains **Comte-all-training-final.txt**, the
complete training data for the France river dataset. All other files
in this folder are data for individual sites in the training set, used
for dynamic Bayesian network structure learning in Banjo.

## Comte-CV-final
Same structure as **Comte-training-final**, but used as a test set for
the France river dataset.


