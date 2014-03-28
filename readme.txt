Scripts for creating phenotype model features based on a silver standard training/testing data
==============================================================================================

1. R version:3.0.2 (2013-09-25)
2. Packages required. RMySQL, VennDiagram, Rlibstree, glmnet
3. R scripts for phenotype modelling (in order of execution):
   a) phen.R: unique ids for patients whose notes containin query terms.
   b) ctrls.R: unique ids for patients whose notes contain none of the query terms
   c) cutoff.R: Time zero cut off for patient records and framed terms for each note after the 
      cut off for each patient
   d) ctrlterms.R: matched control ids from the pool of controls. Matching is done on the number 
      of observations after the cut off instant. For each patient framed terms are generated.
   e) conceptFeats.R: convert terms to concepts. Create a vocabulary of concepts and a feature vector for
      each case and control id consisting of TFIDF for each concept in the pruned vocabulary.
   f) meds.R: creates a cleaned set of medications (removing formulation and dosage data). Prunes the 
      vocabulary of meds after discarding low frequency meds. Creates a feature vecor for each id based on 
      presence/absence in the note.
   g) labs.R: Creates a set (union) of all labs. prunes the list to discard low frequency test and for 
      each creates a weighted average of the in-range status value.
   h) codes.R: creates a set of all codes after truncating the digits after the decimal point. Creates 
      a   presence/absence feature vector for each code, for each id.
   i) designmatrix.R Combines the codes/meds/labs/concepts feature sets into one big design matrix.
4. The scripts make use of databases in the STRIDE data ware house. It is required to have access credentials
   configured ni the .my.cnf before running the scripts.


