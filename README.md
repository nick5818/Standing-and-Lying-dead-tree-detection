This study applies novel advanced statistical methods to airborne laser scanning data to detect and classify standing dead trees (SDTs) and lying dead trees (LDTs) 
The methods include the Linear Discriminant Analysis (LDA) for SDTs and template matching techniques for LDTs to improve the efficiency of deadwood detection.
The scripts are part of my thesis. The analysis apply only for .las files and aims for the classification of the standing dead trees and the detection of lying dead trees

1st script refers to general processing of .las files (filtering noise and height) as well as the annotation accuracy assessment.

2nd script contains the functions used for the pre-processing and the segmentation of trees.

3rd script refers to the segmentation of trees, some graphs to evaluate the results and the LDA analysis.

4th script is based on template matching analysis for the detection of the lying trees

#### Instructions ####
1. Wherever there is a path "G:/Users/ioly0001/Thesis/Data/", you should replace it with your own path. It applies for both reading and writing files in the code.
2. The files "merged_sdts" and "merged_ldts" refer to the train data of standing and lying dead trees accordingly. "merged" in this case, indicates that we have a variable with all the train data from multiple .las files.
3. In the ".las pre-processing", the last section "Annotation Accuracy Assessment (STDs)", can be used only if you have field data (GNSS measurements) and you would like to test the annotation accuracy (the train data that you acqired from your .las files) for both SDTs and LDTs. If you do not possess field data then you can skip this part.
4. "merged_mets" in the Segmentation section, also refers to the individual tree metrics from all plots, merged in one file.
5. Please ignore the print statements in the template matching section. It is just an indicator that the loops are progressing.
6. The variables used in histograms as well as the LDA are random. You can use your own variables to test the results.
7. In the segmentation function we have an extra step where we merge an additional csv file (csv_extra, id_extra). This refers to the csv file with some extra annotated SDTs that were not detected from the "locate_trees" function. If you do not have extra annotations then you can skip this part.
8. At the tree tops function we have an output csv with columns: X, Y, Z coordinates and Status which is assigned as "Healthy". The status changed to SDT manually in QGIS based on visual observation of the .las files. Accordingly in the segmentation fucntion, we add the columns of all metrics calculated.
