# RNFuzzyApp

---

RNFuzzyApp is an application to analyse aligned RNA-seq data and do fuzzy clustering.</br>
</br>


- First proceed to a quality control, an alignment and a count (FeatureCounts for example). Input table is geneIDS as rows and conditions/covariates as columns. </br>

<b> Installation </b>
---

- Installation requiere running <b> runApp('path/rna-seq-analysis-app-master-App/App') </b> in R and all requiered packages will automatically install. </br>
- Upload a count csv or tsv table where the first column is composed of genes' names (can be Symbols, EnsembleIds, FlybaseIDs, ...). </br>
- Example tables are available in test_data/.</br>
- Mfuzz soft clustering asks for a  table of same above composition but mean only if you have replicates.



<b>PART Ia : Upload data and visualization</b>
---

- Upload your data as described above, choose your groups </br>
- Visualize the data across :</br>
    - result table</br>
    - Count distribution (bar plot)</br>
    - Hierarchical clustering (heatmap)</br>
    - PCA ( 2D and 3D plots)</br>


<b>PART Ib : Upload data and visualization</b>
---

- Perform soft clustering on time series data using Mfuzz. </br>
With help to choose the number of clusters.
- clusters table automatically downloaded.


<b>PART II : Analysis</b>
---
3 analysis methods are proposed, TCC, DESeq2 or edgeR </br>

<b>- Normalization</b> </br>

The normalization is made by the TCC package.The package allows to compare tag count data with robust normalization strategies.</br>
<i>https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-14-219</i></br>
Or by DESeq2 or by TMM, RLE, upperquartile (with a edgeR analysis) </br>
</br>
- Choose your method and parameters</br>
- Visualize the results</br>
- Download contents</br>

<b>- Analysis</b> </br>

For each result, downloading png images and results is available.</br>

- Heatmap and clustering </br>
- PCA ( 2D and 3D plots)</br>
- Filter part to choose two conditions to perform MA and Volcano plots
- MA plot</br>
- Volcano plot</br>

<b> PART III : Mfuzz soft clustering </b>
---

Given a csv table of mean counts per timepoints, Mfuzz generates fuzzy clustering regarding the chosen number of clusters.
Methods are present to help the user choose the numer of clusters, and the time series plots will be generated.
Also available : enrichment for a chosen cluster.


<b> PART IV : Enrichment & Conversion </b>
---

<b> - Enrichment : </b></br>

Giving a set of ids, this section provides you a GO Term enrichment, Kegg and Wiki Pathways with an associated graph. </br>

<b> - Orthology : </b></br>

Giving a set of  ids, input and target organism, this section provides an orthologs search using gprofiler2 package. </br>

<b> - Conversion : </b></br>

Giving a set of  ids of choice, this section provides a translation to other ids using gprofiler2 package.
