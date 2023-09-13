# RISK Summary Table

The [RISK
cohort](https://www.crohnscolitisfoundation.org/research/current-research-initiatives/pediatric-risk-stratification)
is a closed observational prospective cohort study which enrolled
pediatric (&lt;16 years old) patients within 30 days of a suspected
Crohn’s Disease diagnosis. The final cohort enrolled 1,800 patients
across 28 clinics in the U.S. and Canada, with a particular focus on the
913 children with a Crohn’s Disease diagnosis who were complication free
in the first 90 days post diagnosis. Follow-ups including clinical and
demographic data were conducted every 6 months for a total of an 8-year
follow-up period. Blood, stool and tissue samples were collected at a
regularly scheduled procedure at baseline. Blood samples were collected
every 12 months for a total of 3 years. Molecular data including iChip
genotyping, 16S rDNA sequencing, RNASeq (10M reads, 30M reads, from FFPE
slides) transcriptomics, methylation epigenetics, whole shotgun
sequencing metagenomics (viral, bacterial and fungal), proteomics, and
serology have been generated from available biosamples.

All data used in any RISK summary functions should be loaded using the
`ibdplexus` R package `load_data` function. `risk_summary` is a function
in the ibdplexus package which exports an excel file with a RISK Summary
table. The RISK Summary table has one row for each visit per patient.
All data used to create the summary table comes from provider reported
case report forms. There are several functions that are used in creating
the final RISK summary table which are described below.

## RISK Summary Table Overview

`risk_summary` creates a table with the following columns:

<table>
<colgroup>
<col style="width: 9%" />
<col style="width: 90%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Column</th>
<th style="text-align: left;">Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">DEIDENTIFIED_MASTER_PATIENT_ID</td>
<td style="text-align: left;">Unique master patient ID</td>
</tr>
<tr class="even">
<td style="text-align: left;">DEIDENTIFIED_PATIENT_ID</td>
<td style="text-align: left;">Unique patient ID</td>
</tr>
<tr class="odd">
<td style="text-align: left;">DATA_SOURCE</td>
<td style="text-align: left;">Filtered for RISK</td>
</tr>
<tr class="even">
<td style="text-align: left;">VISIT_ENCOUNTER_ID</td>
<td style="text-align: left;">Unique visit encounter ID</td>
</tr>
<tr class="odd">
<td style="text-align: left;">TYPE_OF_ENCOUNTER</td>
<td style="text-align: left;">Visit type description, either enrollment
visit or follow-up visit based on 6 month cadence</td>
</tr>
<tr class="even">
<td style="text-align: left;">VISIT_MONTH</td>
<td style="text-align: left;">Numeric column with follow-up visit month,
enrollment visit is 0</td>
</tr>
<tr class="odd">
<td style="text-align: left;">VISIT_ENCOUNTER_START_DATE</td>
<td style="text-align: left;">Date associated with visit</td>
</tr>
<tr class="even">
<td style="text-align: left;">AGE_AT_ENCOUNTER</td>
<td style="text-align: left;">Age of patient at encounter</td>
</tr>
<tr class="odd">
<td style="text-align: left;">GENDER</td>
<td style="text-align: left;">Gender of patient, only possible values
are female or male</td>
</tr>
<tr class="even">
<td style="text-align: left;">BIRTH_YEAR</td>
<td style="text-align: left;">Year patient was born</td>
</tr>
<tr class="odd">
<td style="text-align: left;">DIAGNOSIS_DATE</td>
<td style="text-align: left;">Date which patient first received a
diagnosis</td>
</tr>
<tr class="even">
<td style="text-align: left;">DIAGNOSIS</td>
<td style="text-align: left;">Diagnosis of patient at that visit. Either
Crohn’s Disease, IBD Unclassified, Not IBD, Ulcerative Colitis or NA.
Diagnosis can change with each visit</td>
</tr>
<tr class="odd">
<td style="text-align: left;">FINAL_DIAGNOSIS</td>
<td style="text-align: left;">Patient’s diagnosis at final encounter.
Linked to patient, not visit</td>
</tr>
<tr class="even">
<td
style="text-align: left;">DISEASE_BEHAVIOR_STRICTURING/FIBROSTENOTIC</td>
<td style="text-align: left;">Does the patient exhibit
stricturing/fibrostenotic disease behavior at encounter</td>
</tr>
<tr class="odd">
<td
style="text-align: left;">DISEASE_BEHAVIOR_INTERNALLY_PENTRATING</td>
<td style="text-align: left;">Does the patient exhibit internally
penetrating disease behavior at encounter</td>
</tr>
<tr class="even">
<td style="text-align: left;">FIRST_BEHAVIOR</td>
<td style="text-align: left;">The first disease behavior recorded for
Crohn’s Disease patients based on the <a
href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1856208/">Montreal
Classification</a>.<span class="math inline"><sup>1</sup></span></td>
</tr>
<tr class="odd">
<td style="text-align: left;">FINAL_BEHAVIOR</td>
<td style="text-align: left;">The last disease behavior recorded for
Crohn’s Disease patients based on the <a
href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1856208/">Montreal
Classification</a>.<span class="math inline"><sup>1</sup></span></td>
</tr>
<tr class="even">
<td style="text-align: left;">DISEASE_JOURNEY</td>
<td style="text-align: left;">The Montreal Classification disease
journey for Crohn’s Disease patients, FIRST_BEHAVIOR -&gt; MIDDLE
BEHAVIOR (when available) -&gt; FINAL_BEHAVIOR</td>
</tr>
<tr class="odd">
<td style="text-align: left;">IBD_FAMILY_HISTORY</td>
<td style="text-align: left;">Any first-degree member of the subject’s
family have a known history of IBD</td>
</tr>
<tr class="even">
<td style="text-align: left;">EXTRAINTESTINAL MANIFESTATIONS COLUMNS -
20:39</td>
<td style="text-align: left;">Does the patient present with a new
extraintestinal manifestation since last review</td>
</tr>
<tr class="odd">
<td style="text-align: left;">LAB RESULT COLUMNS - 40:62</td>
<td style="text-align: left;">Numeric results for various lab tests.
Possible lab results include: hemoglobin, lymphocytes, creatinine,
alanine aminotransferase (ALT), albumin, alkaline phosphatase (ALP),
aspartate aminotransferase (AST), C reactive protein, eosinophil,
erythocyte sedimentation rate (ESR), gamma-glutamyl transferase (GGT),
hematocrit, neutrophil, platelet count, urea, white blood cell count,
GM-CSF, IgA ASCA, IgG ASCA, I2, OMPC, CBIR FLA, ANCA</td>
</tr>
<tr class="even">
<td style="text-align: left;">MEDICATION COLUMNS - 63:138</td>
<td style="text-align: left;">Information about the medications a
patient was on at a visit encounter. ANTIBIOTICS, CORTICOSTEROIDS,
IMMUNOMODULATORS, BIOLOGIC AGENTS, 5-ASA ORAL are filled out as Yes/No
for enrollment visits only. Columns beginning with ONGOING_ are only
filled out at follow-up encounters. Date is used if patient has received
medication, otherwise the column is left blank or filled with
<code>No</code>. <code>Yes</code> is used when date is not
available.</td>
</tr>
<tr class="odd">
<td style="text-align: left;">PCDAI COLUMNS - 139:165</td>
<td style="text-align: left;">Columns with the values required for PCDAI
and/or wPCDAI calculations. Any numerical categorization of values is
done to calculate wPCDAI.</td>
</tr>
<tr class="even">
<td style="text-align: left;">WPCDAI</td>
<td style="text-align: left;">Weighted pediatric Crohn’s disease
activity index, created using the wpcdai function in ibdplexus.
Remission is &lt; 12.5. Mild is 12.5 to 40. Moderate is &gt;40 to 57.5.
Severe is &gt; 57.5.</td>
</tr>
<tr class="odd">
<td style="text-align: left;">DISEASE LOCATION COLUMNS - 167:178</td>
<td style="text-align: left;">Disease involvement at all listed
locations. Values are: Normal, Macroscopic Disease, Microscopic Disease
only, Not Assessed, Unknown</td>
</tr>
<tr class="even">
<td style="text-align: left;">PERIANAL DISEASE COLUMNS - 179:186</td>
<td style="text-align: left;">Does the patient exhibit any aspects of
perianal disease at this encounter. Aspects of perianal disease
available: large skin tags, ulcers, fissure(s), isolated abscess,
multiple abscesses, perianal fistula/e, recto-vaginal fistula/e,
ano-vaginal fistula/e</td>
</tr>
<tr class="odd">
<td
style="text-align: left;">ENDOSCOPIC_ASSESSMENT_DATA_COLLECTION_METHOD</td>
<td style="text-align: left;">Method of endoscopic data collection for
results presented in following endoscopic assessment columns. Possible
values are: Not Obtainable, Retrospective Chart Review, Proceduralist
(Retrospective), Proceduralist (Contemporaneous)</td>
</tr>
<tr class="even">
<td style="text-align: left;">ENDOSCOPIC ASSESSMENT COLUMNS -
188:213</td>
<td style="text-align: left;">Results for various endoscopic assessment
values. Possible areas assessed include: rectum, ileum, ascending colon,
descending colon, sigmoid, transverse colon, and any non-ulcerated or
ulcerated stenoisis anywhere. Yes/No for columns assessing deep or
superficial ulceration. Numeric values for columns assessing amount of
surface involved and amount of surface ulcerated.</td>
</tr>
<tr class="odd">
<td style="text-align: left;">OMICS COLUMNS - 214:223</td>
<td style="text-align: left;">Lists of files for omics data if generated
from biosamples collected at that visit. Possible omics include:
Immunochip high-density array, ITS2 sequencing, RNASeq (paired-end
150-bp reads), RNASeq (single-end 50-bp reads), viral metagenomics
sequencing (virome), whole shotgun sequencing (WGS), 16S, DNA
methylation, genotyping (global screening array), proteomic biomarker
panels (Olink)</td>
</tr>
<tr class="even">
<td style="text-align: left;">BIOSAMPLE COLUMNS - 224:230</td>
<td style="text-align: left;">Status (Stored/In Lab) for the biosamples
a patient might have available from that visit. Possible biosamples
available include: plasma, mucosal RNA, blood DNA, mucosal DNA, and
stool.</td>
</tr>
<tr class="odd">
<td style="text-align: left;">OMICS_ANYTIME COLUMNS - 231:240</td>
<td style="text-align: left;">1 if the patient has omics data at any
visit, 0 if no omics data is available for that patient</td>
</tr>
<tr class="even">
<td style="text-align: left;">BIOSAMPLE_ANYTIME COLUMNS 241:247</td>
<td style="text-align: left;">1 if the patient has a biosample at any
visit, 0 if biosamples not available for that patient</td>
</tr>
</tbody>
</table>

<!-- ### MEDICATIONS_AT_VISIT -->
<!-- The column `MEDICATIONS_AT_VISIT` is a list of medications the patient was on at that visit. The medications at visit column is based on medications listed associated with the VISIT_ENCOUNTER_ID in the prescriptions table. -->
<!-- If a medication had been administered but there was no start date recorded, assume the start date is the visit encounter date. -->

## `wpcdai`

The function `wpcdai` creates a dataframe with patient and visit
encounter ID’s, the wPCDAI calculated for a patient, and all PCDAI
columns.

wPCDAI is the weighted version of the Pediatric Crohn’s Disease Activity
Index. It is more feasible than the full PCDAI for many patients who may
be missing some assessments or characteristics required for the
calculation. wPCDAI has performed better than other abbreviated PCDAI
calculations.<sup>2</sup> In the RISK summary table wPCDAI is calculated
for any patient with the required values, even if they do not have a
Crohn’s Disease diagnosis.

The required values for the wPCDAI calculation are found in the
following columns. Each variable necessary for the calculation is
assigned a numerical category to calculate the wPCDAI.<sup>3</sup> *Note
the numerical categorization would be different to calculate the full
PCDAI .*

<table style="width:99%;">
<colgroup>
<col style="width: 26%" />
<col style="width: 30%" />
<col style="width: 42%" />
</colgroup>
<thead>
<tr class="header">
<th>Column Used in Calculation</th>
<th>Original Columns</th>
<th>Logic</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>PCDAI - MOST SEVERE ABDOMINAL PAIN PAST 7 DAYS</code></td>
<td><code>PCDAI - MOST SEVERE ABDOMINAL PAIN PAST 7 DAYS</code></td>
<td><p>None: 0</p>
<p>Mild (brief, does not interfere with activies): 10</p>
<p>Moderate/Severe (daily, longer lasting, affects activities,
nocturnal): 20</p></td>
</tr>
<tr class="even">
<td><code>PCDAI-PT FUNCTING,GENERAL WELLBEING PAST 7 DAY</code></td>
<td><code>PCDAI-PT FUNCTING,GENERAL WELLBEING PAST 7 DAY</code></td>
<td><p>No limitations of activities, well: 0</p>
<p>Occassional difficulty in maintaining age appropriate activities,
below par: 10</p>
<p>Frequent limitation of activity, very poor: 20</p></td>
</tr>
<tr class="odd">
<td><code>PCDAI - STOOLS PER DAY PAST 7 DAYS</code></td>
<td><code>PCDAI - STOOLS PER DAY PAST 7 DAYS</code></td>
<td><p>0-1 liquid stools, no blood: 0</p>
<p>2 semi-formed stools with little blood, or 2-5 liquid stools +/-
blood: 7.5</p>
<p>Gross bleedings or 6 or more liquid or noctural diarrhea: 15</p></td>
</tr>
<tr class="even">
<td><code>PCDAI - ESR</code></td>
<td><code>PCDAI - ESR (MM/HR) WITHIN LAST 2 WEEKS</code></td>
<td><p>ESR (mm/hr) &lt; 20: 0 |</p>
<p>ESR (mm/hr) from 20 - 50: 7.5</p>
<p>ESR (mm/hr) &gt; 50: 15 |</p></td>
</tr>
<tr class="odd">
<td><code>PCDAI - ALBUMIN</code></td>
<td><code>PCDAI - ALBUMIN (G/DL WITHIN LAST 2 WEEKS</code></td>
<td><p>Albumin (g/dL) <span class="math inline">≥</span> 3.5: 0 |</p>
<p>Albumin (g/dL) from 3.1 - 3.4: 10</p>
<p>Albumin (g/dL) <span class="math inline">≤</span> 3: 20 |</p></td>
</tr>
<tr class="even">
<td><code>PCDAI - WEIGHT</code></td>
<td><code>PCDAI - WEIGHT</code></td>
<td><p>Weight gain or voluntary weight stable/loss: 0</p>
<p>Involuntary weight stable or weight loss 1-9%: 5</p>
<p>Weight loss <span class="math inline">≥</span> 10%: 10 |</p></td>
</tr>
<tr class="odd">
<td><code>PCDAI - PERIRECTAL DISEASE</code></td>
<td><table>
<thead>
<tr class="header">
<th>Perirectal Disease Columns</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>PCDAI -PERIRECTAL DISEASE: ASYMPTOMATIC TAGS</code></td>
</tr>
<tr class="even">
<td><code>PCDAI -PERIRECTAL DISEASE: INFLAMMED TAGS</code></td>
</tr>
<tr class="odd">
<td><code>PCDAI -PERIRECTAL DISEASE: FISSURE</code></td>
</tr>
<tr class="even">
<td><code>PCDAI -PERIRECTAL DISEASE: INDOLENT FISTULA</code></td>
</tr>
<tr class="odd">
<td><code>PCDAI -PERIRECTAL DISEASE:ACTIVE FISTULA/ABSCESS</code></td>
</tr>
<tr class="even">
<td><code>PCDAI -PERIRECTAL DISEASE:DRAINAGE OR TENDERNESS</code></td>
</tr>
</tbody>
</table></td>
<td><p>None/asymptomatic tags: 0</p>
<p>1-2 indolent fistula/scant drainage/no tenderness: 7.5</p>
<p>Active fistula, drainage, tenderness/abscess: 15</p></td>
</tr>
<tr class="even">
<td><code>PCDAI - EIM</code></td>
<td><table>
<thead>
<tr class="header">
<th>EIM Columns</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>PCDAI-EIM: FEVER &gt; 38.5C FOR 3 DAYS OVER PAST WEEK</code></td>
</tr>
<tr class="even">
<td><code>PCDAI - EIM: ORAL ULCERS</code></td>
</tr>
<tr class="odd">
<td><code>PCDAI - EIM: UVEITIS</code></td>
</tr>
<tr class="even">
<td><code>PCDAI - EIM: DEFINITE ARTHRITIS</code></td>
</tr>
<tr class="odd">
<td><code>PCDAI - EIM: E NODOSUM</code></td>
</tr>
<tr class="even">
<td><code>PCDAI - EIM: P GANGRENOSUM</code></td>
</tr>
</tbody>
</table></td>
<td><p>No EIM: 0</p>
<p>One or more EIM: 10</p></td>
</tr>
</tbody>
</table>

wPCDAI is calculated by summing the numeric values from the required
columns. The scoring for disease activity for the wPCDAI
are<sup>2</sup>:

<table>
<thead>
<tr class="header">
<th>wPCDAI Score</th>
<th>Disease Activity</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><span class="math inline"> &lt; 12.5</span></td>
<td>Remission</td>
</tr>
<tr class="even">
<td><span class="math inline">12.5 − 40</span></td>
<td>Mild</td>
</tr>
<tr class="odd">
<td><span class="math inline"> &gt; 40</span></td>
<td>Moderate</td>
</tr>
<tr class="even">
<td><span class="math inline"> &gt; 57.5</span></td>
<td>Severe</td>
</tr>
</tbody>
</table>

A decrease of 17.5 points is taken evidence of improvement.<sup>2</sup>

## References

1.  Satsangi J, Silverberg MS, Vermeire S, Colombel JF. The Montreal
    classification of inflammatory bowel disease: controversies,
    consensus, and implications. Gut. 2006 Jun;55(6):749-53. doi:
    10.1136/gut.2005.082909. PMID: 16698746; PMCID: PMC1856208.

2.  Turner D, Griffiths AM, Walters TD, Seah T, Markowitz J, Pfefferkorn
    M, Keljo D, Waxman J, Otley A, LeLeiko NS, Mack D, Hyams J,
    Levine A. Mathematical weighting of the pediatric Crohn’s disease
    activity index (PCDAI) and comparison with its other short versions.
    Inflamm Bowel Dis. 2012 Jan;18(1):55-62. doi: 10.1002/ibd.21649.
    Epub 2011 Feb 23. PMID: 21351206.

3.  Turner D, Levine A, Walters TD, Focht G, Otley A, López VN, Koletzko
    S, Baldassano R, Mack D, Hyams J, Griffiths AM. Which PCDAI Version
    Best Reflects Intestinal Inflammation in Pediatric Crohn Disease? J
    Pediatr Gastroenterol Nutr. 2017 Feb;64(2):254-260. doi:
    10.1097/MPG.0000000000001227. PMID: 27050050.
