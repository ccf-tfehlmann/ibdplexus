diagnosis\_emr\_extract User Guide
================

This file contains a function to extract relevant diagnosis codes from
diagnosis and/or patient problem emr data for a specific condition
category of interest.

Future Updates: 1. Additional code to add indicator to patient level
data for the presence of a diagnosis. 2. Relevant ICD 9 Codes to
supplement ICD 10 codes.

Requires packages: ibdplexus, DiagrammeR, tidyverse

| Condition Categories                     | ICD 10 Diagnosis Codes                                                                                                                                                                                         | ICD 9 Diagnosis Codes                                                                                                       |
| ---------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------- |
| SYSTEMIC FUNGAL INFECTION                | B39, B45, B38, B40, B46, B44, B37, B59                                                                                                                                                                         | 110-118                                                                                                                     |
| CANCER                                   | All C codes, d0, d1, d2, d3, d4                                                                                                                                                                                | 140-229                                                                                                                     |
| COLORECTAL CANCER                        | C18, C19, C20                                                                                                                                                                                                  | 153, 154.0, 154.1                                                                                                           |
| CERVIX CARCINOMA                         | D06                                                                                                                                                                                                            | 233.1                                                                                                                       |
| SKIN CARCINOMA                           | C44.01, C44.02, C44.11, C44.12, C44.21, C44.22, C44.31, C44.32, C44.41, C44.42, C44.51, C44.52, C44.61, C44.62, C44.71, C44.72, C44.81, C44.82, C44.91, C44.92                                                 | 232                                                                                                                         |
| STOMA                                    | L24.B0, L24.B1, L24.B3, Z93.3, Z93.2                                                                                                                                                                           | V44.2, V44.3                                                                                                                |
| DEMYELINATING DISORDER                   | G35, G36, G37                                                                                                                                                                                                  | 340, 341                                                                                                                    |
| CELIAC                                   | K90.0                                                                                                                                                                                                          | 579.0                                                                                                                       |
| PSC                                      | K83.01                                                                                                                                                                                                         | 576.1                                                                                                                       |
| GI BLEEDING                              | K92.1                                                                                                                                                                                                          | 569.3, 578.1, 599.70, 777.3, 792.1                                                                                          |
| GI ULCER                                 | K25, K27, K28, K26, K63.3, K62.6                                                                                                                                                                               | 531-534                                                                                                                     |
| PERIANAL ABSCESS OR FISTULA              | K50.913, K50.914, K50.813, K50.814, K50.013, K50.014, K50.113, K50.114, K51.013, K51.014, K51.213, K51.214, K51.313, K51.314, K51.413, K51.414, K51.513, K51.514, K51.813, K51.814, K51.913, K51.914, K60, K61 | 565.1, 566                                                                                                                  |
| WEIGHT LOSS                              | R63.4                                                                                                                                                                                                          | 783.1, 783.2                                                                                                                |
| B2 OR B3                                 | K50.912, K50.112, K50.012, K50.812                                                                                                                                                                             | 560.89, 560.9                                                                                                               |
| MALNOURISHMENT                           | E4                                                                                                                                                                                                             | 263.9, 269.9                                                                                                                |
| ANEMIA                                   | D50, D51, D52, D53                                                                                                                                                                                             | 280-281                                                                                                                     |
| DIARRHEA                                 | R19.7, K59.1, K58.0                                                                                                                                                                                            | 564.5, 787.91                                                                                                               |
| NAUSEA OR VOMITING                       | R11                                                                                                                                                                                                            | 787.0                                                                                                                       |
| HYPOALBUMINEMIA                          | E88.09                                                                                                                                                                                                         | 273.8                                                                                                                       |
| FEVER                                    | R50.9, R61                                                                                                                                                                                                     | 780.6                                                                                                                       |
| ABDOMINAL PAIN                           | R10                                                                                                                                                                                                            | 789.0                                                                                                                       |
| CDI                                      | A04.7                                                                                                                                                                                                          | 008.45                                                                                                                      |
| ARTHRITIS OR LOW BACK PAIN               | M13, M05, M06, M07, M08, M10, M11, M12, M14, M1A, ( need to add M54.5)                                                                                                                                         | 710-716, 724.2                                                                                                              |
| DACTYLITIS                               | L08.9                                                                                                                                                                                                          | 686.9                                                                                                                       |
| NON UC IBD DIAGNOSIS                     | K50, K52.3, K52.83, K55.9                                                                                                                                                                                      | 555, 558.9                                                                                                                  |
| TOXIC MEGACOLON                          | K59.31                                                                                                                                                                                                         | 564.7                                                                                                                       |
| FULMINANT COLITIS                        | K55.03                                                                                                                                                                                                         | 557.0                                                                                                                       |
| INTRAABDOMINAL ABSCESS                   | L02.211, K65.1                                                                                                                                                                                                 | 567.22, 682.2                                                                                                               |
| STRICTURE STENOSIS                       | K56.69                                                                                                                                                                                                         | 560.89                                                                                                                      |
| COLON ADENOMA                            | D12.2, D12.3, D12.4, D12.5, D12.6, K31.A2, K55.20                                                                                                                                                              | 211.3, 235.2                                                                                                                |
| INFECTION                                | L0, A49, A0 (need to refine and add B99)                                                                                                                                                                       | 001-009,130-136                                                                                                             |
| TUBERCULOSIS                             | A15, A17, A18, A19                                                                                                                                                                                             | 010-018                                                                                                                     |
| DIABETES                                 | E08, E09, E10, E11, E13                                                                                                                                                                                        | 250                                                                                                                         |
| HYPERTENSION                             | I10, I11, I12, I13, I15, I16                                                                                                                                                                                   | 401-405                                                                                                                     |
| COPD                                     | J44                                                                                                                                                                                                            | 491.21, 493.2, 496                                                                                                          |
| CKD STAGE IIB OR MORE                    | N18.32, N18.4, N18.5                                                                                                                                                                                           | 585.2-585.5                                                                                                                 |
| UNSTABLE ANGINA OR MYOCARDIAL INFARCTION | I20, I21                                                                                                                                                                                                       | 410, 412, 413                                                                                                               |
| AUTOIMMUNE INFLAMMATORY DISEASE          | M05, M06, M3, M04                                                                                                                                                                                              | 714, 710                                                                                                                    |
| HEPATITIS B                              | B16, B18.0, B18.1, B19.1                                                                                                                                                                                       | 070.2, 070.3                                                                                                                |
| HEPATITIS C                              | B17.1, B18.2, B19.2                                                                                                                                                                                            | 070.41, 070.44, 070.51, 070.54, 070.7                                                                                       |
| INHERITED AUTOIMMUNE DISORDER            | D80.0, D82.0, D80.4, D82.3, N41.4, Q82.8, D81.0, D81.1, D81.2, D81.3, E70.330, D76.1, D82.4, D82.2, D81.6, D81.7, D83, D80.2, D84.1, G11.3, D81.5, D81.8                                                       | 279.04, 279.12, 279.02, 279.8, 601.8, 757.2, 757.39, 279.2, 270.2, 288.4, 279.8, 279.06, 279.01, 277.6, 334.8, 277.2, 266.2 |

Function code for reference:

``` r
# library(ibdplexus)
# require(DiagrammeR)
# library(tidyverse)
# 
# data=load_data(datadir="C:/Users/sharvey/Documents/Plexus/EMR/",cohort="SPARC",domains="Diagnosis",data_type="EMR")
# 
# emr_extract_diagnosis <- function(
#   data,
#   inclusion =NULL ,
#   exclusion = NULL,
#   custominc=NULL,
#   customexc=NULL,
#   datecutoff=NULL,
#   days=36500
# ){
#   inclusion1=toupper(inclusion)
#   exclusion1=toupper(exclusion)
#   if("CANCER" %in% inclusion1){
#     inc="c|d0|d1|d2|d3|d4"
#   } else if ("WEIGHT LOSS" %in% inclusion1){
#     inc="R63.4"
#   } else if ("ABDOMINAL PAIN" %in% inclusion1){
#     inc="R10"
#   } else if ("STOMA" %in% inclusion1){
#     inc="L24.B0|L24.B1|L24.B3|Z93.3|Z93.2"
#   } else if ("PSC" %in% inclusion1){
#     inc="K83.01"
#   } else if ("GI BLEEDING" %in% inclusion1){
#     inc="K92.1"
#   } else if ("GI ULCER" %in% inclusion1){
#     inc="K25|K27|K28|K26|K63.3|K62.6"
#   } else if ("PERIANAL ABSCESS OR FISTULA" %in% inclusion1){
#     inc="K50.913|K50.914|K50.813|K50.814|K50.013|K50.014|K50.113|K50.114|K51.013|K51.014|K51.213|K51.214|K51.313|K51.314|K51.413|K51.414|K51.513|K51.514|K51.813|K51.814|K51.913|K51.914|K60|K61"
#   } else if ("SYSTEMIC FUNGAL INFECTION" %in% inclusion1){
#     inc="B39|B45|B38|B40|B46|B44|B37|B59"
#   } else if ("DEMYELINATING DISORDER" %in% inclusion1){
#     inc="G35|G36|G37"
#   } else if ("CELIAC" %in% inclusion1){
#     inc="K90.0"
#   } else if ("B2 OR B3" %in% inclusion1){
#     inc="K50.912|K50.112|K50.012|K50.812"
#   } else if ("MALNOURISHMENT" %in% inclusion1){
#     inc="E4"
#   } else if ("ANEMIA" %in% inclusion1){
#     inc="D50|D51|D52|D53"
#   } else if ("DIARRHEA" %in% inclusion1){
#     inc="R19.7|K59.1|K58.0"
#   } else if ("NAUSEA OR VOMITING" %in% inclusion1){
#     inc="R11"
#   } else if ("FEVER" %in% inclusion1){
#     inc="R50.9|R61"
#   } else if ("CDI" %in% inclusion1){
#     inc="A04.7"
#   } else if ("ARTHRITIS OR LOW BACK PAIN" %in% inclusion1){
#     inc="M13|M05|M06|M07|M08|M10|M11|M12|M14|M1A"
#   } else if ("DACTYLITIS" %in% inclusion1){
#     inc="L08.9"
#   } else if ("HYPOALBUMINEMIA" %in% inclusion1){
#     inc="E88.09"
#   } else if ("NON UC IBD DIAGNOSIS" %in% inclusion1){
#     inc="K50|K52.3|K52.83|K55.9"
#   } else if ("TOXIC MEGACOLON" %in% inclusion1){
#     inc="K59.31"
#   } else if ("FULMINANT COLITIS" %in% inclusion1){
#     inc="K55.03"
#   } else if ("INTRAABDOMINAL ABSCESS" %in% inclusion1){
#     inc="L02.211|K65.1"
#   } else if ("STRICTURE STENOSIS" %in% inclusion1){
#     inc="K56.69"
#   } else if ("COLON ADENOMA" %in% inclusion1){
#     inc="D12.2|D12.3|D12.4|D12.5|D12.6|K31.A2|K55.20"
#   } else if ("INFECTION" %in% inclusion1){
#     inc="L0|A49|A0"
#   } else if ("TUBERCULOSIS" %in% inclusion1){
#     inc="A15|A17|A18|A19"
#   } else if ("DIABETES" %in% inclusion1){
#     inc="E08|E09|E10|E11|E13"
#   } else if ("HYPERTENSION" %in% inclusion1){
#     inc="I10|I11|I12|I13|I15|I16"
#   } else if ("COPD" %in% inclusion1){
#     inc="J44"
#   } else if ("CKD STAGE IIB OR MORE" %in% inclusion1){
#     inc="N18.32|N18.4|N18.5"
#   } else if ("UNSTABLE ANGINA OR MYOCARDIAL INFARCTION" %in% inclusion1){
#     inc="I20|I21"
#   } else if ("AUTOIMMUNE INFLAMMATORY DISEASE" %in% inclusion1){
#     inc="M05|M06|M3|M04"
#   } else if ("HEPATITIS B" %in% inclusion1){
#     inc="B16|B18.0|B18.1|B19.1"
#   } else if ("HEPATITIS C" %in% inclusion1){
#     inc="B17.1|B18.2|B19.2"
#   } else if ("INHERITED AUTOIMMUNE DISORDER" %in% inclusion1){
#     inc="D80.0|D82.0|D80.4|D82.3|N41.4|Q82.8|D81.0|D81.1|D81.2|D81.3|E70.330|D76.1|D82.4|D82.2|D81.6|D81.7|D83|D80.2|D84.1|G11.3|Q82.8|D81.5|D81.8"
#   } else if ("CUSTOM" %in% inclusion1){
#     inc=custominc
#   }
#   if("diagnosis" %in% names(data)){
#     dxemricd=data$diagnosis %>%
#       filter(DIAG_SYSTEM_NAME!="Local"&DATA_SOURCE=="EMR") %>%
#       filter(grepl(inc,SRC_DIAG_CONCEPT_CODE,ignore.case=TRUE))
#     if("SKIN CARCINOMA" %in% exclusion1){
#       exc="C44.01|C44.02|C44.11|C44.12|C44.21|C44.22|C44.31|C44.32|C44.41|C44.42|C44.51|C44.52|C44.61|C44.62|C44.71|C44.72|C44.81|C44.82|C44.91|C44.92"
#       dxemricd1=dxemricd
#       dxemricd=dxemricd1 %>%
#         filter(!grepl(exc,SRC_DIAG_CONCEPT_CODE,ignore.case=TRUE))
#     }
#     if("CERVIX CARCINOMA" %in% exclusion1){
#       exc="d06"
#       dxemricd1=dxemricd
#       dxemricd=dxemricd1 %>%
#         filter(!grepl(exc,SRC_DIAG_CONCEPT_CODE,ignore.case=TRUE))
#     }
#     if("CUSTOM" %in% exclusion1){
#       exc=customexc
#       dxemricd1=dxemricd
#       dxemricd=dxemricd1 %>%
#         filter(!grepl(exc,SRC_DIAG_CONCEPT_CODE,ignore.case=TRUE))
#     }
#   }
#   if("patient_problem" %in% names(data)){
#     ppemricd=data$patient_problem %>%
#       filter(SOURCE_PROB_CODE_SYSTEM_NAME!="Local"&DATA_SOURCE=="EMR") %>%
#       filter(grepl(inc,SOURCE_PROB_CODE,ignore.case=TRUE))
#     if("SKIN CARCINOMA" %in% exclusion1){
#       exc="C44.01|C44.02|C44.11|C44.12|C44.21|C44.22|C44.31|C44.32|C44.41|C44.42|C44.51|C44.52|C44.61|C44.62|C44.71|C44.72|C44.81|C44.82|C44.91|C44.92"
#       ppemricd1=ppemricd
#       ppemricd=ppemricd1 %>%
#         filter(!grepl(exc,SOURCE_PROB_CODE,ignore.case=TRUE))
#     }
#     if("CERVIX CARCINOMA" %in% exclusion1){
#       exc="d06"
#       ppemricd1=ppemricd
#       ppemricd=ppemricd1 %>%
#         filter(!grepl(exc,SOURCE_PROB_CODE,ignore.case=TRUE))
#     }
#     if("CUSTOM" %in% exclusion1){
#       exc=customexc
#       ppemricd1=ppemricd
#       ppemricd=ppemricd1 %>%
#         filter(!grepl(exc,SOURCE_PROB_CODE,ignore.case=TRUE))
#     }
#   }
# 
# 
#   if("diagnosis" %in% names(data)&!"patient_problem" %in% names(data)){
#     if("CONSENT" %in% datecutoff){
#       dxemricd2=dxemricd
#       dxemricd=data$demographics %>%
#         extract_consent("SPARC") %>%
#         mutate(dateconsentcalc=as.Date(DATE_OF_CONSENT, format="%d-%B-%Y")) %>%
#         mutate(earliestemr=dateconsentcalc - days) %>%
#         #merge(encemr,by="DEIDENTIFIED_MASTER_PATIENT_ID")%>%
#         merge(dxemricd2,by="DEIDENTIFIED_MASTER_PATIENT_ID",allow.cartesian = TRUE) %>%
#         mutate(dateforfilter=as.Date(DIAGNOSIS_DATE, format="%d-%B-%Y")) %>%
#         filter(earliestemr<=dateforfilter&dateforfilter<=dateconsentcalc)
#     }
#     result=list(dxemricd)
#     names(result)=c("diagnosis")
#   }
#   if("patient_problem" %in% names(data)&!"diagnosis" %in% names(data)){
#     if("CONSENT" %in% datecutoff){
#       ppemricd2=ppemricd
#       ppemricd=data$demographics %>%
#         extract_consent("SPARC") %>%
#         mutate(dateconsentcalc=as.Date(DATE_OF_CONSENT, format="%d-%B-%Y")) %>%
#         mutate(earliestemr=dateconsentcalc - days) %>%
#         #merge(encemr,by="DEIDENTIFIED_MASTER_PATIENT_ID")%>%
#         merge(ppemricd2,by="DEIDENTIFIED_MASTER_PATIENT_ID",allow.cartesian = TRUE) %>%
#         mutate(dateforfilter=as.Date(PROBLEM_START_DATE, format="%d-%B-%Y")) %>%
#         filter(earliestemr<=dateforfilter&dateforfilter<=dateconsentcalc)
#     }
#     result=list(ppemricd)
#     names(result)=c("patient_problem")
#   }
#   if("patient_problem" %in% names(data)&"diagnosis" %in% names(data)){
#     if("CONSENT" %in% datecutoff){
#       dxemricd2=dxemricd
#       dxemricd=data$demographics %>%
#         extract_consent("SPARC") %>%
#         mutate(dateconsentcalc=as.Date(DATE_OF_CONSENT, format="%d-%B-%Y")) %>%
#         mutate(earliestemr=dateconsentcalc - days) %>%
#         #merge(encemr,by="DEIDENTIFIED_MASTER_PATIENT_ID")%>%
#         merge(dxemricd2,by="DEIDENTIFIED_MASTER_PATIENT_ID",allow.cartesian = TRUE) %>%
#         mutate(dateforfilter=as.Date(DIAGNOSIS_DATE, format="%d-%B-%Y")) %>%
#         filter(earliestemr<=dateforfilter&dateforfilter<=dateconsentcalc)
#       ppemricd2=ppemricd
#       ppemricd=data$demographics %>%
#         extract_consent("SPARC") %>%
#         mutate(dateconsentcalc=as.Date(DATE_OF_CONSENT, format="%d-%B-%Y")) %>%
#         mutate(earliestemr=dateconsentcalc - days) %>%
#         #merge(encemr,by="DEIDENTIFIED_MASTER_PATIENT_ID")%>%
#         merge(ppemricd2,by="DEIDENTIFIED_MASTER_PATIENT_ID",allow.cartesian = TRUE) %>%
#         mutate(dateforfilter=as.Date(PROBLEM_START_DATE, format="%d-%B-%Y")) %>%
#         filter(earliestemr<=dateforfilter&dateforfilter<=dateconsentcalc)
#     }
#     result=list(dxemricd,ppemricd)
#     names(result)=c("diagnosis","patient_problem")
#   }
# 
#   return(result)
# }
```