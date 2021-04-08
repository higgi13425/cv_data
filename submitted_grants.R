# use datapasta to paste vector from submitted grants
# update last paste date here:
# 

submitted <- c("Grant#\tTitle\tSponsor Name\tRole in Grant\tTotal Direct/Indirect\tBegin Dt\tEnd Dt", "Precision Medicine In Inflammatory Bowel Disease: Refining the Clinical and Genomic Predictors of Response to Anti-IL-12/23 Therapy", "[Updt By: Grants Load]", "NIH-DHHS-US\tCo-I without Effort\t$981,040\t07/21\t06/26\tEdit\tDelete", "AXL is a Therapeutic Target for Intestinal Fibrosis", "[Updt By: Grants Load]", "American Gastroenterological A\tCo-I without Effort\t$300,000\t07/21\t06/24\tEdit\tDelete", "Validation and Characterization of Pleiotropic IBD Risk Variants", "[Updt By: Grants Load]", "Crohn's and Colitis Foundation\tCo-I without Effort\t$270,000\t07/21\t06/24\tEdit\tDelete", "AXL as a Therapeutic Target for Intestinal Fibrosis", "[Updt By: Grants Load]", "Crohn's and Colitis Foundation\tCo-I without Effort\t$270,000\t07/21\t06/24\tEdit\tDelete", "Improving Access Disparities to High-Cost Biologic Medications for Inflammatory Bowel Disease Patients", "[Updt By: Grants Load]", "American Gastroenterological A\tPI\t$300,000\t07/21\t06/24\tEdit\tDelete", "Assessing biomarkers of intestinal fibrosis and inflammation in Crohn's Disease via an endoscopic imaging catheter", "[Updt By: Grants Load]", "NIH-DHHS-US\tPI\t$491,260\t01/21\t12/21\tEdit\tDelete", "Assessing biomarkers of intestinal fibrosis and inflammation in Crohn's Disease via an endoscopic imaging catheter", "[Updt By: Grants Load]", "NIH-DHHS-US\tPI\t$3,542,676\t07/20\t06/25\tEdit\tDelete")


submitted %>% 
  str_replace_all(pattern = "\tEdit\tDelete", "") %>% 
  read_tsv() %>% 
  janitor::clean_names() %>% 
  filter(grant_number != "[Updt By: phiggins]") %>% 
  filter(grant_number != "[Updt By: Grants Load]") %>%   
  mutate(sponsor_name = case_when(is.na(sponsor_name) ~ lead(grant_number), TRUE ~ sponsor_name)) %>% 
  mutate(role_in_grant = case_when(is.na(role_in_grant) ~ lead(title), TRUE ~ role_in_grant)) %>% 
  mutate(total_direct_indirect = case_when(is.na(total_direct_indirect) ~ lead(sponsor_name), TRUE ~ total_direct_indirect)) %>% 
  mutate(begin_dt = case_when(is.na(begin_dt) & str_detect(string = total_direct_indirect, pattern = "$") ~ lead(role_in_grant), TRUE ~ begin_dt)) %>% 
  mutate(end_dt = case_when(is.na(end_dt) & str_detect(total_direct_indirect, "$") ~ lead(total_direct_indirect), TRUE ~ end_dt)) %>% 
  mutate(title = case_when(is.na(title) ~ grant_number, TRUE ~ title)) %>% 
  mutate(grant_number = case_when(title == grant_number ~ "", TRUE ~ grant_number)) %>% 
  filter(str_detect(end_dt, "/")) ->
submitted_grants

