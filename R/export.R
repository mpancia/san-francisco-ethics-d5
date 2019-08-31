# Get the total donations per donor for each filer.
get_donor_totals_per_filer <- function(con) {
  query <- '
  MATCH (n:Filer)-[:HAS_DONATION]->(donation:Donation)-[:MADE_BY]->(d:Donor)
  OPTIONAL MATCH
    (donation)-[:MADE_BY]->(d:Donor)-[:WORKED_AT]->(e:Employer)-[:IS_MEMBER_OF]->(emp_industry:Industry)
  OPTIONAL MATCH
    (donation)-[:MADE_BY]->(d:Donor)-[:WORKED_AS]->(o:Occupation)-[:IS_MEMBER_OF]->(occ_industry:Industry)
  OPTIONAL MATCH
    (donation)-[:MADE_BY]->(d:Donor)-[:IS_MEMBER_OF]->(personal_industry:Industry)
  WITH *
  RETURN
    n.id as filer_id,
    n.name as filer_name,
    d.name as donor_name,
    SUM(donation.amount) as total_donation,
    COUNT(donation) as total_donations,
    substring(reduce(s="", name in collect(distinct occ_industry.name) | s + "|" + name), 1) as occupation_industries,
    substring(reduce(s="", name in collect(distinct emp_industry.name) | s + "|" + name), 1) as employee_industries,
    substring(reduce(s="", name in collect(distinct personal_industry.name) | s + "|" + name), 1) as individual_industries
  ORDER BY total_donation DESC
  '
  df <- query_to_df(query, con)
  df
}

get_industry_totals_per_filer <- function(con) {
  query <- "
  MATCH
  	(n:Filer)-[:HAS_DONATION]->(donation:Donation)-[:MADE_BY]->(d: Donor)
  OPTIONAL MATCH
  	(d)-[:WORKED_AS]->(occ:Occupation)-[:HAS_CLASS]->(occl: OccupationClass)
  OPTIONAL MATCH
    (d)-[:WORKED_AT]->(e:Employer)-[:IS_MEMBER_OF]->(emp_industry:Industry)
  OPTIONAL MATCH
    (d)-[:WORKED_AS]->(o:Occupation)-[:IS_MEMBER_OF]->(occ_industry:Industry)
  OPTIONAL MATCH
    (d)-[:IS_MEMBER_OF]->(personal_industry:Industry)
  WITH
    donation,
    occl,
    COALESCE(personal_industry.name, occ_industry.name, emp_industry.name) as industry_name,
    n.name as filer_name,
    n.id as filer_id
  RETURN
    filer_name,
    filer_id,
    industry_name,
    occl.name as occupation_class,
    sum(donation.amount) as total_donations
  ORDER BY total_donations DESC
  "
  df <- query_to_df(query, con)
  df
}

get_industry_totals_per_zipcode_per_filer <-function(con) {
  query <- "
  MATCH
    (f:Filer)-[:HAS_DONATION]->(donation:Donation)-[:MADE_BY]->(d: Donor)
  OPTIONAL MATCH
    (d)-[:WORKED_AT]->(e:Employer)-[:IS_MEMBER_OF]->(emp_industry:Industry)
  OPTIONAL MATCH
    (d)-[:WORKED_AS]->(o:Occupation)-[:IS_MEMBER_OF]->(occ_industry:Industry)
  OPTIONAL MATCH
    (d)-[:IS_MEMBER_OF]->(personal_industry:Industry)
  OPTIONAL MATCH
    (d)-[:RESIDES_IN]->(zipcode:ZipCode)
  WITH
    d,
    donation,
    COALESCE(personal_industry.name, occ_industry.name, emp_industry.name) as industry_name,
    f.name as filer_name,
    zipcode.zip_code as zipcode
  RETURN filer_name, industry_name, zipcode, sum(donation.amount) as total_donation_amount, count(distinct d.name) as total_donors
  "
  df <- query_to_df(query, con)
  df

get_employer_totals_per_filer <- function(con) {
  query <- "
  MATCH
    (employer:Employer)<-[:WORKED_AT]-(donor)-[:MADE_DONATION]->(donation:Donation)-[:MADE_TO]->(filer:Filer)
  OPTIONAL MATCH
    (employer)-[:IS_MEMBER_OF]->(industry:Industry)
  RETURN
    industry.name as industry_name,
    employer.name as employer_name,
    filer.name as filer_name,
    count(donation) as total_donations,
    sum(donation.amount) as total_donation_amount,
    count (distinct donor.name) as total_donors
    ORDER BY total_donors DESC
  "
  df <- query_to_df(query, con)
  df
}
