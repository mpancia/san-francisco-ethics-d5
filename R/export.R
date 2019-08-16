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
    (d)-[:WORKED_AT]->(e:Employer)-[:IS_MEMBER_OF]->(emp_industry:Industry)
  OPTIONAL MATCH
    (d)-[:WORKED_AS]->(o:Occupation)-[:IS_MEMBER_OF]->(occ_industry:Industry)
  OPTIONAL MATCH
    (d)-[:IS_MEMBER_OF]->(personal_industry:Industry)
  WITH
    donation,
    COALESCE(personal_industry.name, occ_industry.name, emp_industry.name) as industry_name,
    n.name as filer_name,
    n.id as filer_id
  RETURN filer_name, filer_id, industry_name, sum(donation.amount) as total_donations
  "
  df <- query_to_df(query, con)
  df
}
