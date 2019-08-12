# Get the total donations per donor for each filer.
get_donor_totals_per_filer <- function(con) {
  query <- "
  MATCH (n:Filer)-[:HAS_DONATION]->(q:Donation)-[:MADE_BY]->(d:Donor)
  RETURN
    n.id as filer_id,
    n.name as filer_name,
    d.name as donor_name,
    SUM(q.amount) as total_donation
  ORDER BY total_donation DESC
  "
  query_to_df(query, con)
}

get_industry_totals_per_filer <- function(con) {
  query <- "
  MATCH
    (n:Filer)-[:HAS_DONATION]->(donation:Donation)
  OPTIONAL MATCH
    (donation)-[:MADE_BY]->(d:Donor)-[:WORKED_AT]->(e:Employer)-[:IS_MEMBER_OF]->(emp_industry:Industry)
  OPTIONAL MATCH
    (donation)-[:MADE_BY]->(d:Donor)-[:WORKED_AS]->(o:Occupation)-[:IS_MEMBER_OF]->(occ_industry:Industry)
  OPTIONAL MATCH
    (donation)-[:MADE_BY]->(d:Donor)-[:IS_MEMBER_OF]->(personal_industry:Industry)
  RETURN
    sum(donation.amount) as total_amount,
    COALESCE(personal_industry.name, occ_industry.name, emp_industry.name) as industry_name,
    n.name as filer_name,
    n.id as filer_id
  "
  query_to_df(query, con)
}
