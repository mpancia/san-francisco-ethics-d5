# San Francisco D5 Ethics Data

## Summary

This is a reproducible data pipeline that collects and parses the San Francisco ethics data associated to the 
2019 District 5 supervisor race. Roughly speaking, it does the following:

* Downloads the data from the SF Ethics Socrata endpoint (see the query in `plan.R`);
* Parses the raw data to extract the entities and relationships within the data, thought of as a graph (the data model is described below);
* Loads the entities and relations into a [Neo4j](https://neo4j.com/) graph database
* Corrects some of the data and adds extra information not present in the raw data
* Summarizes and extracts the data in several forms

## Methodology

The goal of this whole thing is to be as transparent as possible. There are several choices that are made in the process of the data parsing and analysis:

* People donating are identified by their first/last names (there are a small number of donors, and so collisions are unlikely, though this is possible);
* Employer names are standardized and re-mapped using regular expression rules found in `data/employer_replacements.csv` and some data cleaning that is found in the `match.R` file (see the `dplyr` mutations in the appropriate function);
* Employers are categorized by industry using the regulare expression rules found in `data/industry_mapping_patterns.csv` and used in `categorize.R`, [PRs accepted to improve or correct this!]

  The taxonomy of industries is as follows: 

  ```  
   * AIRLINE
   * ARCHITECTURE
   * BUILDING TRADES
   * CONSULTING (BUSINESS)
   * CONSULTING (DESIGN/ENGINEERING)
   * CONSULTING (POLITICS)
   * EDUCATION
   * ENERGY
   * ENTERTAINMENT
   * FINANCE
   * FITNESS
   * FOOD SERVICE
   * GOVERNMENT
   * HEALTHCARE (INSURANCE)
   * HEALTHCARE (OTHER)
   * HEALTHCARE (PROVIDER)
   * HOSPITALITY
   * MANUFACTURING
   * MARIJUANA
   * MEDIA
   * NON-PROFIT (COMMUNITY)
   * PRIVATE LAW
   * REAL ESTATE (DEVELOPER)
   * REAL ESTATE (LANDLORD)
   * REAL ESTATE (OTHER)
   * RETAIL (SMALL BUSINESS)
   * RETIRED
   * RIDESHARE
   * SELF-EMPLOYED
   * STUDENT
   * TECH
   * TELECOMMUNICATIONS
   * UNEMPLOYED
  ``` 
  
## Requirements for reproduction

This requires an installation of R, of course, as well as credentials for a Neo4j database to write the data to. 

These credentials should be stored in a `.env` file with the following variables (`GRAPHENE` refers to the Heroku add-in for hosting Neo4j, but any server will do):

```
GRAPHENEDB_URL=
GRAPHENEDB_BOLT_USER=
GRAPHENEDB_BOLT_PASSWORD=
```

This requires using the http/https endpoint for Neo4j, not Bolt -- the code in `plan.R` will read these in from the `.env` file in order to connect to the database, and uses the unsecure `http` by default. If you want to use the `https` url, modify the code there. 

## Usage

This uses [drake](https://github.com/ropensci/drake) as a workflow-management tool and [renv](https://github.com/rstudio/renv) for environment management. If you are unfamiliar with it, it's basically an R-based, data-aware version of GNU Make. If you run `make.R` interactively, it will run a pipeline that generates all of the data and does all of the loading to the Neo4j server defined as above.

Alternatively, you can use the dumps of the output data that are found in the `data/output` folder. This comes in several forms.