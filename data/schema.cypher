CREATE CONSTRAINT ON (filer:Filer) ASSERT filer.id IS UNIQUE;
CREATE CONSTRAINT ON (filer:Filer) ASSERT filer.name IS UNIQUE;

CREATE CONSTRAINT ON (donor:Donor) ASSERT donor.name IS UNIQUE;

CREATE CONSTRAINT ON (employer:Employer) ASSERT employer.name IS UNIQUE;

CREATE CONSTRAINT ON (zipcode:ZipCode) ASSERT zipcode.zip_code IS UNIQUE;

CREATE CONSTRAINT ON (industry:Industry) ASSERT industry.name IS UNIQUE;

CREATE CONSTRAINT ON (state:State) ASSERT state.abbreviation IS UNIQUE;
