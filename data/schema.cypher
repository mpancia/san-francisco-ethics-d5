CREATE CONSTRAINT ON (filer:Filer) ASSERT filer.id IS UNIQUE;
CREATE CONSTRAINT ON (filer:Filer) ASSERT filer.name IS UNIQUE;

CREATE CONSTRAINT ON (donor:Donor) ASSERT donor.name IS UNIQUE;

CREATE CONSTRAINT ON (employer:Employer) ASSERT employer.name IS UNIQUE;
