# citationSearch

This package contains data and code for searching citations against several
online libraries by using Solr. 

To be used in the project repository: https://github.com/datalab-dev/tyler_scott/

# Setup

Have Solr instance running.

Install Solr from https://solr.apache.org  
Start an instance in cloud mode: `solr start -c`

Navigate to http://localhost:8983/solr/ to see if solr is running.

# Installation

```
git clone
R CMD BUILD
R CMD INSTALL
```

or with devtools

```
devtools::install_github("ucdavisdatalab/citationSearch")
```

or from within project directory

```
devtools::install()
```

# Contact

Arthur Koehl avkoehl at ucdavis dot edu
