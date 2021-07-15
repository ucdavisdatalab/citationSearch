# citationSearch

This package contains data and code for searching citations against several
online libraries by using Solr. 

To be used in the main project repo: 
https://github.com/datalab-dev/tyler_scott/

# Setup

Have Solr instance running.

Install Solr from https://solr.apache.org  
Start an instance in cloud mode: `solr start -c`

Navigate to http://localhost:8983/solr/ to see if solr is running.

# Installation

Clone this repo and install with `devtools` from within project directory

```
devtools::install()
```

# Quick Start

These instructions walk through searching anystyle citations against the
indexes. Assumes you don't need to redownload the library record data, and
instead will be using the versions that come with this package.

First, load the package:
```
library(citationSearch)
```

Then build the index. This only needs to be done the first time.
```
index_all() # pass overwrite=TRUE to recreate the index
```

Search a single citation against the index:
```
citation = anystyle[1,] # take first record
query = search_construct_query(citation)
hits = search_collection("all", query)
```

# Working on this package

Make changes to the code, then run `devtools::load_all()` and test them.
To update documentation and NAMESPACE file `devtools::document()`.

# Contact

Arthur Koehl avkoehl at ucdavis dot edu
