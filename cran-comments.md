## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

* checking package dependencies ... NOTE
  Imports includes 21 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.
  
  This package depends directly on the imported libraries, since it automates 
  data manipulation procedures, variable analysis and graphical representation, 
  so the included libraries are recommended to be included in the dependencies 
  section and not in suggest.
