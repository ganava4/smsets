## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.

### Version 2.0.0 is a new version (2026-01-24)

Two new functions (Levenetestsms.mv and its corresponding print method) have
been added to the package to allow calculation of robust Levene's tests for the 
comparison of variation of m samples in multivariate data. The vignette has been
updated accordingly.

### Version 1.2.3 is a patched version (2025-04-07)

Previous doi link unsuccessful. Fixed according to Uwe Ligges' instructions

### Version 1.2.2 is a patched version (2025-04-07).

doi link edited

Removed checks from github.

### Fixing package version 1.1.0 (2025-28-04).

Answers to Konstanze's indications 

* The reference link in the Description field has been added to the DESCRIPTION 
# file. Authors and year were also added.

* \value tags were added to the following .Rd files
      print.BoxM.F.Rd: \value
      print.Hotelling.mat.Rd: \value
      print.LeveneT2.Rd: \value
      print.Levenetests2s.mv.Rd: \value
      print.OnewayMANOVA.Rd: \value
      print.Penrose.dist.Rd: \value
      print.ttests2s.mv.Rd: \value
      print.VanValen.Rd: \value

* In addition, each print method implemented in the corresponding 
print function has been explained in the documentation (structure (class) 
and meaning of the output).

