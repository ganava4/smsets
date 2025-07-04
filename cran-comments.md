## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

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

