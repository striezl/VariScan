# VariScan is a fast search tool for SAP program variants

Z_VARI_SCAN is supposed to be something like RS_ABAP_SOURCE_SCAN for program variants - so for example if you are doing some kind of refactoring and therefore want to find a certain value or pattern in all variants it could be your weapon of choice. Never again check variants manually!

## Manual, Screenshots
See [Wiki](https://github.com/striezl/VariScan/wiki).

## Prerequisits

Install [abapgit](https://docs.abapgit.org/guide-install.html) if you have not done so yet. 

## Installation

Clone the VariScan repo using [abapgit](https://github.com/abapGit/abapGit). Create a package first, we recommend Z_VARI.

## Authorization

The program Z_VARI_SCAN checks for the following authorization:
* Object = ZVARI
* Field = ZVARI
* Field Value = X

Note that SAP_ALL might need regenaration (transaction su21) due to the new authority object ZVARI that comes with installing VariScan.

You can program your own custom authority check by implementing BAdI Z_VARI_AUTH (use Enhancement Spot Z_VARI_AUTH in transaction se18).

The Program Z_VARI_SCAN uses RFC function module Z_VARI_SCAN for parallel processing. Note that this FM does not have an implicit authority check, S_RFC should be set accordingly.

## Constraints

System programs (trdir-rstat = S) are out of scope. 

S/4: Blacklisted programs (see Table ABLM_BLACKLIST or SAP Note 2249880) will not be checked, even if execution is temporarily allowed.

## Troubleshooting

### Dumps occur 

To get the variant content, each examined program must be generated at runtime which can cause various errors.  Try to exclude programs / packages that cause dumps via selection in Z_VARI_SCAN. Try to find those programs by examining dumps in transaction st22. At runtime each variant is checked in an exklusive tasks, so if one or more tasks fail (with runtime error) the overall reporting will still complete.

### Slow scan

Normally the program should work quite fast of course depending on your database, available tasks, the number of scanned packages and so on. Under good circumstances even a scan of the entire program repository can be possible within minutes or less. However single programs can cause a long runtime and slow down the analysis. For the moment those are quite hard to find, it might help to use the default exclusions with which Z_VARI_SCAN is delivered. Furthermore, if you can narrow the scope of your search to certain packages or programs. Another reason for a slow scan might be a too high or too low number of max tasks, both leading to inefficient processing, see Warning.
 
 ## Warning
 *Beware of setting parameter "Max. number of tasks" too high, it might slow down the entire system!* Always make sure not to consume most of your available dialog process (see transaction sm50), maybe a rule of thumb for dev system might be half of them max., of course heavily depending on your individual setup. Set the parameter to "1" if you want the most conservative setup (at the price of a longer runtime).
 
 ## Contribution
Your [contribution](https://docs.abapgit.org/guide-contributing.html) is very welcome!

## FAQ
For questions/comments/bugs/feature requests/wishes please create an issue.
