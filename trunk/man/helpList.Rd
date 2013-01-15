\name{helpList} 
\alias{helpList} 
\docType{package} 
\title{metrumrg: Defined Functions}
\description{Exhaustive list of functions defined by \pkg{metrumrg}.}
\details{
\link[=metrumrg]{metrumrg}

\itemize{
\item \link{[.comment} subset a comment
\item \link{[.conditioned} subset a conditioned object
\item \link{[.digest} subset a digest object
\item \link{[.flag} subset a flag
\item \link{[.initList} subset initList
\item \link{[.keyed} subset a keyed data frame
\item \link{[.nmctl} subset a control stream object
\item \link{[.temporal} subset a temporal object
\item \link{[[.comment} select element from a comment
\item \link{[[.flag} select element from a flag
\item \link{[[.nmctl} select element from a control stream object
\item \link{[[.temporal} select element from a temporal object
\item \link{[<-.temporal} assign to a temporal object
\item \link{\%+\%} generic set union binary operator
\item \link{\%&\%} generic set intersection binary operator
\item \link{\%-\%} generic set difference binary operator
\item \link{\%u\%} generic set union binary operator
\item \link{\%n\%} generic set intersection binary operator
\item \link{\%+\%.default} vector set union
\item \link{\%&\%.default} vector set intersection
\item \link{\%-\%.default} vector set difference
\item \link{\%contains\%} test whether x contains y
\item \link{\%crossed.on\%} test whether a is crossed on b
\item \link{\%crosses\%} test whether a crosses b
\item \link{\%nested.in\%} test whether a is nested in b
\item \link{\%nests\%} test whether a nests b
\item \link{$.init} select init element
\item \link{$<-.init} assign init element
\item \link{accept} document the acceptance of an R installation
\item \link{acceptance} display the acceptance documentation for an R installation
\item \link{acr} calculate accumulation ratio
\item \link{addl.data.frame} collapse dose records by means of ADDL notation
\item \link{addl.default} calculate the ADDL data item (NONMEM) from explicit dose records
\item \link{addl} calculate addl
\item \link{after} test whether elements occur after some condition is \code{TRUE}
\item \link{aggregate.keyed} aggregate a keyed data frame
\item \link{align.decimal} align numeric column in latex table on the decimal
\item \link{and.keyed} produce inner join of two keyed data frames
\item \link{as.best.data.frame} coerce a data.frame to unambigous numeric or character columns
\item \link{as.best.default} coerce a vector unambiguously to numeric or character
\item \link{as.best.digest} coerce a digest to unambiguous numeric or character
\item \link{as.best} generic
\item \link{as.character.comment} convert comment to character
\item \link{as.character.flag} convert flag to character
\item \link{as.character.init} convert init to character
\item \link{as.character.initList} convert initList to character
\item \link{as.character.nmctl} convert nmctl to character
\item \link{as.character.temporal} convert temporal to character
\item \link{as.chartime.numeric} convert numeric to time
\item \link{as.chartime} convert to time (generic, internal)
\item \link{as.comment.comment} convert comment to comment
\item \link{as.comment.default} convert default to comment
\item \link{as.comment} convert to comment
\item \link{as.conditioned.digest} convert a digest to conditioned
\item \link{as.conditioned.isolated} convert isolated to conditioned
\item \link{as.conditioned.keyed} convert keyed to conditioned
\item \link{as.conditioned} split a data frame per levels of conditioning variables
\item \link{as.csv.filename.character} convert character to csv.filename 
\item \link{as.csv.filename} convert to csv.filename
\item \link{as.data.frame.block} convert a block of lines to data frame
\item \link{as.data.frame.comment} convert comment to data frame
\item \link{as.data.frame.flag} convert flag to data frame
\item \link{as.day.day} coerce day to day
\item \link{as.day.hour} coerce hour to day
\item \link{as.day.month} coerce month to day
\item \link{as.day.nominal} coerce nominal to day             
\item \link{as.day.numeric} coerce numeric to day
\item \link{as.day.week} coerce week to day
\item \link{as.day.year} coerce year to day
\item \link{as.day} coerce to day
\item \link{as.digest.data.frame} convert data.frame to digest
\item \link{as.digest.digest} convert digest to digest
\item \link{as.digest.keyed} convert keyed to digest
\item \link{as.digest.nm} convert nm to digest
\item \link{as.digest} resolve static components of an object hierarchy
\item \link{as.document.character} convert tex fragments to latex document
\item \link{as.document.data.frame} convert data.frame to latex document
\item \link{as.document} convert to latex document
\item \link{as.file.runlog} convert to convert runlog to file
\item \link{as.filename.character} convert character to filename
\item \link{as.filename} convert to filename
\item \link{as.flag.default} convert default to flag
\item \link{as.flag.flag} convert flag to flag
\item \link{as.flag} convert to flag
\item \link{as.halfmatrix.default} convert a vector to halfmatrix
\item \link{as.halfmatrix.halfmatrix} preserve halfmatrix
\item \link{as.halfmatrix} generic
\item \link{as.hour.day} coerce day to hour
\item \link{as.hour.hour} coerce hour to hour
\item \link{as.hour.minute} coerce minute to hour
\item \link{as.hour.nominal} coerce nominal to hour
\item \link{as.hour.numeric} coerce numeric to hour
\item \link{as.hour.second} coerce second to hour
\item \link{as.hour} coerce to hour
\item \link{as.init.init} convert init to init
\item \link{as.init.numeric} convert numeric to init
\item \link{as.init} convert to init
\item \link{as.initList.character} convert character to initList
\item \link{as.initList.initList} convert initList to initList
\item \link{as.initList.list} convert list (of init) to initList
\item \link{as.initList.numeric} convert numeric to initList
\item \link{as.initList} convert to initList (generic)
\item \link{as.isolated.digest} convert digest to isolated
\item \link{as.isolated.isolated} convert isolated to isolated
\item \link{as.isolated.keyed} convert keyed to isolated
\item \link{as.isolated} split a data frame column-wise into members with only one non-key column
\item \link{as.keyed.data.frame} convert data frame to keyed
\item \link{as.keyed.nm} convert nm to isolated
\item \link{as.keyed} convert to keyed
\item \link{as.list.nmctl} convert nmctl to list
\item \link{as.matrix.halfmatrix} convert a vector to a symmetric matrix
\item \link{as.mDate.character} convert character to mDate
\item \link{as.mDate.Date} convert Date to mDate
\item \link{as.mDate.dates} convert dates (chron) to mDate
\item \link{as.mDate.mDate} convert mDate to mDate
\item \link{as.mDate.numeric} convert numeric to mDate
\item \link{as.mDate.sasdate} convert SAS date to mDate
\item \link{as.mDate} convert to mDate
\item \link{as.mDateTime.character} convert character to mDateTime
\item \link{as.mDateTime.chron} convert chron to mDateTime
\item \link{as.mDateTime.mDate} convert mDate to mDateTime
\item \link{as.mDateTime.mDateTime} convert mDateTime to mDateTime
\item \link{as.mDateTime.numeric} convert numeric to mDateTime
\item \link{as.mDateTime.POSIXct} convert POSIXct to mDateTime
\item \link{as.mDateTime.POSIXlt} convert POSIXlt to mDateTime
\item \link{as.mDateTime} convert to mDateTime
\item \link{as.minute.hour} coerce hour to minute
\item \link{as.minute.minute} coerce minute to minute
\item \link{as.minute.nominal} coerce nominal to minute
\item \link{as.minute.numeric} coerce numeric to minute
\item \link{as.minute.second} coerce second to minute
\item \link{as.minute} coerce to minute
\item \link{as.month.day} coerce day to month
\item \link{as.month.month} coerce month to month
\item \link{as.month.nominal} coerce nominal to month
\item \link{as.month.numeric} coerce numeric to month
\item \link{as.month} coerce to month
\item \link{as.moot} convert to moot
\item \link{as.motif.digest} create motif for digest
\item \link{as.motif.keyed} create motif for keyed
\item \link{as.motif.motif} create motif from motif
\item \link{as.motif.nm} create motif for nm
\item \link{as.motif} create default motif for various objects
\item \link{as.mTime.character} convert character to mTime
\item \link{as.mTime.mTime} convert mTime to mTime
\item \link{as.mTime.numeric} convert numeric to mTime
\item \link{as.mTime.times} convert times (chron) to mTime
\item \link{as.mTime} convert to mTime
\item \link{as.nm.data.frame} convert data frame to nm
\item \link{as.nm} convert to nm (NMTRAN-ready NONMEM data set)
\item \link{as.nmctl.character} convert character to nmctl
\item \link{as.nmctl} convert to nmctl
\item \link{as.numeric.chartime} convert chartime to numeric
\item \link{as.pdf.character} convert tex fragments to pdf
\item \link{as.pdf.data.frame} convert data.frame to pdf
\item \link{as.pdf.document} convert latex document to pdf
\item \link{as.pdf} convert to pdf
\item \link{as.pxml.ext} convert a param file to XML
\item \link{as.rigged} convert to rigged
\item \link{as.roles.digest} create roles for digest
\item \link{as.roles.keyed} create roles for keyed
\item \link{as.roles} create default roles for parts of objects
\item \link{as.runlog.file} convert file to runlog
\item \link{as.runlog.unilog} convert unilog to runlog
\item \link{as.second.day} coerce day to second
\item \link{as.second.hour} coerce hour to second
\item \link{as.second.minute} coerce minute to second
\item \link{as.second.nominal} coerce nominal to second
\item \link{as.second.numeric} coerce numeric to second
\item \link{as.second.second} coerce second to second
\item \link{as.second} coerce to second (time unit)
\item \link{as.tabular} convert to a latex tabular environment
\item \link{as.unilog.lst} convert nonmem output (.lst) to unilog
\item \link{as.unilog.pxml} convert nonmem output (.ext) to unilog
\item \link{as.unilog.run} convert run to unilog
\item \link{as.unilog.runlog} convert runlog to unilog
\item \link{as.week.day} coerce day to week
\item \link{as.vector.keyed} coerce keyed to vector
\item \link{as.week.nominal} coerce nominal to week
\item \link{as.week.numeric} coerce numeric to week
\item \link{as.week.week} coerce week to week
\item \link{as.week} coerce to week
\item \link{as.xml.character} convert character to XML
\item \link{as.xml.data.frame} convert data.frame to XML
\item \link{as.xml.default} convert arbitrary object to XML
\item \link{as.xml} convert object to XML
\item \link{as.year.day} coerce day to year
\item \link{as.year.numeric} coerce numeric to year
\item \link{as.year.year} coerce year to year
\item \link{as.year} coerce to year
\item \link{at} test whether elements occur where some condition is \code{TRUE}
\item \link{attribute} encode an attribute for an XML open tag
\item \link{auc} calculate area under the curve
\item \link{AUC} calculate the area under the curve for each subject over the time interval for dv using the trapezoidal rule
\item \link{aug} add named elements to an object
\item \link{autolog.pl} prepare command lines for autolog.pl.
\item \link{backtrans} backtransform cols in x, returning x
\item \link{badAmt.nm} AMT is NA where EVID is 1
\item \link{badAmt} generic
\item \link{badDv.nm} DV is NA where EVID is 0
\item \link{badDv} generic
\item \link{badII.nm} II is greater than 0 where ADDL is NA or 0 (unless SS > 0)
\item \link{badII} generic
\item \link{bakfor} impute NAs using last observation carried forward after next observation carried backward
\item \link{before} test whether elements occur before some condition is \code{TRUE}
\item \link{bin} calculate bin limits for x and place each value in a bin, returning the number of values in each bin
\item \link{bmi} calculate body mass index
\item \link{bounds} calculate normal quantiles at bounding probabilities
\item \link{bracket} create an XML tag
\item \link{breaks} calculate breaks between grouped elements
\item \link{bsa} calculate body surface area
\item \link{bundleRead} read filtered directory contents into a list
\item \link{c.comment} combine comment
\item \link{c.flag} combine flag
\item \link{c.temporal} combine temporal
\item \link{cavg} calculate average concentration
\item \link{check.subjects} check for missing data, numeric data, ranges, etc.
\item \link{clear} drop regular patterns from a character vector
\item \link{CLNR} delete files and directories and log what was deleted to a text file in the project
\item \link{closers} set closing positions for wikmath nests
\item \link{cmax} calculate maximum steady-state concentration
\item \link{cmin} calculate minimum steady-state concentration
\item \link{colname<-} change identified column names
\item \link{command} generate a latex command
\item \link{compileflag} calculate compiler flag for qsub
\item \link{compiler} extract compiler specification from configuration file
\item \link{compute.cwres} compute conditional weighted residuals
\item \link{config} calculate path to configuration file
\item \link{constant.default} test whether values of x are contant within an index
\item \link{constant} generic
\item \link{contains} test a character vector for occurences of a pattern
\item \link{convertLines} convert line endings
\item \link{covariatePlots} create diagnostic plots for covariates
\item \link{crcl} calculate creatinine clearance
\item \link{crosses} test whether a crosses b
\item \link{css} calculate steady-state concentration
\item \link{ctl2xml} isolate xml comments from a control stream
\item \link{cvLognormal} calculate coefficient of variation for log-normal data
\item \link{cvNormal} calculate coefficient of variation for normal data
\item \link{cwresPlots} create conditional weighted residual diagnostic plots
\item \link{dataFormat} combine NONMEM predictions, parameter estimates, and covariates into a standard format
\item \link{dataSynthesis} build a standard plotting data set for a given NONR run
\item \link{deranged.data.frame} instantiate records implied by range data
\item \link{deranged.keyed} instantiate records implied by range data
\item \link{deranged} generic
\item \link{diagnosticPlots} create standard diagnostic plots
\item \link{digest} alias for as.digest
\item \link{distance} calculate element positions relative to the \eqn{n^{th}}{n^th} \code{TRUE} element positions (usually, within levels of an index)
\item \link{divide.keyed} for keyed data frames, rows in x that have matching keys in y
\item \link{dupKeys.default} give a logical index to duplicated rows of a keyed data frame, AND the rows of which they are duplicates
\item \link{dupKeys.nm} default behavior, after stripping comments
\item \link{dupKeys} generic
\item \link{dynamic.data.frame} return the dynamic portion of a data frame
\item \link{dynamic.keyed} return the dynamic portion of a keyed data frame
\item \link{dynamic} generic
\item \link{electronicAppendix} create an FDA-style electronic appendix from a subversioned directory
\item \link{episcript} run a script in an environment that defines the passed arguments
\item \link{explicitPath} add separators to control stream paths that have none
\item \link{extfile} extract arbitrary file specification from a control stream
\item \link{extractPath} isolate file path from surrounding text (MSF, TAB files).
\item \link{f} alias for as.flag
\item \link{falseAmt.nm} AMT defined where EVID is not 1
\item \link{falseAmt} generic
\item \link{falseDv.nm} DV is defined where EVID is not 0
\item \link{falseDv} generic
\item \link{filename} generate standard file name (internal)
\item \link{findNonmemCommand} find available NONMEM executables
\item \link{first} repeat, for each element, the first element where a condition is \code{TRUE} (usually, within levels of an index)
\item \link{fixed.init} find out if an init is fixed
\item \link{fixed.initList} find out if elements of an initList are fixed
\item \link{fixed} find out if something is fixed (generic)
\item \link{fixed<-.init} set the value of 'fixed' for an init
\item \link{fixed<-.initList} set the values of 'fixed' for elements of an initList 
\item \link{fixed<-} set the value of 'fixed'
\item \link{fixedwidth.data.frame} convert a data.frame to fixed-width format
\item \link{fixedwidth} convert to fixed-width format
\item \link{fixFile} rewrite run name in control stream file specifications
\item \link{fixProblem} rewrite run name in control stream PROBLEM statement
\item \link{follow} follow progress of NONMEM runs
\item \link{forbak} impute NAs using last observation carried forward and then next observation carried backward
\item \link{format.comment} format comment 
\item \link{format.flag} format flag
\item \link{format.init} format init
\item \link{format.initList} format initList
\item \link{format.legacy} pretty-print legacy information (conditioned objects)
\item \link{format.mDate} format mDate
\item \link{format.mDateTime} format mDateTime
\item \link{format.mTime} format mTime
\item \link{format.nmctl} format nmctl
\item \link{format.nominal} format nominal
\item \link{ftable2data.frame} convert ftable to data.frame as displayed
\item \link{getCovs} return non-commented non-duplicated rows of a NONMEM data set (internal)
\item \link{getCwres} return the conditional weighted residuals pertinent to a given run (internal, expects cwtab1.deriv)
\item \link{getdname} extract the path and file from the DATA block in the control stream associated with filename (internal)
\item \link{getPars} return the non-duplicated (ID) rows corresponding to the specified filename (internal)
\item \link{getTabs} return the observation rows (EVID==0) corresponding to the specified filename (internal)
\item \link{glue} paste with no separator
\item \link{groupnames} construct a single vector of group names from possibly multiple indices to the plotting data set (internal)
\item \link{half.matrix} vectorize the lower triangular portion of a symmetric matrix
\item \link{half} generic
\item \link{hash} prepend a character to each line of output
\item \link{head.digest} show head of each table in a digest
\item \link{hidden.data.frame} show commented rows of a data frame
\item \link{hidden} generic
\item \link{hide.data.frame} comment out the specified rows of a data frame, appending a flag that suggests the reason
\item \link{hide} generic
\item \link{ibw} calculate ideal body weight
\item \link{ignored} check which rows of an input data set were ignored by NONMEM
\item \link{ind.cwres} compute conditional weighted residuals (internal)
\item \link{index.conditioned} index a conditioned object
\item \link{index.digest} index a digest
\item \link{index.isolated} index an isolated object
\item \link{index.keyed} index a keyed data frame
\item \link{index.nm} index an nm object
\item \link{index} create an index plot
\item \link{inner.data.frame} reduce columns to inner quantiles by imputing NA
\item \link{inner} generic
\item \link{is.alpha} test whether text is letters-only
\item \link{is.cwres.readable.file} compute conditional weighted residuals (internal)
\item \link{is.defined} test whether elements are defined
\item \link{is.diagonal} test whether parameter label represents matrix diagonal element
\item \link{is.fixed} detect thetas in parameter labels
\item \link{is.iiv} detect omegas in parameter labels
\item \link{is.latex.token} test whether text is valid latex command or environment
\item \link{is.offdiagonal} test whether parameter label represents matrix offdiagonal element
\item \link{is.one.nonalpha} test whether text is singular non-letter
\item \link{is.random} detect omegas and sigmas in parameter labels
\item \link{is.residual} detect sigmas in parameter labels
\item \link{is.square.matrix} test whether a matrix is square
\item \link{is.square} generic
\item \link{isSubversioned} check if files are subversioned
\item \link{isSubversionedFile} check if one file is subversioned
\item \link{iterations} retrieve iteration statistics from NONMEM output file
\item \link{justUnits} extract units from wikimath
\item \link{ke} calculate elimination constant
\item \link{key} return the key of a keyed data frame
\item \link{key<-} assign a key
\item \link{last} repeat, for each element, the last element where a condition is \code{TRUE} (usually, within levels of an index)
\item \link{latest} identify the latest of each set of variants
\item \link{latex.args} format latex arguments
\item \link{latex.options} format latex options
\item \link{lbm} calculate lean body mass
\item \link{left.keyed} produce left join of two keyed data frames
\item \link{legacy} extract legacy from a conditioned object
\item \link{legacy<-} assign legacy for a conditioned object
\item \link{lhs} extract left-hand side of wikimath
\item \link{locf} impute NA using last observation carried forward
\item \link{lookup.one} translate among parameter attributes (scalar)
\item \link{lookup} translate among parameter attributes (vector)
\item \link{lower} calculate normal quantiles at lower probability of interval
\item \link{ltable.data.frame} convert a data.frame to a latex table
\item \link{ltable} convert to a latex table
\item \link{lyse.data.frame} return the static and dynamic portions of a data frame
\item \link{lyse.keyed} return the static and dynamic portions of a keyed data frame
\item \link{lyse} generic
\item \link{map} map one set of values to another
\item \link{maxChar} return the number of printed characters for the widest element of x
\item \link{merge.keyed} merge keyed data frames, preserving attributes
\item \link{merge.nm} merge nm objects
\item \link{metaMerge.character} recursively merge based on a vector of file names
\item \link{metaMerge.default} merge two groomed objects
\item \link{metaMerge.list} recursively merge elements of a list
\item \link{metaMerge} generic 
\item \link{metaSub.character} systematically substitute elements in a character vector
\item \link{metaSub.filename} systematically substitute elements in a text file
\item \link{metaSub} generic
\item \link{metrumrgURL} view a file in the metrumrg package
\item \link{minus.keyed} drop rows in x that have matching rows in y
\item \link{minus.moot} drop moot rows
\item \link{moot.nm} identify moot rows (rows not influencing NONMEM estimates)
\item \link{moot} generic
\item \link{motif} alias for as.motif
\item \link{msffile} extract msffile specification from a control stream
\item \link{naInContext} display rows of data with missing values, as well as other rows with the same key
\item \link{naKeys.default} detect rows for which one or more key fields is NA
\item \link{naKeys.nm} default behavior, after stripping comments
\item \link{naKeys} generic
\item \link{name<-} change identified object names
\item \link{nasum} diagnose missingness
\item \link{ncomma} report names as comma-delimited
\item \link{nest} nest an XML fragment in a parent element
\item \link{nix} identify Unix-like platforms (internal)
\item \link{nm.pl} prepare command lines for nm.pl
\item \link{nm} give a zero-row data frame with suitable columns and column classes; i.e. a template for NMTRAN data sets
\item \link{nmPlots} a list of functions that plot nm objects
\item \link{nmVersion} extract NONMEM version specification from configuration file
\item \link{nocb} impute NA with next observation carried backward
\item \link{NONR} run NONMEM and create diagnostic plots
\item \link{NONR72} run NONMEM 7.2.0 or later
\item \link{noPk.nm} rows where EVID is never 0 within SUBJ
\item \link{noPk} generic
\item \link{nospace} drop spaces from text
\item \link{not.keyed} keyed data frame rows that have problematic keys (NA, dups)
\item \link{noUnits} drop units from wikimath
\item \link{nth} repeat, for each element, the \eqn{n^{th}}{n^th} element where a condition is \code{TRUE}  (usually, within levels of an index)
\item \link{nxt} calculate the next element for each element of x
\item \link{offdiag.halfmatrix} select just the off-diagonal elements of a halfmatrix
\item \link{offdiag} generic
\item \link{omegacor} convert omega covariance in NONMEM output to correlation matrix
\item \link{only} repeat, for each element, the only element where a condition is \code{TRUE} (usually, within levels of an index)
\item \link{Ops.keyed} use concise syntax to join data frames
\item \link{ord.halfmatrix} compute the order of a half matrix
\item \link{ord.matrix} compute the order of a symmetric matrix
\item \link{ord} generic
\item \link{packageCheck} load a package and run package-level examples
\item \link{padded} pad a numeric with leading zeros
\item \link{panel.bar} for each level, plot vertical (or horizontal) bars corresponding to x (y).
\item \link{panel.covplot} plot distributions with respect to reference values; i.e. combine panel.cuts, panel.densitystrip, and panel.ref
\item \link{panel.cuts} for each level, plot percent observations in each vertical category defined by cuts
\item \link{panel.densitystrip} for each level, plot a filled polygon representing smoothed density of the distribution
\item \link{panel.hist} for each level, plot a histogram
\item \link{panel.ref} plot a reference region in a stripplot
\item \link{panel.stratify} handle strips (levels) of data one at a time
\item \link{parameter2wiki} convert parameter labels to wikmath
\item \link{params} list documented model parameters
\item \link{parens} wrap text in parentheses
\item \link{parfile} extract parfile specification from a control stream
\item \link{partab} construct a model parameter table
\item \link{percent} convert to percent
\item \link{plot.conditioned} plot a conditioned object
\item \link{plot.digest} plot a digest
\item \link{plot.isolated} plot isolated
\item \link{plot.keyed} plot keyed
\item \link{plot.nm} generate standard plots for an nm object using \code{\link{nmPlots}}
\item \link{plotfilename} make a name for a diagnostic plot file
\item \link{PLOTR} create diagnostic plots for NONMEM runs
\item \link{plus.keyed} produce and outer join of keyed data frames
\item \link{plus.rigged} merge an nm object with a rigged data frame
\item \link{pool} test for unique and shared elments among two vectors
\item \link{posmat} coerce a matrix to be positive definite
\item \link{predoseDv.nm} DV is defined before the first record within SUBJ where EVID is 1
\item \link{predoseDv} generic
\item \link{prev} calculate the previous element for each element of x
\item \link{print.comment} print comment
\item \link{print.flag} print flag
\item \link{print.halfmatrix} print halfmatrix
\item \link{print.init} print init
\item \link{print.initList} print initList
\item \link{print.keyed.summary} print keyed summary
\item \link{print.nm.summary} print nm summary
\item \link{print.nmctl} print nmctl
\item \link{print.nominal} print nominal
\item \link{print.temporal} print temporal
\item \link{progress} summarize progress of NONMEM runs
\item \link{purge.dir} purge a directory (internal)
\item \link{purge.files} purge files (internal)
\item \link{purgeRunDir} delete certain files from run directories
\item \link{qstat} invoke SGE qstat
\item \link{qsub} prepare commandlines for qsub
\item \link{raised.keyed} produce sequential left join of two keyed data frames
\item \link{read.cwres.data} compute conditional weighted residuals (internal)
\item \link{read.nm} read a csv file and try to transform to nm
\item \link{read.nmctl} read a NONMEM control stream, converting to modifiable object
\item \link{reapply} apply a function across cells of an indexed vector, giving an identically-indexed result
\item \link{rep.comment} repeat comment
\item \link{rep.flag} repeat flag
\item \link{rep.temporal} repeat temporal
\item \link{resample.csv.filename} create replicate data sets by stratified sampling with replacement, after reading csv file
\item \link{resample.data.frame} create replicate data sets by stratified sampling with replacement
\item \link{resample.filename} create replicate data sets by stratified sampling with replacement, after reading file
\item \link{resample} generic
\item \link{resolve} reinterpret relative file paths with respect to specified directory.
\item \link{rhs} extract right-hand side of wikimath
\item \link{rig.nm} generate extra rows, spaced across each subject's timecourse, for obtaining model predictions in NONMEM (EVID==2)
\item \link{rig} generic
\item \link{rinvchisq} generate inverse chi-square distribution
\item \link{riwish} generate inverse Wishart distribution
\item \link{rlog} generate a combined run log for multiple NONMEM runs
\item \link{roles} alias for as.roles
\item \link{row2tabular} convert a vector to a latex tabular row
\item \link{runCommand} issue the system call that invokes NONMEM (internal)
\item \link{runhead} determine whether each element is the start of a run
\item \link{runlog} create a zero-row runlog
\item \link{runNonmem} process a request for NONMEM invocation
\item \link{runstate} determine the status of a run
\item \link{safe.call} call a function, passing only those extra arguments that the function accepts
\item \link{safeQuote} single-quote unquoted text that contains space
\item \link{scavenge} find an expression in lines of text
\item \link{seq.temporal} generate temporal sequence
\item \link{setCwres} append conditional weighted residuals to an appropriate file
\item \link{shuffle} move columns in a data frame
\item \link{sigmacor} convert sigma covariance in NONMEM output to correlation matrix
\item \link{simblock} generate deviates of the elements of a variance-covariance matrix
\item \link{simpar} generate deviates of a set of model parameters for simulation
\item \link{snap} coerce values to nearest of candidates
\item \link{sort.keyed} sort a keyed data frame
\item \link{spaces} create a string of spaces
\item \link{splom.conditioned} scatterplot for conditioned
\item \link{splom.digest} scatterplot for digest
\item \link{splom.keyed} scatterplot for keyed
\item \link{sqrtm} support \code{compute.cwres}
\item \link{stableMerge} produce a left join, with strict error checking
\item \link{star} replace asterisk in x with y (internal)
\item \link{static.data.frame} return the static portion of a data frame
\item \link{static.keyed} return the static portion of a keyed data frame
\item \link{static} generic
\item \link{strain} reduce x to those elements that occur in options (internal)
\item \link{Summary.temporal} group generic summary for objects of class 'temporal'
\item \link{summary.hidden.data.frame} summarize hidden rows of a data frame
\item \link{summary.keyed} summarize a keyed data frame
\item \link{summary.nm} summarize an nm object
\item \link{superset} combine all NONMEM input with all output
\item \link{svnIsText} check if subversioned files are text rather than binary
\item \link{svnMarkAsNonText} tell Subversion to treat files as binary
\item \link{svnMarkAsText} tell Subversion to treat files as text
\item \link{svnMimeType} get the Subversion mime-type for files
\item \link{svnPropGet} get a Subversion property for files
\item \link{svnPropGetFile} get a Subversion property for one file
\item \link{svnPropSet} set a subversion property on files
\item \link{svnPropSetFile} set a Subversion property on a file
\item \link{svnSetMimeType} set Subversion mime type on file
\item \link{synthesis} sequentially left-join an arbitrary number of data frames, picking up novel columns (internal)
\item \link{tabfile} extract tabfile specification from a control stream
\item \link{tabular.data.frame} convert a data.frame to a latex tabular environment
\item \link{tabular} convert to a latex tabular environment
\item \link{tabularformat} generate a format string for a latex tabular environment
\item \link{tagvalue} convert a list to a string of delimited tag-value pairs
\item \link{tex2pdf} convert tex files to pdf
\item \link{text2decimal} convert mixed text to decimal
\item \link{times.keyed} column stable left join for keyed data frames
\item \link{titleCase} convert mixed case to title case, strictly by default
\item \link{Tmax} calculate the time associated with the maximum concentration for each subject
\item \link{tmax} calculate the time of the maximum concentration
\item \link{Tmin} calculate the time associated with the minimum concentration for each subject
\item \link{tos} extract thetas, omegas, and sigmas from wikimath
\item \link{totitle} convert mixed case to title case, not strictly by default
\item \link{transform.keyed} transform a keyed data frame, preserving class
\item \link{tweak.init} randomly adjust an initial parameter value 
\item \link{tweak.initList} randomly adjust a list of initial parameter values
\item \link{tweak.nmctl} randomly adjust the initial fixed effect parameters in a control stream
\item \link{tweak} randomly adjust a value
\item \link{uniKey.keyed} create a single character vector from all the key columns
\item \link{uniKey} generic
\item \link{unilog} create a zero-row unilog
\item \link{unilogcor} convert omega or sigma covariance in NONMEM output to correlation matrix
\item \link{unique.comment} find unique elements of a comment vector
\item \link{unique.temporal} find unique elements of a temporal vector
\item \link{unitDensity} calculate univariate density with maximum equal to 1
\item \link{unitHist} calculate univariate histogram architecture with maximum height equal to 1
\item \link{upper} calculate normal quantiles at upper probability of interval
\item \link{variants} locate variants of a file in distinctive subdirectories
\item \link{viewtex} view (as pdfs) files that contain fragmentary tex code (e.g. tables)
\item \link{wiki2label} convert wikimath to a label, e.g. CL/F
\item \link{wiki2latex} convert wikimath to latex
\item \link{wiki2parameter} convert wikimath to a parameter, e.g. THETA1
\item \link{wiki2plotmath} convert wikimath to plotmath
\item \link{wikiparse} parse wikimath
\item \link{wikitab} extract wikimath specification from a control stream
\item \link{win} identify Windows platforms (internal)
\item \link{wrap} wrap text in a latex environment
\item \link{write.nm} write an nm object to file
\item \link{write.nmctl} write an nmctl object to file
\item \link{xpath} extract value of an xpath exrpession from an xml file
\item \link{xtfrm.comment} produce a numeric vector that sorts in the same order as comment
\item \link{xtfrm.flag} produce a numeric vector that sorts in the same order as flag
\item \link{xtfrm.temporal} produce a numeric vector that sorts in the same order as temporal
\item \link{xyplotExt} plot the parameter search history for a NONMEM7 run
\item \link{zeroAmt.nm} AMT is zero where EVID is 1
\item \link{zeroAmt} generic
\item \link{zeroDv.nm} DV is zero where EVID is zero
\item \link{zeroDv} generic

%\item \link{rinvgamma} generate inverse gamma distribution given n, df, and sigma estimate
} 
}

