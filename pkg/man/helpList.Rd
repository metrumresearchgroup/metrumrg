\name{helpList} 
\alias{helpList} 
\docType{package} 
\title{MIfuns: Defined Functions}
\description{Exhaustive list of functions defined by \pkg{MIfuns}.}
\details{
\link[=MIfuns]{MIfuns}

\itemize{
\item \link{[.comment} subset a comment
\item \link{[.flag} subset a flag
\item \link{[.keyed} subset a keyed data frame
\item \link{[.miTemporal} subset a miTemporal object
\item \link{[.nmctl} subset a control stream object
\item \link{[[.comment} select element from a comment
\item \link{[[.flag} select element from a flag
\item \link{[[.miTemporal} select element from a miTemporal object
\item \link{[[.nmctl} select element from a control stream object
\item \link{[<-.miTemporal} assign to a miTemporal object
\item \link{\%nested.in\%} test whether a is nested in b
\item \link{\%nests\%} test whether a nests b
\item \link{accept} document the acceptance of an R installation
\item \link{acceptance} display the acceptance documentation for an R installation
\item \link{acr} calculate accumulation ratio
\item \link{after} test whether elements occur after some condition is \code{TRUE}
\item \link{align.decimal} align numeric column in latex table on the decimal
\item \link{aggregate.keyed} aggregate a keyed data frame
\item \link{and.keyed} produce inner join of two keyed data frames
\item \link{as.character.comment} convert comment to character
\item \link{as.character.flag} convert flag to character
\item \link{as.character.miTemporal} convert miTemporal to character
\item \link{as.character.nmctl} convert nmctl to character
\item \link{as.chartime} convert to time (generic, internal)
\item \link{as.chartime.numeric} convert numeric to time
\item \link{as.comment} convert to comment
\item \link{as.comment.comment} convert comment to comment
\item \link{as.comment.default} convert default to comment
\item \link{as.csv.filename} convert to csv.filename
\item \link{as.csv.filename.character} convert character to csv.filename 
\item \link{as.data.frame.block} convert a block of lines to data frame
\item \link{as.data.frame.comment} convert comment to data frame
\item \link{as.data.frame.flag} convert flag to data frame
\item \link{as.file.runlog} convert to convert runlog to file
\item \link{as.filename} convert to filename
\item \link{as.filename.character} convert character to filename
\item \link{as.flag} convert to flag
\item \link{as.flag.default} convert default to flag
\item \link{as.flag.flag} convert flag to flag
\item \link{as.halfmatrix} generic
\item \link{as.halfmatrix.default} convert a vector to halfmatrix
\item \link{as.halfmatrix.halfmatrix} preserve halfmatrix
\item \link{as.keyed} convert to keyed
\item \link{as.keyed.data.frame} convert data frame to keyed
\item \link{as.list.nmctl} convert nmctl to list
\item \link{as.matrix.halfmatrix} convert a vector to a symmetric matrix
\item \link{as.miDate} convert to miDate
\item \link{as.miDate.character} convert character to miDate
\item \link{as.miDate.Date} convert Date to miDate
\item \link{as.miDate.dates} convert dates (chron) to miDate
\item \link{as.miDate.miDate} convert miDate to miDate
\item \link{as.miDate.numeric} convert numeric to miDate
\item \link{as.miDateTime} convert to miDateTime
\item \link{as.miDateTime.character} convert character to miDateTime
\item \link{as.miDateTime.chron} convert chron to miDateTime
\item \link{as.miDateTime.miDate} convert miDate to miDateTime
\item \link{as.miDateTime.miDateTime} convert miDateTime to miDateTime
\item \link{as.miDateTime.numeric} convert numeric to miDateTime
\item \link{as.miDateTime.POSIXct} convert POSIXct to miDateTime
\item \link{as.miDateTime.POSIXlt} convert POSIXlt to miDateTime
\item \link{as.miTime} convert to miTime
\item \link{as.miTime.character} convert character to miTime
\item \link{as.miTime.miTime} convert miTime to miTime
\item \link{as.miTime.numeric} convert numeric to miTime
\item \link{as.miTime.times} convert times (chron) to miTime
\item \link{as.moot} convert to moot
\item \link{as.nm} convert to nm (NMTRAN-ready NONMEM data set)
\item \link{as.nm.data.frame} convert data frame to nm
\item \link{as.nmctl} convert to nmctl
\item \link{as.nmctl.character} convert character to nmctl
\item \link{as.numeric.chartime} convert chartime to numeric
\item \link{as.pxml.ext} convert a param file to XML
\item \link{as.rigged} convert to rigged
\item \link{as.runlog.file} convert file to runlog
\item \link{as.runlog.unilog} convert unilog to runlog
\item \link{as.unilog.lst} convert nonmem output (.lst) to unilog
\item \link{as.unilog.pxml} convert nonmem output (.ext) to unilog
\item \link{as.unilog.run} convert run to unilog
\item \link{as.unilog.runlog} convert runlog to unilog
\item \link{as.xml} convert object to XML
\item \link{as.xml.character} convert character to XML
\item \link{as.xml.data.frame} convert data.frame to XML
\item \link{as.xml.default} convert arbitrary object to XML
\item \link{at} test whether elements occur where some condition is \code{TRUE}
\item \link{attribute} encode an attribute for an XML open tag
\item \link{auc} calculate area under the curve
\item \link{AUC} calculate the area under the curve for each subject over the time interval for dv using the trapezoidal rule
\item \link{aug} add named elements to an object
\item \link{autolog.pl} prepare command lines for autolog.pl.
\item \link{backtrans} backtransform cols in x, returning x
\item \link{badAmt} generic
\item \link{badAmt.nm} AMT is NA where EVID is 1
\item \link{badDv} generic
\item \link{badDv.nm} DV is NA where EVID is 0
\item \link{badII} generic
\item \link{badII.nm} II is greater than 0 where ADDL is NA or 0 (unless SS > 0)
\item \link{bakfor} impute NAs using last observation carried forward after next observation carried backward
\item \link{before} test whether elements occur before some condition is \code{TRUE}
\item \link{bin} calculate bin limits for x and place each value in a bin, returning the number of values in each bin
\item \link{bmi} calculate body mass index
\item \link{bracket} create an XML tag
\item \link{breaks} calculate breaks between grouped elements
\item \link{bsa} calculate body surface area
\item \link{c.comment} combine comment
\item \link{c.flag} combine flag
\item \link{c.miTemporal} combine miTemporal
\item \link{cavg} calculate average concentration
\item \link{check.subjects} check for missing data, numeric data, ranges, etc.
\item \link{clear} drop regular patterns from a character vector
\item \link{closers} set closing positions for wikmath nests
\item \link{CLNR} delete files and directories and log what was deleted to a text file in the project
\item \link{cmax} calculate maximum steady-state concentration
\item \link{cmin} calculate minimum steady-state concentration
\item \link{colname<-} change identified column names
\item \link{command} generate a latex command
\item \link{compileflag} calculate compiler flag for qsub
\item \link{compiler} extract compiler specification from configuration file
\item \link{compute.cwres} compute conditional weighted residuals
\item \link{config} calculate path to configuration file
\item \link{constant} generic
\item \link{constant.default} test whether values of x are contant within an index
\item \link{contains} test a character vector for occurences of a pattern
\item \link{covariatePlots} create diagnostic plots for covariates
\item \link{crcl} calculate creatinine clearance
\item \link{crosses} test whether a crosses b
\item \link{css} calculate steady-state concentration
\item \link{ctl2xml} isolate xml comments from a control stream
\item \link{cwresPlots} create conditional weighted residual diagnostic plots
\item \link{dataFormat} combine NONMEM predictions, parameter estimates, and covariates into a standard format
\item \link{dataSynthesis} build a standard plotting data set for a given NONR run
\item \link{deranged} generic
\item \link{deranged.data.frame} instantiate records implied by range data
\item \link{deranged.keyed} instantiate records implied by range data
\item \link{diagnosticPlots} create standard diagnostic plots
\item \link{distance} calculate element positions relative to the \eqn{n^{th}}{n^th} \code{TRUE} element positions (usually, within levels of an index)
\item \link{dupKeys} generic
\item \link{dupKeys.default} give a logical index to duplicated rows of a keyed data frame, AND the rows of which they are duplicates
\item \link{dupKeys.nm} default behavior, after stripping comments
\item \link{electronicAppendix} create an FDA-style electronic appendix from a subversioned directory
\item \link{episcript} run a script in an environment that defines the passed arguments
\item \link{explicitPath} add separators to control stream paths that have none
\item \link{extfile} extract arbitrary file specification from a control stream
\item \link{extractPath} isolate file path from surrounding text (MSF, TAB files).
\item \link{f} alias for as.flag
\item \link{falseAmt} generic
\item \link{falseAmt.nm} AMT defined where EVID is not 1
\item \link{falseDv} generic
\item \link{falseDv.nm} DV is defined where EVID is not 0
\item \link{filename} generate standard file name (internal)
\item \link{first} repeat, for each element, the first element where a condition is \code{TRUE} (usually, within levels of an index)
\item \link{fixedwidth} convert to fixed-width format
\item \link{fixedwidth.data.frame} convert a data.frame to fixed-width format
\item \link{fixFile} rewrite run name in control stream file specifications
\item \link{fixProblem} rewrite run name in control stream PROBLEM statement
\item \link{forbak} impute NAs using last observation carried forward and then next observation carried backward
\item \link{format.comment} format comment 
\item \link{format.flag} format flag
\item \link{format.miDate} format miDate
\item \link{format.miDateTime} format miDateTime
\item \link{format.miTime} format miTime
\item \link{format.nmctl} format nmctl
\item \link{ftable2data.frame} convert ftable to data.frame as displayed
\item \link{getCovs} return non-commented non-duplicated rows of a NONMEM data set (internal)
\item \link{getCwres} return the conditional weighted residuals pertinent to a given run (internal, expects cwtab1.deriv)
\item \link{getdname} extract the path and file from the DATA block in the control stream associated with filename (internal)
\item \link{getPars} return the non-duplicated (ID) rows corresponding to the specified filename (internal)
\item \link{getTabs} return the observation rows (EVID==0) corresponding to the specified filename (internal)
\item \link{glue} paste with no separator
\item \link{groupnames} construct a single vector of group names from possibly multiple indices to the plotting data set (internal)
\item \link{half} generic
\item \link{half.matrix} vectorize the lower triangular portion of a symmetric matrix
\item \link{hash} prepend a character to each line of output
\item \link{hide} generic
\item \link{hide.data.frame} comment out the specified rows of a data frame, appending a flag that suggests the reason
\item \link{hidden} generic
\item \link{hidden.data.frame} show commented rows of a data frame
\item \link{ibw} calculate ideal body weight
\item \link{iterations} retrieve iteration statistics from NONMEM output file
\item \link{ind.cwres} compute conditional weighted residuals (internal)
\item \link{inner} generic
\item \link{inner.data.frame} reduce columns to inner quantiles by imputing NA
\item \link{is.alpha} test whether text is letters-only
\item \link{is.cwres.readable.file} compute conditional weighted residuals (internal)
\item \link{is.latex.token} test whether text is valid latex command or environment
\item \link{is.one.nonalpha} test whether text is singular non-letter
\item \link{is.square} generic
\item \link{is.square.matrix} test whether a matrix is square
\item \link{isSubversioned} check if files are subversioned
\item \link{isSubversionedFile} check if one file is subversioned
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
\item \link{lhs} extract left-hand side of wikimath
\item \link{locf} impute NA using last observation carried forward
\item \link{lookup} translate among parameter attributes (vector)
\item \link{lookup.one} translate among parameter attributes (scalar)
\item \link{ltable} convert to a latex table
\item \link{ltable.data.frame} convert a data.frame to a latex table
\item \link{map} map one set of values to another
\item \link{maxChar} return the number of printed characters for the widest element of x
\item \link{merge.keyed} merge keyed data frames, preserving attributes
\item \link{merge.nm} merge nm objects
\item \link{metaSub} generic
\item \link{metaSub.character} systematically substitute elements in a character vector
\item \link{metaSub.filename} systematically substitute elements in a text file
\item \link{minus.keyed} drop rows in x that have matching rows in y
\item \link{minus.moot} drop moot rows
\item \link{moot} generic
\item \link{moot.nm} identify moot rows (rows not influencing NONMEM estimates)
\item \link{msffile} extract msffile specification from a control stream
\item \link{naInContext} display rows of data with missing values, as well as other rows with the same key
\item \link{naKeys} generic
\item \link{naKeys.default} detect rows for which one or more key fields is NA
\item \link{naKeys.nm} default behavior, after stripping comments
\item \link{name<-} change identified object names
\item \link{nest} nest an XML fragment in a parent element
\item \link{nix} identify Unix-like platforms (internal)
\item \link{nm} give a zero-row data frame with suitable columns and column classes; i.e. a template for NMTRAN data sets
\item \link{nm.pl} prepare command lines for nm.pl
\item \link{nmPlots} a list of functions that plot nm objects
\item \link{nmVersion} extract NONMEM version specification from configuration file
\item \link{nocb} impute NA with next observation carried backward
\item \link{NONR} run NONMEM and create diagnostic plots
\item \link{NONR72} run NONMEM 7.2.0 or later
\item \link{nospace} drop spaces from text
\item \link{noPk} generic
\item \link{noPk.nm} rows where EVID is never 0 within SUBJ
\item \link{noUnits} drop units from wikimath
\item \link{nth} repeat, for each element, the \eqn{n^{th}}{n^th} element where a condition is \code{TRUE}  (usually, within levels of an index)
\item \link{nxt} calculate the next element for each element of x
\item \link{offdiag} generic
\item \link{offdiag.halfmatrix} select just the off-diagonal elements of a halfmatrix
\item \link{omegacor} convert omega covariance in NONMEM output to correlation matrix
\item \link{only} repeat, for each element, the only element where a condition is \code{TRUE} (usually, within levels of an index)
\item \link{Ops.keyed} use concise syntax to join data frames
\item \link{ord} generic
\item \link{ord.halfmatrix} compute the order of a half matrix
\item \link{ord.matrix} compute the order of a symmetric matrix
\item \link{packageCheck} load a package and run package-level examples
\item \link{panel.bar} for each level, plot vertical (or horizontal) bars corresponding to x (y).
\item \link{panel.covplot} plot distributions with respect to reference values; i.e. combine panel.cuts, panel.densitystrip, and panel.ref
\item \link{panel.cuts} for each level, plot percent observations in each vertical category defined by cuts
\item \link{panel.densitystrip} for each level, plot a filled polygon representing smoothed density of the distribution
\item \link{panel.hist} for each level, plot a histogram
\item \link{panel.ref} plot a reference region in a stripplot
\item \link{panel.stratify} handle strips (levels) of data one at a time
\item \link{params} list documented model parameters
\item \link{parens} wrap text in parentheses
\item \link{parfile} extract parfile specification from a control stream
\item \link{partab} construct a model parameter table
\item \link{plot.nm} generate standard plots for an nm object using \code{\link{nmPlots}}
\item \link{plotfilename} make a name for a diagnostic plot file
\item \link{PLOTR} create diagnostic plots for NONMEM runs
\item \link{plus.keyed} produce and outer join of keyed data frames
\item \link{plus.rigged} merge an nm object with a rigged data frame
\item \link{posmat} coerce a matrix to be positive definite
\item \link{predoseDv} generic
\item \link{predoseDv.nm} DV is defined before the first record within SUBJ where EVID is 1
\item \link{prev} calculate the previous element for each element of x
\item \link{print.comment} print comment
\item \link{print.flag} print flag
\item \link{print.halfmatrix} print halfmatrix
\item \link{print.keyed.summary} print keyed summary
\item \link{print.miTemporal} print miTemporal
\item \link{print.nm.summary} print nm summary
\item \link{print.nmctl} print nmctl
\item \link{purge.dir} purge a directory (internal)
\item \link{purge.files} purge files (internal)
\item \link{qsub} prepare commandlines for qsub
\item \link{read.cwres.data} compute conditional weighted residuals (internal)
\item \link{read.nm} read a csv file and try to transform to nm
\item \link{read.nmctl} read a NONMEM control stream, converting to modifiable object
\item \link{reapply} apply a function across cells of an indexed vector, giving an identically-indexed result
\item \link{rep.comment} repeat comment
\item \link{rep.flag} repeat flag
\item \link{rep.miTemporal} repeat miTemporal
\item \link{resample} generic
\item \link{resample.csv.filename} create replicate data sets by stratified sampling with replacement, after reading csv file
\item \link{resample.data.frame} create replicate data sets by stratified sampling with replacement
\item \link{resample.filename} create replicate data sets by stratified sampling with replacement, after reading file
\item \link{resolve} reinterpret relative file paths with respect to specified directory.
\item \link{rhs} extract right-hand side of wikimath
\item \link{rig} generic
\item \link{rig.nm} generate extra rows, spaced across each subject's timecourse, for obtaining model predictions in NONMEM (EVID==2)
\item \link{rinvchisq} generate inverse chi-square distribution
%\item \link{rinvgamma} generate inverse gamma distribution given n, df, and sigma estimate
\item \link{riwish} generate inverse Wishart distribution
\item \link{rlog} generate a combined run log for multiple NONMEM runs
\item \link{row2tabular} convert a vector to a latex tabular row
\item \link{runCommand} issue the system call that invokes NONMEM (internal)
\item \link{runhead} determine whether each element is the start of a run
\item \link{runlog} create a zero-row runlog
\item \link{runNonmem} process a request for NONMEM invocation
\item \link{runstate} determine the status of a run
\item \link{safe.call} call a function, passing only those extra arguments that the function accepts
\item \link{safeQuote} single-quote unquoted text that contains space
\item \link{scavenge} find an expression in lines of text
\item \link{seq.miTemporal} generate miTemporal sequence
\item \link{setCwres} append conditional weighted residuals to an appropriate file
\item \link{shuffle} move columns in a data frame
\item \link{sigmacor} convert sigma covariance in NONMEM output to correlation matrix
\item \link{simpar} generate deviates of a set of model parameters for simulation
\item \link{simblock} generate deviates of the elements of a variance-covariance matrix
\item \link{snap} coerce values to nearest of candidates
\item \link{sort.keyed} sort a keyed data frame
\item \link{spaces} create a string of spaces
\item \link{sqrtm} support \code{compute.cwres}
\item \link{stableMerge} produce a left join, with strict error checking
\item \link{star} replace asterisk in x with y (internal)
\item \link{strain} reduce x to those elements that occur in options (internal)
\item \link{summary.keyed} summarize a keyed data frame
\item \link{summary.hidden.data.frame} summarize hidden rows of a data frame
\item \link{summary.nm} summarize an nm object
\item \link{svnIsText} check if subversioned files are text rather than binary
\item \link{svnMarkAsText} tell Subversion to treat files as text
\item \link{svnMarkAsNonText} tell Subversion to treat files as binary
\item \link{svnMimeType} get the Subversion mime-type for files
\item \link{svnPropGet} get a Subversion property for files
\item \link{svnPropGetFile} get a Subversion property for one file
\item \link{svnPropSet} set a subversion property on files
\item \link{svnPropSetFile} set a Subversion property on a file
\item \link{synthesis} sequentially left-join an arbitrary number of data frames, picking up novel columns (internal)
\item \link{tabfile} extract tabfile specification from a control stream
\item \link{tabular} convert to a latex tabular environment
\item \link{tabular.data.frame} convert a data.frame to a latex tabular environment
\item \link{tabularformat} generate a format string for a latex tabular environment
\item \link{tagvalue} convert a list to a string of delimited tag-value pairs
\item \link{text2decimal} convert mixed text to decimal
\item \link{tmax} calculate the time of the maximum concentration
\item \link{Tmax} calculate the time associated with the maximum concentration for each subject
\item \link{Tmin} calculate the time associated with the minimum concentration for each subject
\item \link{tos} extract thetas, omegas, and sigmas from wikimath
\item \link{transform.keyed} transform a keyed data frame, preserving class
\item \link{uniKey} generic
\item \link{uniKey.keyed} create a single character vector from all the key columns
\item \link{unilog} create a zero-row unilog
\item \link{unilogcor} convert omega or sigma covariance in NONMEM output to correlation matrix
\item \link{unique.miTemporal} find unique elements of a temporal vector
\item \link{unitDensity} calculate univariate density with maximum equal to 1
\item \link{unitHist} calculate univariate histogram architecture with maximum height equal to 1
\item \link{nmVersion} extract NONMEM version specification from configuration file
\item \link{variants} locate variants of a file in distinctive subdirectories
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
\item \link{xtfrm.comment} produce a numeric vector that sorts in the same order as comment
\item \link{xtfrm.flag} produce a numeric vector that sorts in the same order as flag
\item \link{xtfrm.miTemporal} produce a numeric vector that sorts in the same order as miTemporal
\item \link{xyplot.ext} plot the parameter search history for a NONMEM7 run
\item \link{zeroAmt} generic
\item \link{zeroAmt.nm} AMT is zero where EVID is 1
\item \link{zeroDv} generic
\item \link{zeroDv.nm} DV is zero where EVID is zero
} 
} 

