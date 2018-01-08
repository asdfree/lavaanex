library(lodown)
library(lavaan.survey)	
options( survey.lonely.psu = "adjust" )

# retrieve a listing of all available extracts for the european social survey
ess_cat <- get_catalog( "ess" , output_dir = file.path( path.expand( "~" ) , "ESS" ) )

# limit the catalog to only wave #4 for germany and spain
ess_cat <- subset( ess_cat , wave == 4 & grepl( "c=DE|c=ES" , full_url ) )

# download the ess microdata

# load Germany's round four main data file..
ess4.de <- readRDS( file.path( path.expand( "~" ) , "ESS" , "2008/ESS4DE.rds" ) )

# load Germany's round four sample design data file (sddf)..
ess4.de.sddf <- readRDS( file.path( path.expand( "~" ) , "ESS" , "2008/ESS4_DE_SDDF.rds" ) )
ess4.de.sddf$stratify <- 
	factor( gsub( "(\\d+)-.+" , "\\1" , as.character( ess4.de.sddf$stratify ) ) )

levels(ess4.de.sddf$stratify) <- c("West Germany", "East Germany")
stopifnot(tapply(ess4.de.sddf$psu, 
                 ess4.de.sddf$stratify, 
                 function(x) length(unique(x))) == c(109, 59))
ess4.de.m <- merge( ess4.de , ess4.de.sddf)

stopifnot( 
	nrow( ess4.de ) == nrow( ess4.de.m ) & 
	nrow( ess4.de.sddf ) == nrow( ess4.de.m ) 
)
ess4.de.design <- 
	svydesign(
		ids = ~psu ,
		strata = ~stratify ,
		probs = ~prob ,
		data = ess4.de.m
	)
model.cfa <-    
	"range =~ gvjbevn + gvhlthc + gvslvol + gvslvue + gvcldcr + gvpdlwk
	 goals =~ sbprvpv  +  sbeqsoc  +  sbcwkfm"
fit.cfa.ml <- 
	lavaan(
		model.cfa , 
		data = ess4.de.m , 
		estimator = "MLM" , 
		int.ov.free = TRUE ,
		auto.var = TRUE , 
		auto.fix.first = TRUE , 
		auto.cov.lv.x = TRUE
	)
fit.cfa.ml
fit.cfa.surv <- 
	lavaan.survey(
		fit.cfa.ml , 
		survey.design = ess4.de.design
	)
fit.cfa.surv
summary( fit.cfa.surv , standardized = TRUE )
# load Spain's round four main data file..
ess4.es <- readRDS( file.path( path.expand( "~" ) , "ESS" , "2008/ESS4ES.rds" ) )

# load Spain's round four sample design data file (sddf)..
ess4.es.sddf <- readRDS( file.path( path.expand( "~" ) , "ESS" , "2008/ESS4_ES_SDDF.rds" ) )
ess4.es.m <- merge( ess4.es , ess4.es.sddf)

stopifnot( 
	nrow( ess4.es ) == nrow( ess4.es.m ) & 
	nrow( ess4.es.sddf ) == nrow( ess4.es.m ) 
)
ess4.de.m$psu <- paste( "de" , ess4.de.m$psu , sep="-" )
ess4.es.m$psu <- paste( "es" , ess4.es.m$psu , sep="-" )
ess4.m <- rbind( ess4.de.m , ess4.es.m )

ess4.design <- 
	svydesign(
		ids = ~psu,
		strata = ~stratify ,
		probs = ~prob ,
		data = ess4.m
	)
free.values.model.syntax <- " 
  Universalism =~ ipeqopt + ipudrst + impenv
  Benevolence  =~ iphlppl + iplylfr

  Tradition    =~ ipmodst + imptrad
  Conformity   =~ ipfrule + ipbhprp 
  Security     =~ impsafe + ipstrgv
"
free.values.fit <- 
	lavaan(
		free.values.model.syntax , 
		data = ess4.m , 
		auto.cov.lv.x = TRUE , 
		auto.fix.first = TRUE , 
		auto.var = TRUE ,
		int.ov.free = TRUE , 
		estimator = "MLM" ,
		group = "cntry"
	)

summary( free.values.fit , standardized = TRUE )
free.values.fit.eq <- 
	lavaan(
		free.values.model.syntax , 
		data = ess4.m , 
		auto.cov.lv.x = TRUE , 
		auto.fix.first = TRUE , 
		auto.var = TRUE ,
		int.ov.free = TRUE , 
		estimator = "MLM" ,
		group = "cntry" , 
		group.equal = "loadings"
	)

summary( free.values.fit.eq , standardized = TRUE )
lavTestLRT( free.values.fit , free.values.fit.eq , SB.classic = TRUE )
free.values.fit.surv <- lavaan.survey( free.values.fit , ess4.design )
free.values.fit
free.values.fit.surv
free.values.fit.eq.surv <- lavaan.survey( free.values.fit.eq , ess4.design )
free.values.fit.eq
free.values.fit.eq.surv
lavTestLRT(free.values.fit.surv, free.values.fit.eq.surv, SB.classic = TRUE)
reg.syntax <- "
  SelfTranscendence =~ ipeqopt + ipudrst + impenv + iphlppl + iplylfr
  Conservation =~ ipmodst + imptrad + ipfrule + ipbhprp + impsafe + ipstrgv

  ALLOW =~ imdfetn + impcntr

  ALLOW ~ SelfTranscendence + Conservation
"

reg.vals.fit <- 
	lavaan(
		reg.syntax , 
		data = ess4.m , 
		group = "cntry" ,
		estimator = "MLM" ,
		auto.cov.lv.x = TRUE , 
		auto.fix.first = TRUE , 
		auto.var = TRUE , 
		int.ov.free = TRUE
	)

reg.vals.fit.eq <- 
	lavaan( 
		reg.syntax , 
		data = ess4.m , 
		group = "cntry" , 
		group.equal = "regressions" ,
		estimator = "MLM" ,
		auto.cov.lv.x = TRUE , 
		auto.fix.first = TRUE , 
		auto.var = TRUE , 
		int.ov.free = TRUE
	)

	
summary( reg.vals.fit.eq , standardize = TRUE )
lavTestLRT( reg.vals.fit , reg.vals.fit.eq , SB.classic = TRUE)
reg.vals.fit.surv <- lavaan.survey( reg.vals.fit , ess4.design )
reg.vals.fit.eq.surv <- lavaan.survey( reg.vals.fit.eq , ess4.design )

lavTestLRT(reg.vals.fit.surv, reg.vals.fit.eq.surv, SB.classic = TRUE)
