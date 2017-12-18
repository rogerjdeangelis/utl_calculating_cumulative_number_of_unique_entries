Cumulative count distinct by sequential groups problem.

  8 Solutions

   3 SAS/WPS
       1. SQL
       2. Sort
       3. hash

   5 R
       1. sqldf (same as SAS/WPS)
       2. dplyr
       3. data table
       4. zoo  rollapply
       5. sapply
INPUT
=====
                                     RULES
 SD1.HAVE total obs=17 |      cumulative count distinct
                       |
   EXP     ENTRIES     |  Cum count
                       |
   exp1     abcd       |   1
   exp1     efgh       |   2
   exp1     ijkl       |   3
   exp1     mnop       |   4    cum= because 4 distincts
                       |
   exp2     qrst       |   5
   exp2     uvwx       |   6
   exp2     abcd       |   6    do not increment because in exp1
   exp2     efgh       |   6    do not increment because in exp1
                       |
   exp3     ijkl       |   6    do not increment because in exp1
   exp3     qrst       |   6    do not increment because in exp2
   exp3     uvwx       |   6    do not increment because in exp2
   exp3     yzab       |   7
                       |
   exp4     yzab       |   7
   exp4     cdef       |   8
   exp4     mnop       |   8
   exp4     uvwx       |   8
   exp4     ghij       |   9


PROCESS (SAS/WPS)

  * interesting cartesian join
 1. proc sql;
      select
         r.exp
        ,count(distinct l.entries)
      from
         have l, have r
      where
        l.exp <= r.exp
      group
        by r.exp
    ;quit;

 2. proc sort data=sd1.have out=havSrt equals nodupkey;
     by entries;
    run;quit;

    proc sort data=havSrt out=havOdr;
     by exp ;
    run;quit;

    data want;
     set havOdr;
     by exp;
     cnt=_n_;
     if last.exp then output;
    run;quit;

 3. data wrk.wantwps(keep=exp cnt);
       retain cnt .;
       set sd1.have;
         by exp;
       if _n_ = 1 then do;
         declare hash h();
         rc = h.definekey("entries");
         rc = h.definedone();
       end;
       have_entries= h.add(KEY:entries,DATA:entries)=0;
       if have_entries then do;
          cnt=sum(cnt,have_entries);
       end;
       if last.exp then output;
       drop rc;
    run;quit;



  5 R SOLUTIONS;


  * same as SAS/WPS
  sqldf("select b.exp, count(Distinct a.entries)
    from have a join have b on a.exp <= b.exp group by b.exp");

  * YOU DECIDE ON READABILITY;
  have %>%
    mutate(ENTRIES = cumsum(!duplicated(ENTRIES))) %>%
    group_by(EXP) %>%
    summarise(ENTRIES = last(ENTRIES));

  setDT(have)[, new := cumsum(!duplicated(ENTRIES))];
  have[, .(ENTRIES = new[.N]), by = EXP];

  n <- nrow(have)
  have_sorted <- have[order(have$exp), ]
  len_uniq <- function(x) length(unique(x))
  have_cum <- transform(have_sorted,  cum = rollapplyr(entries, 1:n, len_uniq, fill = NA))
  have_cum[!duplicated(have_cum$exp, fromLast = TRUE), -2]

  temp = split(have$entries, have$exp)
  data.frame(E = names(temp),
  V = sapply(Reduce(c, temp, accumulate = TRUE), function(x) length(unique(x))))

OUTPUT
======

  EXP        Cnt
  --------------
  exp1         4
  exp2         6
  exp3         7
  exp4         9


see
https://goo.gl/GdvLvN
https://stackoverflow.com/questions/47836031/r-calculating-cumulative-number-of-unique-entries

useR
https://stackoverflow.com/users/5150629/user

frank
https://stackoverflow.com/users/1191259/frank

grothendieck (two solutions)
https://stackoverflow.com/users/516548/g-grothendieck

dww
https://stackoverflow.com/users/2761575/dww


SOAPBOX ON

  SAS has a strong SQL product and my experience is
  that it outperforms R sqldf?

  SQL often has clean code for 'set/non ordered' problems while R(without sqldf) tends to
  yeild obscure code?

  Data table tends to be faster but is often more obscure?
  Readability is important.

  IML is more readable than R and does not suffer from the 'more is less' issue.

  R packages make R a must?
  However for basic stat matrix operations R or IML function nicely.

SOAPBOX OFF


*                _              _       _
 _ __ ___   __ _| | _____    __| | __ _| |_ __ _
| '_ ` _ \ / _` | |/ / _ \  / _` |/ _` | __/ _` |
| | | | | | (_| |   <  __/ | (_| | (_| | || (_| |
|_| |_| |_|\__,_|_|\_\___|  \__,_|\__,_|\__\__,_|

;


options validvarname=upcase;
libname sd1 "d:/sd1";
data sd1.have;
 input EXP$ ENTRIES$;
cards4;
exp1 abcd
exp1 efgh
exp1 ijkl
exp1 mnop
exp2 qrst
exp2 uvwx
exp2 abcd
exp2 efgh
exp3 ijkl
exp3 qrst
exp3 uvwx
exp3 yzab
exp4 yzab
exp4 cdef
exp4 mnop
exp4 uvwx
exp4 ghij
;;;;
run;quit;

*                          _
 ___  __ _ ___   ___  __ _| |
/ __|/ _` / __| / __|/ _` | |
\__ \ (_| \__ \ \__ \ (_| | |
|___/\__,_|___/ |___/\__, |_|
                        |_|
;

%utl_submit_wps64('
libname sd1 sas7bdat "d:/sd1";
libname wrk sas7bdat "%sysfunc(pathname(work))";
proc sql;
  select
     r.exp
    ,count(distinct l.entries)
  from
     wrk.have l, wrk.have r
  where
    l.exp <= r.exp
  group
    by r.exp
;quit;
 ');

*                               _
 ___  __ _ ___   ___  ___  _ __| |_
/ __|/ _` / __| / __|/ _ \| '__| __|
\__ \ (_| \__ \ \__ \ (_) | |  | |_
|___/\__,_|___/ |___/\___/|_|   \__|

;

proc sort data=sd1.have out=havSrt equals nodupkey;
by entries;
run;quit;

proc sort data=havSrt out=havOdr;
by exp ;
run;quit;

data want;
  set havOdr;
  by exp;
  cnt=_n_;
  if last.exp then output;
run;quit;

*                     _               _
__      ___ __  ___  | |__   __ _ ___| |__
\ \ /\ / / '_ \/ __| | '_ \ / _` / __| '_ \
 \ V  V /| |_) \__ \ | | | | (_| \__ \ | | |
  \_/\_/ | .__/|___/ |_| |_|\__,_|___/_| |_|
         |_|
;
;

%utl_submit_wps64('
libname sd1 sas7bdat "d:/sd1";
libname wrk "%sysfunc(pathname(work))";
data wrk.wantwps(keep=exp cnt);
retain cnt .;
set sd1.have;
by exp;
if _n_ = 1 then do;
  declare hash h();
  rc = h.definekey("entries");
  rc = h.definedone();
end;
have_entries= h.add(KEY:entries,DATA:entries)=0;
if have_entries then do;
   cnt=sum(cnt,have_entries);
end;
if last.exp then output;
drop rc;
run;quit;
');


*____
|  _ \
| |_) |
|  _ <
|_| \_\

;

%utl_submit_wps64('
libname sd1 sas7bdat "d:/sd1";
options set=R_HOME "C:/Program Files/R/R-3.3.2";
libname wrk sas7bdat "%sysfunc(pathname(work))";
proc r;
submit;
source("C:/Program Files/R/R-3.3.2/etc/Rprofile.site", echo=T);
library(dplyr);
library(sqldf);
library(haven);
library(data.table);
have<-read_sas("d:/sd1/have.sas7bdat");
have;
sqldf("select b.exp, count(Distinct a.entries)
       from have a join have b on a.exp <= b.exp group by b.exp");
have %>%
  mutate(ENTRIES = cumsum(!duplicated(ENTRIES))) %>%
  group_by(EXP) %>%
  summarise(ENTRIES = last(ENTRIES));
setDT(have)[, new := cumsum(!duplicated(ENTRIES))];
have[, .(ENTRIES = new[.N]), by = EXP];
endsubmit;
import r=have data=sd1.want;
run;quit;
');

