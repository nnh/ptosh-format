**************************************************************************
Program Name : ptosh-format.sas
Purpose : Automatic Data Conversion of Ptosh-based Data to ADS
Author : Kato Kiroku
Date : 2019/01/25
SAS version : 9.4
**************************************************************************;

/*NOTES*/
  /*1. This program works only when file paths are as listed below.*/
      /* (Study Name)  -         input         -      ext      -  option.csv */
      /* (Study Name)  -         input         -      ext      -  sheet.csv */
      /* (Study Name)  -         input         -  rawdata  -  (rawdata).csv */
      /* (Study Name)  -  ptosh-format */
  /*2. Converted data will be exported to the "ADS" directory shown below.*/
      /* (Study Name)  -  ptosh-format  -  ads */


proc datasets library=work kill nolist; quit;

options mprint mlogic symbolgen minoperator;


*^^^^^^^^^^Find the Current Working Directory^^^^^^^^^^;

%macro FIND_WD;

    %local _fullpath _path;
    %let _fullpath=;
    %let _path=;

    %if %length(%sysfunc(getoption(sysin)))=0 %then
      %let _fullpath=%sysget(sas_execfilepath);
    %else
      %let _fullpath=%sysfunc(getoption(sysin));

    %let _path=%substr(&_fullpath., 1, %length(&_fullpath.)
                       -%length(%scan(&_fullpath., -1, '\'))
                       -%length(%scan(&_fullpath., -2, '\'))
                       -%length(%scan(&_fullpath., -3, '\')) -3);
    &_path.

%mend FIND_WD;

%let cwd=%FIND_WD;
%put &cwd.;

%let raw=&cwd.\input\rawdata;
%let ext=&cwd.\input\ext;
%let ads=&cwd.\ptosh-format\ads;
%let tmp=&cwd.\ptosh-format\tmp;


*^^^^^^^^^^Import All Raw Data within the "RAW" Directory^^^^^^^^^^;

%macro READ_CSV (dir, ext);

    %global cnt;
    %local memcnt filrf rc did name;
    %let cnt=0;

    %let filrf=mydir;
    %let rc=%sysfunc(filename(filrf, &dir.));
    %let did=%sysfunc(dopen(&filrf));
    %if &did ne 0 %then %do;
      %let memcnt=%sysfunc(dnum(&did));

      %do i=1 %to &memcnt;
 
        %let name=%qscan(%qsysfunc(dread(&did, &i)), -1, .);

        %if %qupcase(%qsysfunc(dread(&did, &i))) ne %qupcase(&name) %then %do;
          %if %superq(ext) = %superq(name) %then %do;
            %let cnt=%eval(&cnt+1);
            %put %qsysfunc(dread(&did, &i));

            *Import only when (rawdata).csv has some data;
            data _NULL_;
                if eof then call symputx('nobs', _N_-2);
                infile "&dir.\%qsysfunc(dread(&did, &i))" end=eof;
                input;
            run;

            %put &nobs;

            %If &nobs>=1 %then %do;

              *Find and remove carriage returns or line breaks in (rawdata).csv;
              data _NULL_;
                  infile "&dir.\%qsysfunc(dread(&did, &i))" recfm=n;
                  file "&tmp.\tmp&cnt..csv" recfm=n;
                    retain Flag 0;
                    input a $char1.;
                    if a='"' then
                      if Flag=0 then Flag=1;
                      else Flag=0;
                    if Flag=1 then do;
                      if a='0D'x then do;
                        goto EXIT;
                      end;
                      if a='0A'x then do;
                        goto EXIT;
                      end;
                    end;
                    put a $char1.;
                  EXIT:
              run;
  
              proc import datafile="&tmp.\tmp&cnt..csv"
                  out=tmp&cnt
                  dbms=csv replace;
                  guessingrows=999;
              run;

            %end;

          %end;
        %end;

      %end;

    %end;
    %else %put &dir. cannot be open.;
    %let rc=%sysfunc(dclose(&did));

%mend READ_CSV;

%READ_CSV (&raw., csv);


*^^^^^^^^^^^^^^^Import Options.csv and Sheet.csv from the "EXT" Directory^^^^^^^^^^^^^^^;

proc import datafile="&ext.\sheet.csv"
    out=sheet
    dbms=csv replace;
    guessingrows=999;
run;

proc import datafile="&ext.\option.csv"
    out=option
    dbms=csv replace;
    guessingrows=999;
run;


*^^^^^^^^^^^^^^^Change the Datasets Name^^^^^^^^^^^^^^^;

%macro CHANGE_DS_NAME;

    %do i=1 %to &cnt;

      data tmp&i.;
          length c $50;
          set tmp&i.;
          c=translate(VAR1, '_', '-');
          drop VAR1;
          rename c=VAR1;
      run;

      data _NULL_;
          set tmp&i.;
          by VAR1;
          if first.VAR1 then call symputx("NAME", VAR1);
      run;

      %put &NAME;

      proc datasets library=work noprint;
          change tmp&i.=&NAME.;
      run; quit;

    %end;

%mend CHANGE_DS_NAME;

%CHANGE_DS_NAME;


*^^^^^^^^^^Convert FULL-Width Characters to HALF^^^^^^^^^^;
*RSN : Unable to assign variable labels with full-width symbols like "Åi";

data sheet;
    length c2 $100;
    set sheet;
    if Sheet_alias_name=' ' then delete;
    c1=compress(kpropcase(FieldItem_label, 'full-alphabet, half-alphabet'));
    c2=compress(kpropcase(Option_name, 'full-alphabet, half-alphabet'));
    drop FieldItem_label Option_name;
    rename c1=FieldItem_label c2=Option_name;
run;

data option;
    length c1 $100;
    set option;
    c1=compress(kpropcase(Option_name, 'full-alphabet, half-alphabet'));
    c2=compress(kpropcase(Option__Value_name, 'full-alphabet, half-alphabet'));
    drop Option_name Option__Value_name;
    rename c1=Option_name c2=Option__Value_name;
run;


*^^^^^^^^^^Split the "Sheet" Dataset into Multiple Datasets^^^^^^^^^^;

***************************************TEMPORARY*************************;
data sheet;
    set sheet;
    c=CATX('_', Sheet_alias_name, FieldItem_name_tr__field______);
    drop variable;
    rename c=variable;
run;
***************************************TEMPORARY*************************;

proc sort data=sheet; by Sheet_alias_name; run;

data _NULL_;
    set sheet;
    by Sheet_alias_name;
    if first.Sheet_alias_name then do;
      i+1;
      call symputx('SUBJ'|| left(put(i, 2.)), Sheet_alias_name);
    end;
    if last.Sheet_alias_name then call symputx('TOTAL', i);
run;

%macro SPLIT;

    %global subj total;
    %do i=1 %to &TOTAL;
      data sheet_&&SUBJ&i;
        set sheet;
        where Sheet_alias_name="&&SUBJ&i";
      run;
    %end;

%mend SPLIT;

%SPLIT;


*^^^^^^^^^^Create Formats^^^^^^^^^^;

proc sort data=option; by option_name; run;

*Create Formats from the "Option" Dataset;
data option_2;
    length FMTNAME $24.;
    set option;
    by option_name;
    if first.option_name then do;
      i+1;
    end;
    if Option__Value_code_type='num' then FMTNAME=catx('_', 'fmt', i, 'f');
    else if Option__Value_code_type=' ' then FMTNAME=catx('_', '$fmt', i, 'f');
    keep FMTNAME Option_name Option__Value_name Option__Value_code Option__Value_code_type;
    rename Option__Value_code=START Option__Value_name=LABEL;
run;

proc format cntlin=option_2; run;

data option_3;
    set option_2;
    by option_name;
    if first.option_name then output;
    keep FMTNAME option_name;
run;

data option_from_sheet;
    set sheet;
    if FieldItem_field_type='ctcae' then Option_name='CTCAE';
    if Option_name=' ' then delete;
    field=cats('field', FieldItem_name_tr__field______);
run;

proc sort data=option_from_sheet; by option_name; run;

data option_f;
    merge option_from_sheet option_3;
    by Option_name;
    if FieldItem_field_type='check' then delete;
    keep Sheet_alias_name field FMTNAME FieldItem_field_type variable;
run;

proc sort data=option_f; by Sheet_alias_name; run;


*^^^^^^^^^^Find Datasets where "FieldItem_field_type"="checkbox"^^^^^^^^^^;

data chbox;
  set sheet;
  where FieldItem_field_type='checkbox';
run;

proc sort data=chbox; by option_name; run;

data chbox_2;
  merge chbox (in=a) option;
  by option_name;
  if a;
  field=cats('field_', FieldItem_name_tr__field______);
  new_var=cats(variable, '_op_', Option__Value_code);
run;

proc sql noprint;
    select cats(upcase(Sheet_alias_name))
      into : _ch_dslist_ separated by " "
    from chbox;
quit;

%put &_ch_dslist_;


*^^^^^^^^^^Data Integration Macro^^^^^^^^^^;

%macro INTEGRATE (ds);

    data sheet_&ds.;
        length field $12;
        set sheet_&ds.;
        if FieldItem_field_type=' ' then delete;
        field=cats('field', FieldItem_name_tr__field______);
    run;

    *Create Macro Variables;
    proc sql noprint;

      *"_KEEP_" holds "field" for Keep Statement;
      select cats(field)
        into : _KEEP_ separated by " "
      from sheet_&ds.
        where FieldItem_field_type NE "ctcae";
      %let _KEEP_=&_KEEP_;

      *"_LABEL_" holds "field='FieldItem_label'" for Label Statement;
      select catx("=", field, quote(trim(FieldItem_label)))
        into : _LABEL_ separated by " "
      from sheet_&ds.
        where FieldItem_field_type NE "ctcae";
      %let _LABEL_=&_LABEL_;

      *"_RENAME_" holds "field=variable" for Rename Statement;
      select catx("=", field, trim(variable))
        into : _RENAME_ separated by " "
      from sheet_&ds.
        where FieldItem_field_type NE "ctcae";
      %let _RENAME_=&_RENAME_;

      *"_NUM_" holds "field" for Numeric Conversion;
      select cats(field)
        into : _NUM_ separated by " "
      from sheet_&ds.
        where exists (select * from sheet_&ds. where FieldItem_field_type="num")
        and FieldItem_field_type="num";
      *In Case There is NOTHING Found Above, Let "_NUM_" Hold " " (NULL);
      select " "
        into : _NUM_ separated by " "
      from sheet_&ds.
        where not exists (select * from sheet_&ds. where FieldItem_field_type="num");
      %let _NUM_=&_NUM_;

      *"_DATE_" holds "field" for Date-Format;
      select cats(field)
        into : _DATE_ separated by " "
      from sheet_&ds.
        where exists (select * from sheet_&ds. where FieldItem_field_type="date")
        and FieldItem_field_type="date";
      *In Case There is NOTHING Found Above, Let "_DATE_" Hold " " (NULL);
      select " "
        into : _DATE_ separated by " "
      from sheet_&ds.
        where not exists (select * from sheet_&ds. where FieldItem_field_type="date");
      %let _DATE_=&_DATE_;

      *"_FORM_" holds "field FMTNAME" for Format Statement;
      select catx(" ", field, trim(FMTNAME) || '.')
        into : _FORM_ separated by " "
      from option_f
        where exists (select * from option_f where Sheet_alias_name="&ds.")
        and Sheet_alias_name="&ds."
        and FieldItem_field_type='num';
      *In Case There is NOTHING Found Above, Let "_FORM_" Hold " " (NULL);
      select " "
        into : _FORM_ separated by " "
      from option_f
        where not exists (select * from option_f where Sheet_alias_name="&ds.");
      %let _FORM_=&_FORM_;

      *"_CTCAE_FLD_" holds "field" for CTCAE Conversion;
      select cats(field)
        into : _CTCAE_FLD_ separated by " "
      from sheet_&ds.
        where exists (select * from sheet_&ds. where FieldItem_field_type="ctcae")
        and FieldItem_field_type="ctcae";
      *In Case There is NOTHING Found Above, Let "_CTCAE_FLD_" Hold " " (NULL);
      select " "
        into : _CTCAE_FLD_ separated by " "
      from sheet_&ds.
        where not exists (select * from sheet_&ds. where FieldItem_field_type="ctcae");
      %let _CTCAE_FLD_=&_CTCAE_FLD_;

      *"_CTCAE_KP1_" holds "variable_trm" for Keep Statement;
      select cats(variable, '_trm')
        into : _CTCAE_KP1_ separated by " "
      from sheet_&ds.
        where exists (select * from sheet_&ds. where FieldItem_field_type="ctcae")
        and FieldItem_field_type="ctcae";
      *In Case There is NOTHING Found Above, Let "_CTCAE_KP1_" Hold " " (NULL);
      select " "
        into : _CTCAE_KP1_ separated by " "
      from sheet_&ds.
        where not exists (select * from sheet_&ds. where FieldItem_field_type="ctcae");
      %let _CTCAE_KP1_=&_CTCAE_KP1_;

      *"_CTCAE_KP2_" holds "field" for Keep Statement;
      select cats(variable, '_grd')
        into : _CTCAE_KP2_ separated by " "
      from sheet_&ds.
        where exists (select * from sheet_&ds. where FieldItem_field_type="ctcae")
        and FieldItem_field_type="ctcae";
      *In Case There is NOTHING Found Above, Let "_CTCAE_KP2_" Hold " " (NULL);
      select " "
        into : _CTCAE_KP2_ separated by " "
      from sheet_&ds.
        where not exists (select * from sheet_&ds. where FieldItem_field_type="ctcae");
      %let _CTCAE_KP2_=&_CTCAE_KP2_;

      *"_CTCAE_LAB_" holds "'FieldItem_label'" for Label Statement;
      select cats(quote(trim(FieldItem_label)))
        into : _CTCAE_LAB_ separated by " "
      from sheet_&ds.
        where FieldItem_field_type="ctcae";
      *In Case There is NOTHING Found Above, Let "_CTCAE_LAB_" Hold " " (NULL);
      select " "
        into : _CTCAE_LAB_ separated by " "
      from sheet_&ds.
        where not exists (select * from sheet_&ds. where FieldItem_field_type="ctcae");
      %let _CTCAE_LAB_=&_CTCAE_LAB_;

      *"_CTCAE_VAR_" holds "variable" to Rename CTCAE variables;
      select cats(variable)
        into : _CTCAE_VAR_ separated by " "
      from sheet_&ds.
        where FieldItem_field_type="ctcae";
      *In Case There is NOTHING Found Above, Let "_CTCAE_VAR_" Hold " " (NULL);
      select " "
        into : _CTCAE_VAR_ separated by " "
      from sheet_&ds.
        where not exists (select * from sheet_&ds. where FieldItem_field_type="ctcae");
      %let _CTCAE_VAR_=&_CTCAE_VAR_;

      *"_CTCAE_FRM_" holds "variable FMTNAME" for Format Statement;
      select catx(" ", trim(variable) || '_trm', trim(FMTNAME) || '.')
        into : _CTCAE_FRM_ separated by " "
      from option_f
        where exists (select * from option_f where Sheet_alias_name="&ds.")
        and Sheet_alias_name="&ds."
        and FieldItem_field_type='ctcae';
      *In Case There is NOTHING Found Above, Let "_CTCAE_FRM_" Hold " " (NULL);
      select " "
        into : _CTCAE_FRM_ separated by " "
      from option_f
        where not exists (select * from option_f where Sheet_alias_name="&ds." and FieldItem_field_type='ctcae');
      %let _CTCAE_FRM_=&_CTCAE_FRM_;

    quit;

    *Display Macro Variables in Log Window (to check);
    %put &_KEEP_.;
    %put &_LABEL_.;
    %put &_RENAME_.;
    %put &_NUM_.;
    %put &_DATE_.;
    %put &_FORM_.;
    %put &_CTCAE_FLD_.;
    %put &_CTCAE_KP1_.;
    %put &_CTCAE_KP2_.;
    %put &_CTCAE_LAB_.;
    %put &_CTCAE_VAR_.;
    %put &_CTCAE_FRM_.;

    *Convert Character Variables Specified Above;
    %macro CONVERT_1 (varlist1, varlist2, varlist3, varlist4, varlist5);

        *to NUMERIC;
        %local i v1;
        %if &_NUM_. ne %then %do;
          %do i=1 %to %sysfunc(countw(&varlist1));
            %let v1=%scan(&varlist1, &i);
            _n_&i=input(&v1, best12.);
            drop &v1;
            rename _n_&i=&v1;
          %end;
        %end;

/*        *to DATE-FORMAT;*/
/*        %local i v2;*/
/*        %if &_DATE_. ne %then %do;*/
/*          %do i=1 %to %sysfunc(countw(&varlist2));*/
/*            %let v2=%scan(&varlist2, &i);*/
/*            format _d_&i YYMMDD10.;*/
/*            _d_&i=input(&v2, YYMMDD10.);*/
/*            drop &v2;*/
/*            rename _d_&i=&v2;*/
/*          %end;*/
/*        %end;*/

        *to CTCAE-FORMAT;
        %local i v3 v4 v5;
        %if &_CTCAE_FLD_. ne %then %do;
          %do i=1 %to %sysfunc(countw(&varlist3));
            %let v3=%scan(&varlist3, &i);
            _c1_&i=scan(&v3, 1, '-'); _c1_n_&i=input(_c1_&i, best12.);
            _c2_&i=scan(&v3, -1, '-'); _c2_n_&i=input(_c2_&i, best12.);
            drop &v3 _c1_&i _c2_&i;
            %let v4=%scan(&varlist4, &i, ' ');
            label _c1_n_&i=&v4 _c2_n_&i=&v4;
            %let v5=%scan(&varlist5, &i);
            rename _c1_n_&i=&v5._trm _c2_n_&i=&v5._grd;
          %end;
        %end;

    %mend CONVERT_1;

    data &ds._2;
        set &ds.;
        %CONVERT_1 (&_NUM_., &_DATE_., &_CTCAE_FLD_., &_CTCAE_LAB_., &_CTCAE_VAR_.);
    run;

    *Put FORMAT, KEEP, LABEL and RENAME Statement;
    data xxx_&ds.;
        set &ds._2;
        format &_FORM_. &_CTCAE_FRM_.;
        keep VAR9 &_KEEP_. &_CTCAE_KP1_. &_CTCAE_KP2_.;
        label VAR9='è«ó·ìoò^î‘çÜ' &_LABEL_.;
        rename VAR9=SUBJID &_RENAME_.;
    run;

    *Create variables if data has "checkbox" type;
    %if %upcase(&ds.) in (&_ch_dslist_.) %then %do;

        %macro CONVERT_2 (ds);
            %local n i j k;
  
              data chbox_3;
                set chbox_2;
                where Sheet_alias_name="&ds.";
                keep Sheet_alias_name Option_name Option__Value_code
                Option__Value_code_type Option__Value_name field new_var;
              run;

              proc sql noprint;
  
                select cats(variable)
                  into : _var_ separated by " "
                from chbox
                  where Sheet_alias_name="&ds.";

                select cats(new_var)
                  into : _ch_new_varlist_ separated by " "
                from chbox_3;

                select catx('=', new_var, quote(trim(Option__Value_name), "'"))
                  into : _ch_lab_ separated by " "
                from chbox_3;

              quit;

              %put &_var_;
              %put &_ch_new_varlist_;
              %put &_ch_lab_;

              data xxx_&ds.;
                length &_ch_new_varlist_. $8;
                set xxx_&ds.;
        /*        TEMP*/
        /*        c=input(&_var_, $12.);*/
        /*        drop &_var_;*/
        /*        rename c=&_var_;*/
        /*        TEMP*/
                label &_ch_lab_.;
                array AR(*) &_ch_new_varlist_.;
                do n=1 to dim(AR);
                  AR(n)='F';
                end;
              run;

              data xxx_&ds.;
                set xxx_&ds.;
                %do i=1 %to %sysfunc(countw(&_var_));
                  %let v1=%scan(&_var_, &i);
                  comma_&i=countw(&v1. , ',');
                  if &v1.=' ' then comma_&i=999;
                %end;
              run;

              %do i=1 %to %sysfunc(countw(&_var_));
    
                proc sql noprint;
                  select cats(comma_&i)
                    into : number_&i separated by " "
                  from xxx_&ds.;
                quit;

                %put &v1;
                %put &&number_&i;
  
                %do j=1 %to %sysfunc(countw(&&number_&i));
                  %let ind_&j=%scan(&&number_&i, &j);
      
                  %put &&ind_&j;

                  %if &&ind_&j NE 999 %then %do;

                    %do k=1 %to &&ind_&j;
                      data xxx_&ds.;
                        set xxx_&ds.;
                        if comma_&i=&&ind_&j then do;
                          array AR(*) &_ch_new_varlist_;
                          AR(scan(&v1, &k, ','))='T';
                        end;
                      run;
                    %end;

                  %end;
                %end;        
              %end;

              data xxx_&ds.;
                format SUBJID &_var_;
                set xxx_&ds.; 
                drop n;
                %do i=1 %to %sysfunc(countw(&_var_));
                  drop comma_&i;
                %end;
              run;

        %mend CONVERT_2;

        %CONVERT_2 (&ds.);

    %end;

    *Sort by SUBJID (number);
    proc sort data=xxx_&ds. sortseq=linguistic (numeric_collation=on); by SUBJID; run;

    *Export Datasets to the "ADS" Directory (only AE, SAE_REPORT and COMMITTEES_OPINION);
    %if %upcase(&ds.) in (AE SAE_REPORT COMMITTEES_OPINION) %then %do;
        proc export data=xxx_&ds.
            outfile="&ads.\&ds..csv"
            dbms=csv replace;
        run;
    %end;

    *Sort by SUBJID (character);
    proc sort data=xxx_&ds.; by SUBJID; run;

%mend INTEGRATE;


*Execute the MACRO (INTEGRATE);

%macro EXECUTE;

    %do i=1 %to &TOTAL;
      %INTEGRATE (&&SUBJ&i);
    %end;

%mend EXECUTE;

%EXECUTE;


*^^^^^^^^^^Export the Combined Dataset^^^^^^^^^^;

data to_combine;
    set sashelp.vtable (where=(libname='WORK'));
    if memname=:'XXX';
    if memname in ('XXX_AE' 'XXX_SAE_REPORT' 'XXX_COMMITTEES_OPINION') then delete;
run;

proc sql noprint;
    select cats(memname)
      into : _DSLIST_ separated by " "
    from to_combine;
quit;

%put &_DSLIST_;

data ptdata;
    merge &_DSLIST_;
    by subjid;
run;

proc sort data=ptdata sortseq=linguistic (numeric_collation=on); by SUBJID; run;

proc export data=ptdata
    outfile="&ads.\ptdata.csv"
    dbms=csv replace;
run;



*__________EoF__________;
