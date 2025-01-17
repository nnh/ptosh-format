**************************************************************************
Program : ptosh-format.sas
Purpose : Automatic Data Conversion of Ptosh-based Data to ADS
Author : Kato Kiroku, Mariko Ohtsuka
Published : 2024-10-23
Version : 1.0.1
**************************************************************************;

/*NOTES*/
  /*1. This program works only when file paths are as listed below.*/
      /* (Trial Folder)  -        input        -      ext      -  options.csv */
      /* (Trial Folder)  -        input        -      ext      -  sheets.csv */
      /* (Trial Folder)  -        input        -  rawdata  -  (rawdata).csv */
      /* (Trial Folder)  -  ptosh-format  */
  /*2. Converted data will be exported to the "ADS" directory shown below.*/
      /* (Trial Folder)  -  ptosh-format  -  ads */


proc datasets library=work kill nolist; quit;
dm log 'clear';

options mprint mlogic symbolgen minoperator;
%macro delete_var_if_exists(varname);
    %if %symglobl(&varname) %then %do;
        %symdel &varname;
        %put &varname was deleted.;
    %end;
    %else %do;
        %put &varname does not exist.;
    %end;
%mend;
%macro delete_all_global_vars();
    proc sql noprint;
        select name
        into :varlist separated by ' '
        from dictionary.macros
        where scope = 'GLOBAL' 
          and name not in ('SYSENV', 'SYSSCP', 'SYSSCPL')
          and substr(name, 1, 4) ne 'SYS_';  /* 'SYS_'で始まる自動マクロ変数を除外 */
    quit;
	%if %symexist(VARLIST) = 0 %then %do;
        %return;
    %end;

    %let count = %sysfunc(countw(&varlist));
    %do i = 1 %to &count;
        %let varname = %scan(&varlist, &i);
        %delete_var_if_exists(&varname);
    %end;
%mend;
%delete_all_global_vars;

*------------------------------Current Working Directories------------------------------;

*Find the current working directory;
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
%put &cwd;

%let create_log_dir=%sysfunc(dcreate(log, &cwd.\ptosh-format));

proc printto log="&cwd.\ptosh-format\log\ptosh-format.log" new; run;
%put %sysfunc(date(), yymmdd10.);

%let raw=&cwd.\input\rawdata;
%let ext=&cwd.\input\ext;
%let create_ads_dir=%sysfunc(dcreate(ads, &cwd.\ptosh-format));
%let ads=&cwd.\ptosh-format\ads;
%macro DELETE_FILES_IN_FOLDER(folder_path);
    filename myfolder "&folder_path.";

    data _null_;
        length fname $256;
        rc = filename('dir', "&folder_path.");
        did = dopen('dir');

        if did > 0 then do;
            do i = 1 to dnum(did);
                fname = dread(did, i);
                rc = filename('file', cats("&folder_path.\", fname));
                if fexist('file') then do;
                    rc = fdelete('file');
                    if rc = 0 then
                        put "Deleted file: " fname;
                    else
                        put "ERROR: Failed to delete file: " fname;
                end;
            end;
            rc = dclose(did);
        end;
        else put "ERROR: Unable to open directory &folder_path.";

        rc = filename('dir');
    run;

%mend DELETE_FILES_IN_FOLDER;
%DELETE_FILES_IN_FOLDER(&ads.);

%let create_temp_dir=%sysfunc(dcreate(temp, &cwd.\ptosh-format));
%let temp=&cwd.\ptosh-format\temp;

libname libraw "&cwd.\input\rawdata" access=readonly;
libname libext "&cwd.\input\ext" access=readonly;
libname libads "&cwd.\ptosh-format\ads";
libname library "&cwd.\ptosh-format\ads";


*------------------------------Sheets.csv and Options.csv------------------------------;

*Import Sheets.csv and Options.csv from the "EXT" Directory;
proc import datafile="&ext.\sheets.csv"
    out=sheet
    dbms=csv replace;
    guessingrows=MAX;
run;
proc import datafile="&ext.\options.csv"
    out=option
    dbms=csv replace;
    guessingrows=MAX;
run;

*Adjust the "Option_name" variable in both datasets to be the same length;
data sheet;
    length op $100 c $100 field $12;
    set sheet;
    *Delete unnecessary variables (where "variable" is unnamed);
    if variable=' ' then delete;
	Sheet_alias_name= translate(Sheet_alias_name, '_', '-');
    *Delete blanks;
    field=cats('field', FieldItem_name_tr__field______);
    c=compress(variable); drop variable; rename c=variable;
    op=Option_name; drop Option_name; rename op=Option_name;
run;
data option;
    length op $100;
    set option;
    op=Option_name; drop Option_name; rename op=Option_name;
run;


*------------------------------"Variable" in the Sheet Dataset------------------------------;

*Find out if there are duplicate variables;
%macro CHECK4ERRORS;

    %let cancel=;

	proc sql noprint;
		create table temp_sheet_check4errors as
		select variable,
		case 
			when sheet_category ne "ae_report" & 
				 sheet_category ne "committees_opinion" & 
                 sheet_category ne "multiple" then "others"
			else sheet_category
		end as sheet_category
		from sheet
		order by sheet_category, variable; 
	quit;
    proc freq data=temp_sheet_check4errors noprint;
        tables variable / out=sheet_vcount;
        by sheet_category;
    run;

    *If duplicate variables are found, let "WarMessage" hold the variable names;
    proc sql noprint;
        select quote(strip(variable))
          into : WarMessage separated by " "
        from sheet_vcount
          where count>1;
        select " "
          into : WarMessage separated by " "
        from sheet_vcount
          where not exists (select * from sheet_vcount where count>1);
        %let WarMessage=&WarMessage;
    quit;

    *Only when duplicate variables are found;
    %if &WarMessage NE %then %do;
      data variable_duplicated;
          set sheet;
          where variable in (&WarMessage);
      run;
      *Export a dataset about the duplicate variables;
      proc export data=variable_duplicated
          outfile="&ads.\variable_duplicated.csv"
          dbms=csv replace;
      run;
      *Print warning to log;
      %put WARNING: 変数名 &WarMessage. は重複しています。変数名を変更してください。;
	  proc printto; run;
      *Stop the current step and any further procedures;
      %abort cancel;
    %end;

%mend CHECK4ERRORS;

%CHECK4ERRORS;


*------------------------------Raw Data------------------------------;

*Import all raw data within the "RAW" directory;
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
                  file "&temp.\temp&cnt..csv" recfm=n;
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

              proc import datafile="&temp.\temp&cnt..csv"
                  out=temp&cnt
                  dbms=csv replace;
                  guessingrows=MAX;
              run;

              *If there is an allocation data in the "RAW" directory, rename it to "group";
              %let csvfile_&cnt=%qsysfunc(dread(&did, &i));
              data filename_&cnt;
                  length title $4;
                  title=substr(compress(scan("&&csvfile_&cnt", 2, '_')), 1, 1);
                  title_2=scan("&&csvfile_&cnt", 1, '_');
                  output;
                  call symputx("csvname_&cnt", title, "G");
                  call symputx("trialname_&cnt", title_2, "G");
              run;
              %if %quote(%upcase(&&trialname_&cnt))=ALLOCATION %then %do;
                proc datasets library=work noprint;
                    change temp&cnt=group;
                run; quit;
              %end;
              %if &&csvname_&cnt in (0 1 2 3 4 5 6 7 8 9) %then %do;
                proc datasets library=work noprint;
                    change temp&cnt=_NOT_YET_DEFINED_;
                run; quit;
              %end;

            %end;

          %end;
        %end;

      %end;

    %end;
    %else %put &dir. cannot be open.;
    %let rc=%sysfunc(dclose(&did));

%mend READ_CSV;

%READ_CSV (&raw, csv);


*Change the names of datasets;
%macro CHANGE_DS_NAME;

    %do i=1 %to &cnt;
      %if %sysfunc(exist(temp&i)) %then %do;

        data temp&i;
            length c $50;
            set temp&i;
            *Convert '-' to '_' since it is impossible to use the '-' symbol as a variable name;
            c=translate(VAR1, '_', '-');
            drop VAR1;
            rename c=VAR1;
        run;
        proc sort data=temp&i; by VAR1; run;
        data _NULL_;
            set temp&i end=END;
            by VAR1;
            if _N_=1 then call symputx("NAME", VAR1);
            if END then call symputx("NAME4C", VAR1);
        run;
        %put &NAME;
        %put &NAME4C;

        *When "VAR1" has a sheet name, rename the dataset;
        %if &NAME=&NAME4C %then %do;
          proc datasets library=work noprint;
              change temp&i=&NAME;
          run; quit;
          *Only in "SAE_REPORT", remove duplicate observations;
          %if %upcase(&name)=SAE_REPORT %then %do;
              proc transpose data=sae_report(obs=0) out=sae_report4check;
                  var _all_;
              run;
              *Find out the name of the variable referring to "final report";
              data sae_report4check;
                  set sae_report4check end=final;
                  if final then output;
                  call symputx("_IFTRUE_", _name_);
              run;
              %put &_IFTRUE_;
              data sae_report;
                  set sae_report;
                  if upcase(&_IFTRUE_)='TRUE' then output;
              run;
          %end;
        %end;

      %end;
    %end;

    *Arrange the "group" dataset;
    %if %sysfunc(exist(group)) %then %do;
      data group;
          set group;
          label VAR2='Group';
          rename VAR1=SUBJID VAR2=group;
      run;
      proc sort data=group sortseq=linguistic (numeric_collation=on); by SUBJID; run;
    %end;

%mend CHANGE_DS_NAME;

%CHANGE_DS_NAME;


*------------------------------Sheet Dataset------------------------------;

proc sort data=sheet; by Sheet_alias_name; run;

*Split the "Sheet" dataset into multiple datasets;
data _NULL_;
    set sheet;
    by Sheet_alias_name;
    if first.Sheet_alias_name then do;
      i+1;
      call symputx('SUBJ'|| left(put(i, 2.)), Sheet_alias_name);
    end;
    if last.Sheet_alias_name then call symputx('TOTAL', i);
run;

%macro SPLIT();

    %global subj;
    %do i=1 %to &TOTAL;
      data sheet_&&SUBJ&i;
          set sheet;
          by Sheet_category;
          where Sheet_alias_name="&&SUBJ&i";
          *if FieldItem_field_type=' ' then delete;
      run;
      *Create "label_" datasets which do NOT have full-width characters;
      *RSN : Unable to assign variable labels with full-width symbols like "（";
      data label_&&SUBJ&i;
          set sheet_&&SUBJ&i;
          c1=kpropcase(FieldItem_label, 'full-alphabet, half-alphabet');
          c1_2=tranwrd(c1, '%', 'percent');
          c2=kpropcase(Option_name, 'full-alphabet, half-alphabet');
          drop FieldItem_label Option_name c1;
          rename c1_2=FieldItem_label c2=Option_name;
      run;
    %end;

%mend SPLIT;

%SPLIT();


*------------------------------Format------------------------------;

proc sort data=option; by option_name; run;

*Create formats from the "Option" dataset;
data option_2;
    length FMTNAME $24.;
    set option;
    by option_name;
    if first.option_name then do;
      i+1;
    end;
    if Option__Value_code_type='num' then do;
      FMTNAME=catx('_', 'fmt', i, 'f');
      TYPE='N';
    end;
    else if Option__Value_code_type in ('', 'char') then do;
      FMTNAME=catx('_', '$fmt', i, 'f');
      TYPE='C';
    end;
    keep FMTNAME Option_name Option__Value_name Option__Value_code Option__Value_code_type TYPE;
    rename Option__Value_code=START Option__Value_name=LABEL;
run;
proc format cntlin=option_2 library=library; run;

*Create new dataset from the "Option" and "Sheet" dataset for further format statements;
data option_3;
    set option_2;
    by Option_name;
    if first.Option_name then output;
    keep FMTNAME Option_name;
run;
data option_from_sheet;
    set sheet;
    if FieldItem_field_type='ctcae' then Option_name='CTCAE';
    if Option_name=' ' then delete;
run;
proc sort data=option_3; by option_name; run;
proc sort data=option_from_sheet; by option_name; run;

*Combine two datasets created above to make macro varibales later;
data option_f;
    merge option_from_sheet option_3;
    by Option_name;
    if FieldItem_field_type='check' then delete;
    keep Sheet_alias_name field FMTNAME FieldItem_field_type variable;
run;
proc sort data=option_f; by Sheet_alias_name; run;


*------------------------------"Checkbox"-typed Variable------------------------------;

*Find datasets which have a "Checkbox"-typed variable;
%macro FIND_CHECKBOX;

    %global _DSLIST4CHB_;

    proc sql noprint;
        *If there is NOTHING found, let "_DSLIST4CHB_" hold " " (NULL);
        %let _DSLIST4CHB_=;
        *If "Checkbox"-typed variables are found, let "_DSLIST4CHB_" hold the datasets names;
        select cats(upcase(Sheet_alias_name))
          into : _DSLIST4CHB_ separated by " "
        from sheet
          where FieldItem_field_type="checkbox";
    quit;
    %put &_DSLIST4CHB_;

    *Only when "Checkbox"-typed variables are found;
    %if &_DSLIST4CHB_ NE %then %do;
      data chbox;
          set sheet;
          where FieldItem_field_type='checkbox';
      run;
      proc sort data=chbox; by option_name; run;
      proc sql;
          create table ChboxCandidates as
          select a.Option_name, a.variable, a.Sheet_alias_name, a.field, b.Option__Value_name, b.Option__Value_code, b.Option__Value_code_type
          from chbox as a, option as b
          where a.Option_name=b.Option_name;
      quit;
      proc sort data=ChboxCandidates sortseq=linguistic(Numeric_Collation=ON);
          by Option__Value_code;
      run;
      proc sort data=ChboxCandidates;
          by Option_name variable Sheet_alias_name;
      run;
      data chbox_2;
          set ChboxCandidates;
          *Create new variables which show "TRUE" or "FALSE" conditions;
          new_var=cats(variable, '_', Option__Value_code);
      run;
    %end;

    *If "Checkbox"-typed variables are NOT found, make _DSLIST4CHB_ hold "_NOTHING_FOUND_";
    %if &_DSLIST4CHB_= %then %let _DSLIST4CHB_="_NOTHING_FOUND_";

%mend FIND_CHECKBOX;

%FIND_CHECKBOX;

%put &_DSLIST4CHB_;

%macro CONVERT_SUBJID_TO_STRING(ds);

    /* Get the type of VAR9 */
    proc contents data=&ds. out=contents(keep=name type) noprint;
    run;

    /* Check the type of VAR9 */
    data _null_;
        set contents;
        if name = 'VAR9' then call symputx('vartype', type);
    run;

    /* Convert only if VAR9 is numeric */
    %if &vartype = 1 %then %do; /* 1 indicates numeric type */
        data temp;
			length tempVar9 $4.;
            set &ds.;
            tempVar9 = cats(put(VAR9, best12.)); /* Convert numeric to string */
            drop VAR9;
        run;

        data &ds.;
            set temp;
            rename tempVar9 = VAR9;
        run;
    %end;

%mend CONVERT_SUBJID_TO_STRING;

%macro CONVERT_SUBJID();

    %do i=1 %to &TOTAL;
        %let ds=&&SUBJ&i;
        %if %sysfunc(exist(&ds.)) %then %do;
            %CONVERT_SUBJID_TO_STRING(&ds.);
        %end;
    %end;

%mend CONVERT_SUBJID;

%CONVERT_SUBJID();
*------------------------------Macro to Aggregate Datasets------------------------------;
%global _DATE_;
%macro CONVERT_TO_DATE(ds);
	proc sql noprint;
		create table target_date_vars as
		select field
        from sheet_&ds.
        where exists (select * from sheet_&ds. where FieldItem_field_type="date")
        and FieldItem_field_type="date";
	quit;

	proc contents data=&ds. out=contents_data noprint;
	run;

	data non_date_vars;
		set contents_data;
		where index(format, 'YYMMDD') = 0;
	run;

	proc sql noprint;
		create table ds_conv as
		select a.NAME
		from non_date_vars a, target_date_vars b
		where a.NAME = b.field;
	quit;

	*In case there is NOTHING found, let "_DATE_" hold " " (NULL);
    %let _DATE_=;
    *"_DATE_" holds "field" for date-format;
	proc sql noprint;
    	select cats(NAME)
        into : _DATE_ separated by " "
        from ds_conv;
	quit;
	%put &_date_;
		
%mend CONVERT_TO_DATE;

%macro AGGREGATE (ds);

    %if %sysfunc(exist(&ds)) %then %do;

    *Create macro variables;
    proc sql noprint;

        *In case there is NOTHING found, let "_KEEP_" hold " " (NULL);
        %let _KEEP_=;
        *"_KEEP_" holds "field" for keep statement;
        select cats(field)
          into : _KEEP_ separated by " "
        from sheet_&ds.
          where FieldItem_field_type NE "ctcae";

        *In case there is NOTHING found, let "_LABEL_" hold " " (NULL);
        %let _LABEL_=;
        *"_LABEL_" holds "field='FieldItem_label'" for label statement;
        select catx("=", field, quote(compress(FieldItem_label)))
          into : _LABEL_ separated by " "
        from label_&ds.
          where FieldItem_field_type NE "ctcae";

        *In case there is NOTHING found, let "_RENAME_" hold " " (NULL);
        %let _RENAME_=;
        *"_RENAME_" holds "field=variable" for rename statement;
        select catx("=", field, compress(variable))
          into : _RENAME_ separated by " "
        from sheet_&ds.
          where FieldItem_field_type NE "ctcae";

        *In case there is NOTHING found, let "_NUM_" hold " " (NULL);
        %let _NUM_=;
        *"_NUM_" holds "field" for numeric conversion;
        select cats(field)
          into : _NUM_ separated by " "
        from sheet_&ds.
          where exists (select * from sheet_&ds. where FieldItem_field_type="num")
          and FieldItem_field_type="num";

        *In case there is NOTHING found, let "_FORM_" hold " " (NULL);
        %let _FORM_=;
        *"_FORM_" holds "field FMTNAME" for format statement;
        select catx(" ", field, trim(FMTNAME) || '.')
          into : _FORM_ separated by " "
        from option_f
          where exists (select * from option_f where Sheet_alias_name="&ds.")
          and Sheet_alias_name="&ds."
          and FieldItem_field_type='num';

        *In case there is NOTHING found, let "_CTCAE_FLD_" hold " " (NULL);
        %let _CTCAE_FLD_=;
        *"_CTCAE_FLD_" holds "field" for CTCAE conversion;
        select cats(field)
          into : _CTCAE_FLD_ separated by " "
        from sheet_&ds.
          where exists (select * from sheet_&ds. where FieldItem_field_type="ctcae")
          and FieldItem_field_type="ctcae";

        *In case there is NOTHING found, let "_CTCAE_KP1_" hold " " (NULL);
        %let _CTCAE_KP1_=;
        *"_CTCAE_KP1_" holds "variable_trm" for keep statement;
        select cats(variable, '_trm')
          into : _CTCAE_KP1_ separated by " "
        from sheet_&ds.
          where exists (select * from sheet_&ds. where FieldItem_field_type="ctcae")
          and FieldItem_field_type="ctcae";

        *In case there is NOTHING found, let "_CTCAE_KP2_" hold " " (NULL);
        %let _CTCAE_KP2_=;
        *"_CTCAE_KP2_" holds "field" for keep statement;
        select cats(variable, '_grd')
          into : _CTCAE_KP2_ separated by " "
        from sheet_&ds.
          where exists (select * from sheet_&ds. where FieldItem_field_type="ctcae")
          and FieldItem_field_type="ctcae";

        *In case there is NOTHING found, let "_CTCAE_LAB_" hold " " (NULL);
        %let _CTCAE_LAB_=;
        *"_CTCAE_LAB_" holds "'FieldItem_label'" for label statement;
        select cats(quote(compress(FieldItem_label)))
          into : _CTCAE_LAB_ separated by " "
        from sheet_&ds.
          where FieldItem_field_type="ctcae";

        *In case there is NOTHING found, let "_CTCAE_VAR_" hold " " (NULL);
        %let _CTCAE_VAR_=;
        *"_CTCAE_VAR_" holds "variable" to rename CTCAE variables;
        select compress(variable)
          into : _CTCAE_VAR_ separated by " "
        from sheet_&ds.
          where FieldItem_field_type="ctcae";

        *In case there is NOTHING found above, let "_CTCAE_FRM_" hold " " (NULL);
        %let _CTCAE_FRM_=;
        *"_CTCAE_FRM_" holds "variable FMTNAME" for format statement;
        select catx(" ", compress(variable) || '_trm', trim(FMTNAME) || '.')
          into : _CTCAE_FRM_ separated by " "
        from option_f
          where exists (select * from option_f where Sheet_alias_name="&ds.")
          and Sheet_alias_name="&ds."
          and FieldItem_field_type='ctcae';

        %let _FORM_CHAR_=;
        select catx(" ", field, trim(FMTNAME) || '.')
          into : _FORM_CHAR_ separated by " "
        from option_f
          where exists (select * from option_f where Sheet_alias_name="&ds.")
          and Sheet_alias_name="&ds."
          and FieldItem_field_type='char';


    quit;
	%CONVERT_TO_DATE(&ds.);

    *Display macro variables in log window (to check);
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
    %put &_FORM_CHAR_.;

    *Convert character variables specified above;
    %macro CONVERT_1 (varlist1, varlist2, varlist3, varlist4, varlist5);

        *to NUMERIC;
        %local i v1;
        %if &varlist1 ne %then %do;
          %do i=1 %to %sysfunc(countw(&varlist1));
            %let v1=%scan(&varlist1, &i);
            _n_&i=input(&v1, best12.);
            drop &v1;
            rename _n_&i=&v1;
          %end;
        %end;

        *to DATE-FORMAT;
        %local i v2;
        %if &varlist2 ne %then %do;
          %do i=1 %to %sysfunc(countw(&varlist2));
            %let v2=%scan(&varlist2, &i);
            _d_&i=input(&v2, YYMMDD10.);
            format _d_&i YYMMDD10.;
            drop &v2;
            rename _d_&i=&v2;
          %end;
        %end;

        *to CTCAE-FORMAT;
        %local i v3 v4 v5;
        %if &varlist3 ne %then %do;
          %do i=1 %to %sysfunc(countw(&varlist3));
            %let v3=%scan(&varlist3, &i);
            _c1_&i=scan(&v3, 1, '-'); _c1_n_&i=input(_c1_&i, best12.);
            _c2_&i=scan(&v3, -1, '-'); _c2_n_&i=input(_c2_&i, best12.);
            drop &v3 _c1_&i _c2_&i;
            %let v4=%scan(&varlist4, &i, ' ');
            label _c1_n_&i=&v4 _c2_n_&i='Grade';
            %let v5=%scan(&varlist5, &i);
            rename _c1_n_&i=&v5._trm _c2_n_&i=&v5._grd;
          %end;
        %end;

    %mend CONVERT_1;

    data &ds._dmy;
        set &ds.;
        %CONVERT_1 (&_NUM_., &_DATE_., &_CTCAE_FLD_., &_CTCAE_LAB_., &_CTCAE_VAR_.);
    run;

    *Put FORMAT, KEEP, LABEL and RENAME statement;
    data xxx_&ds.;
        set &ds._dmy;
        retain Var_Obs 0;
        Var_Obs+1;
        format &_FORM_. &_CTCAE_FRM_. &_FORM_CHAR_.;
        keep VAR9 &_KEEP_. &_CTCAE_KP1_. &_CTCAE_KP2_. Var_Obs;
        label VAR9='症例登録番号' &_LABEL_.;
        rename VAR9=SUBJID &_RENAME_.;
    run;

    *Only when there are datasets in "_DSLIST4CHB_" which have a "Checkbox"-typed variable;
    %if %upcase(&ds.) in (&_DSLIST4CHB_.) %then %do;

        %macro CONVERT_2 (ds_chb);

            %local n i j k;

            data chbox_3;
                set chbox_2;
                where Sheet_alias_name="&ds_chb.";
                keep Sheet_alias_name Option_name Option__Value_code Option__Value_code_type Option__Value_name field new_var variable;
            run;

            *Create Macro Variables;
            proc sql noprint;
                *"_var_" holds "variable" to list variables which have "checkbox" type in one dataset;
                select cats(variable)
                  into : _var_ separated by " "
                from chbox
                  where Sheet_alias_name="&ds_chb.";
                *"_ch_lab_" holds "new_var='Option__Value_name'" to label new variables;
                select catx('=', new_var, quote(trim(Option__Value_name), "'"))
                  into : _ch_lab_ separated by " "
                from chbox_3;
            quit;
            %put &_var_;
            %put &_ch_lab_;

            *Determine the number of times according to the number of "Checkbox"-typed variabes in one dataset;
            %do i=1 %to %sysfunc(countw(&_var_));
              %let v1=%scan(&_var_, &i);
              %put &v1;

              *"_ch_new_varlist_" holds "new_var" to create new variables;
              proc sql noprint;
                  select cats(new_var)
                    into : _ch_new_varlist_ separated by " "
                  from chbox_3
                    where variable="&v1.";
              quit;
              %put &_ch_new_varlist_;

              data xxx_&ds.;
                  length &_ch_new_varlist_. $8;
                  set xxx_&ds.;
                  *If the varible has no values, set up a '999' flag;
                  comma_&i=countw(&v1. , ',');
                  if &v1.=' ' then comma_&i=999;
                  *Create new variables;
                  array AR(*) &_ch_new_varlist_.;
                  *Set 'FALSE' as default values;
                  do n=1 to dim(AR);
                    AR(n)='FALSE';
                  end;
              run;

              *'number_&i' holds 'comma_&i' to get checkbox variable values;
              proc sql noprint;
                  select cats(comma_&i)
                    into : number_&i separated by " "
                  from xxx_&ds.;
              quit;

              *Assign 'TRUE' values to the relevant variables, depending on the flag variable;
              %do j=1 %to %sysfunc(countw(&&number_&i));
                %let ind_&j=%scan(&&number_&i, &j);
                %put &&ind_&j;
                %if &&ind_&j NE 999 %then %do;
                  %do k=1 %to &&ind_&j;
                    data _null_;
                        set xxx_&ds.;
                        if Var_Obs=&j then do;
                            Var_ChBoxNum=scan(&v1, &k, ',');
                            call symputx('ChBoxNum', Var_ChBoxNum);
                        end;
                    run;
                    data xxx_&ds.;
                        set xxx_&ds.;
                        if Var_Obs=&j then &v1._&ChBoxNum='TRUE';
/*                          array AR(*) &_ch_new_varlist_;*/
/*                          AR(scan(&v1, &k, ','))='TRUE';*/
                    run;
                  %end;
                %end;

              %end;

            %end;

            *Drop the flag variable;
            data xxx_&ds.;
                format SUBJID &_var_;
                set xxx_&ds.;
                *Assign labels to new variables;
                label &_ch_lab_.;
                drop n Var_Obs;
                %do i=1 %to %sysfunc(countw(&_var_));
                  drop comma_&i;
                %end;
            run;

        %mend CONVERT_2;

        %CONVERT_2 (&ds);

    %end;

    *Sort by SUBJID (number);
    proc sort data=xxx_&ds sortseq=linguistic (numeric_collation=on); by SUBJID; run;

    data _NULL_;
        set sheet_&ds;
        by Sheet_category;
        *"_CATEGORY_" holds "Sheet_category" to assign correct category code;
        if first.Sheet_category then call symputx("_CATEGORY_", Sheet_category);
    run;
    %put &_CATEGORY_;

    *Export the datasets to the "ADS" directory (only AE_REPORT, COMMITTEES_OPINION and MULTIPLE);
    %if %upcase(&_CATEGORY_) in (AE_REPORT COMMITTEES_OPINION MULTIPLE) %then %do;

        *If there is an allocation dataset ("group"), add it to the datasets;
        %if %sysfunc(exist(group)) %then %do;
          data xxx_&ds;
              merge xxx_&ds(in=a) group;
              by SUBJID;
              if a;
          run;
        %end;

        *Export the SAS datasets;
        data libads.&ds; set xxx_&ds; run;

        *Create new datasets to show contents of the datasets;
        data &ds._contents;
            set sashelp.vcolumn;
            where libname="LIBADS" and memname="%upcase(&ds.)";
            keep libname memname name type length label format informat;
        run;
        *Export the "contents" dataset;
        proc export data=&ds._contents
            outfile="&ads.\&ds._contents.csv"
            dbms=csv replace;
        run;

    %end;

    *For Ptdata;
    %if not(%upcase(&_CATEGORY_) in (AE_REPORT COMMITTEES_OPINION MULTIPLE)) %then %do;
        data yyy_&ds; set xxx_&ds; run;
        *Sort by SUBJID (character);
        proc sort data=yyy_&ds.; by SUBJID; run;
    %end;

    %end;

%mend AGGREGATE;

*Execute the MACRO (INTEGRATE);

%macro EXECUTE;

    %do i=1 %to &TOTAL;
        %AGGREGATE (&&SUBJ&i);
    %end;

%mend EXECUTE;

%EXECUTE;


*------------------------------Ptdata (Combined Dataset)------------------------------;

%macro COMBINE_into_PTDATA;

    *Get contents of datasets except for "AE, SAE and COMMITTEES_OPINION";
    data to_combine;
        set sashelp.vtable (where=(libname='WORK'));
        if memname=:'YYY';
    run;

    *Create a macro variable that holds the names of the datasets;
    proc sql noprint;
        select cats(memname)
          into : _DSLIST_ separated by " "
        from to_combine;
    quit;
    %put &_DSLIST_;

    *Create "ptdata" by merging the datasets;
    data ptdata;
        merge &_DSLIST_;
        by SUBJID;
    run;
    proc sort data=ptdata sortseq=linguistic (numeric_collation=on); by SUBJID; run;

    *If there is an allocation dataset ("group"), add it to the datasets;
    %if %sysfunc(exist(group)) %then %do;
      data ptdata;
          merge ptdata(in=a) group;
          by SUBJID;
          if a;
      run;
    %end;

    *Export the SAS dataset;
    data libads.ptdata; set ptdata; run;

    *Create new dataset to show contents of the dataset;
    data ptdata_contents;
        set sashelp.vcolumn;
        where libname="LIBADS" and memname="PTDATA";
        keep libname memname name type length label format informat;
    run;
    *Export the "contents" dataset;
    proc export data=ptdata_contents
        outfile="&ads.\ptdata_contents.csv"
        dbms=csv replace;
    run;

%mend COMBINE_into_PTDATA;

%COMBINE_into_PTDATA;


proc printto; run;

*__________EoF__________;
