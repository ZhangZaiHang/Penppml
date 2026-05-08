******************************************************************************************************************************
******************************************************************************************************************************
***
*** Title of project:	Regularized PPML for High-Dimensional Structural Gravity: Evidence on Agricultural RTA Effects
*** Coauthor:			Zaihang Zhang (Nanjing Agricultural University)
*** Data source: 		International Trade and Production Database for Estimation (ITPD-E); Deep Trade Agreements (DTA)
***
*** written: 			1 May 2025
*** 
*** by: 				Zaihang Zhang
*** E-Mail: 		    zzaihang_feb04@outlook.com
*** 
******************************************************************************************************************************
******************************************************************************************************************************

clear all

//////////////////////////////////////////////
/* SET PATH OF REPLICATION PACKAGE MANUALLY */
//////////////////////////////////////////////

global path			=	"C:\Users\ZaihangZhang\Desktop\MachineLearninginAgriculturalTradeResearch"				


******************************************************************************************************************************
***	Specification of the globals calling the standardized structures of directories
******************************************************************************************************************************

global ext_data		=	"${path}\data\ext"					   	 		/* Path for external data */
global usr_data		=	"${path}\data\usr"				   	    		/* Path for user generated files */
global tmp_data		=	"${path}\data\tmp"					   	 		/* Path for temporary files */
global path_code	=	"${path}\code"									/* Path for program codes */
global path_log		=	"${path}\results\logs"				    		/* Path for log files */
global path_tables	=	"${path}\results\tables"						/* Path for tables */
global path_figures	=	"${path}\results\figures"						/* Path for figures */

******************************************************************************************************************************

cap log close
set logtype text
set linesize 255

******************************************************************************************************************************
******************************************************************************************************************************
***	End global settings
******************************************************************************************************************************
******************************************************************************************************************************


******************************************************************************************************************************
*** 1. Preparation of trade data
******************************************************************************************************************************
//
// log using ${path_log}/data_preparation.log, replace
// do ${path_code}/data_preparation.do
// log close

******************************************************************************************************************************
*** 2. Figures (main text)
******************************************************************************************************************************

// log using ${path_log}/20_Figure_1.log, replace
// do ${path_code}/20_Figure_1.do
// log close
//
// log using ${path_log}/20_Figure_2.log, replace
// do ${path_code}/20_Figure_2.do
// log close
//
// log using ${path_log}/20_Figure_3.log, replace
// do ${path_code}/20_Figure_3.do
// log close
//
// log using ${path_log}/20_Figure_4.log, replace
// do ${path_code}/20_Figure_4.do
// log close

******************************************************************************************************************************
*** 3. Tables (main text)
******************************************************************************************************************************

// log using ${path_log}/30_Table_1.log, replace
// do ${path_code}/30_Table_1.do
// log close

******************************************************************************************************************************
*** EOF
******************************************************************************************************************************
