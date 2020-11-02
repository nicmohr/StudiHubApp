*&---------------------------------------------------------------------*
*& Report /SWIT/DH_DB_HELP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /swit/dh_db_help.

select SINGLE * from /swit/dh_user WHERE userid eq '37' into  @data(ls_user).

  clear ls_user-firstname.
  clear ls_user-lastname.

  UPDATE /swit/dh_user FROM ls_user.
