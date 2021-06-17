class Z_CL_VARI_AUTH definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces Z_IF_VARI_AUTH .
protected section.
private section.
ENDCLASS.



CLASS Z_CL_VARI_AUTH IMPLEMENTATION.


  method Z_IF_VARI_AUTH~CHECK_AUTHORITY.

    AUTHORITY-CHECK OBJECT 'ZVARI' ID 'ZVARI' FIELD 'X'.
    IF sy-subrc = 0.
      ok = abap_true.
    ENDIF.

*** Comment the coding above and uncomment below if you don't want to use the authority check for ZVARI

    "ok = abap_true.

  endmethod.


  METHOD z_if_vari_auth~check_authority_secu.

*    AUTHORITY-CHECK OBJECT 'S_PROGRAM'
*         ID 'P_GROUP' FIELD secu
*         ID 'P_ACTION' FIELD 'VARIANT'.
*    IF sy-subrc = 0.
*      ok = abap_true.
*    ENDIF.

*** Uncomment the coding above and comment below if you want to use the authority check for standard object S_PROGRAM / VARIANT

    ok = abap_true.

  ENDMETHOD.
ENDCLASS.
