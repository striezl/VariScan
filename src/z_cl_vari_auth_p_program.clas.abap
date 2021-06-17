class Z_CL_VARI_AUTH_P_PROGRAM definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces Z_IF_VARI_AUTH .
protected section.
private section.
ENDCLASS.



CLASS Z_CL_VARI_AUTH_P_PROGRAM IMPLEMENTATION.


  method Z_IF_VARI_AUTH~CHECK_AUTHORITY.
    ok = abap_true.
  endmethod.


  METHOD z_if_vari_auth~check_authority_secu.
    authority-check object 'S_PROGRAM'
         ID 'P_GROUP' FIELD secu
         ID 'P_ACTION' FIELD 'VARIANT'.
    IF sy-subrc = 0.
      ok = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
