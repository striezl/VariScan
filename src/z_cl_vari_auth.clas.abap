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
      ok = 'X'.
    ENDIF.

  endmethod.
ENDCLASS.
