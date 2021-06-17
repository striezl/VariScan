interface Z_IF_VARI_AUTH
  public .


  interfaces IF_BADI_INTERFACE .

  methods CHECK_AUTHORITY
    returning
      value(OK) type ABAP_BOOL .
  methods CHECK_AUTHORITY_SECU
    importing
      value(SECU) type SECU
    returning
      value(OK) type ABAP_BOOL .
endinterface.
