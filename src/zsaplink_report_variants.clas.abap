class ZSAPLINK_REPORT_VARIANTS definition
  public
  inheriting from ZSAPLINK
  final
  create public .

*"* public components of class ZSAPLINK_REPORT_VARIANTS
*"* do not include other source files here!!!
public section.

  methods CHECKEXISTS
    redefinition .
  methods CREATEIXMLDOCFROMOBJECT
    redefinition .
  methods CREATEOBJECTFROMIXMLDOC
    redefinition .
protected section.
*"* protected components of class ZSAPLINK_REPORT_VARIANTS
*"* do not include other source files here!!!

  class-methods GET_VARIANT_NAME
    importing
      !OBJECT_NAME type STRING
    exporting
      !PROGRAM_NAME type RSVAR-REPORT
      !VARIANT_NAME type RSVAR-VARIANT .

  methods DELETEOBJECT
    redefinition .
  methods GETOBJECTTYPE
    redefinition .
private section.
*"* private components of class ZSAPLINK_REPORT_VARIANTS
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZSAPLINK_REPORT_VARIANTS IMPLEMENTATION.


METHOD checkexists.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Kareemullah Quadri
*      quadri.ks@gmail.com

  DATA:  l_program_name TYPE rsvar-report,
         l_variant_name TYPE rsvar-variant,
          l_return_code TYPE sy-subrc.

  CALL METHOD zsaplink_report_variants=>get_variant_name
    EXPORTING
      object_name  = objname
    IMPORTING
      program_name = l_program_name
      variant_name = l_variant_name.

  CALL FUNCTION 'RS_VARIANT_EXISTS'
    EXPORTING
      report              = l_program_name
      variant             = l_variant_name
    IMPORTING
      r_c                 = l_return_code
    EXCEPTIONS
      not_authorized      = 1
      no_report           = 2
      report_not_existent = 3
      report_not_supplied = 4
      OTHERS              = 5.

  IF sy-subrc NE 0 OR
     l_return_code NE 0.
    CLEAR exists.
  ELSE.
    exists = 'X'.
  ENDIF.

ENDMETHOD.


METHOD createixmldocfromobject.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Kareemullah Quadri
*      quadri.ks@gmail.com

  DATA:  l_program_name TYPE          rsvar-report,
         l_variant_name TYPE          rsvar-variant,
         lt_rsparams    TYPE TABLE OF rsparams,
         ls_rsparams    TYPE          rsparams,
         ls_varid       TYPE          varid,

         lt_varit       TYPE TABLE OF varit,
         ls_varit       TYPE          varit,

         lt_varis       TYPE TABLE OF varis,
         ls_varis       TYPE          varis.


  DATA: _objtype     TYPE string,
        mtext        TYPE string,
        rc           TYPE sysubrc.

* XML nodes
  DATA:rootnode     TYPE REF TO if_ixml_element,
       rsparam_node TYPE REF TO if_ixml_element,
       varid_node   TYPE REF TO if_ixml_element,
       varit_node   TYPE REF TO if_ixml_element,
       varis_node   TYPE REF TO if_ixml_element.

  CALL METHOD zsaplink_report_variants=>get_variant_name
    EXPORTING
      object_name  = objname
    IMPORTING
      program_name = l_program_name
      variant_name = l_variant_name.

  CLEAR ls_varid.
  SELECT SINGLE *
    INTO ls_varid
    FROM varid
   WHERE report  EQ l_program_name
     AND variant EQ l_variant_name.

  IF sy-subrc NE 0.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING textid = zcx_saplink=>not_found.
  ENDIF.


  CLEAR lt_varit[].
  SELECT *
    INTO TABLE lt_varit
    FROM varit
   WHERE report  EQ l_program_name
     AND variant EQ l_variant_name.
  IF sy-subrc NE 0.
*     Do nothing...No texts for variants !!
*     Thanks Ok...
  ENDIF.

  CLEAR lt_varis.
  SELECT *
    INTO TABLE lt_varis
    FROM varis
   WHERE report  EQ l_program_name
     AND variant EQ l_variant_name.
  IF sy-subrc NE 0.
*     Do nothing...default selection screen used
  ENDIF.

  CALL FUNCTION 'RS_VARIANT_CONTENTS'
    EXPORTING
      report                      = l_program_name
      variant                     = l_variant_name
*   MOVE_OR_WRITE               = 'W'
*   NO_IMPORT                   = ' '
*   EXECUTE_DIRECT              = ' '
* IMPORTING
*   SP                          =
    TABLES
*   L_PARAMS                    =
*   L_PARAMS_NONV               =
*   L_SELOP                     =
*   L_SELOP_NONV                =
      valutab                     = lt_rsparams
*   OBJECTS                     =
*   FREE_SELECTIONS_DESC        =
*   FREE_SELECTIONS_VALUE       =
   EXCEPTIONS
     variant_non_existent        = 1
     variant_obsolete            = 2
     OTHERS                      = 3
            .
  IF sy-subrc <> 0.

    CASE sy-subrc.
      WHEN 2.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING textid = zcx_saplink=>not_found.

      WHEN OTHERS.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                INTO mtext
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING msg = mtext.

    ENDCASE.

  ENDIF.

  _objtype = getobjecttype( ).
  rootnode = xmldoc->create_element( _objtype ).
  rootNode->set_attribute( name = 'VARIANT_NAME' value = objname  ).

    varid_node = xmldoc->create_element( 'varid' ).
    setattributesfromstructure( node = varid_node structure = ls_varid ).
    rc = rootnode->append_child( varid_node ).

  LOOP AT lt_varit INTO ls_varit.
    varit_node = xmldoc->create_element( 'varit' ).
    setattributesfromstructure( node = varit_node structure = ls_varit ).
    rc = rootnode->append_child( varit_node ).
  ENDLOOP.

  LOOP AT lt_varis INTO ls_varis.
    varis_node = xmldoc->create_element( 'varis' ).
    setattributesfromstructure( node = varis_node structure = ls_varis ).
    rc = rootnode->append_child( varis_node ).
  ENDLOOP.

  LOOP AT lt_rsparams INTO ls_rsparams.
    rsparam_node = xmldoc->create_element( 'rsparam' ).
    setattributesfromstructure( node = rsparam_node structure = ls_rsparams ).
    rc = rootnode->append_child( rsparam_node ).
  ENDLOOP.

  rc = xmldoc->append_child( rootnode ).
  ixmldocument = xmldoc.

ENDMETHOD.


METHOD createobjectfromixmldoc.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Kareemullah Quadri
*      quadri.ks@gmail.com

*      Plugin created by:
*      Kareemullah Quadri
*      quadri.ks@gmail.com

  DATA:  l_program_name TYPE          rsvar-report,
         l_variant_name TYPE          rsvar-variant,
         lt_rsparams    TYPE TABLE OF rsparams,
         ls_rsparams    TYPE          rsparams,
         ls_varid       TYPE          varid,

         lt_varit       TYPE TABLE OF varit,
         ls_varit       TYPE          varit,

         lt_rsdynnr     TYPE TABLE OF rsdynnr,
         ls_rsdynnr     TYPE          rsdynnr,

         lt_varis       TYPE TABLE OF varis,
         ls_varis       TYPE          varis.


  DATA: _objtype     TYPE string,
        mtext        TYPE string,
        rc           TYPE sysubrc.

  DATA : node        TYPE REF TO if_ixml_element,
         filter      TYPE REF TO if_ixml_node_filter,
         iterator    TYPE REF TO if_ixml_node_iterator.

* XML nodes
  DATA:rootnode     TYPE REF TO if_ixml_element,
       rsparam_node TYPE REF TO if_ixml_element,
       varid_node   TYPE REF TO if_ixml_element,
       varit_node   TYPE REF TO if_ixml_element,
       varis_node   TYPE REF TO if_ixml_element.

  DATA  l_checkexists TYPE flag.

  _objtype = getobjecttype( ).

  xmldoc = ixmldocument.
  rootnode = xmldoc->find_from_name( _objtype ).

  varid_node = xmldoc->find_from_name( 'varid' ).
  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = varid_node
    CHANGING
      structure = ls_varid.

  CONCATENATE ls_varid-report ls_varid-variant
         INTO objname RESPECTING BLANKS .
  name = objname.

  l_checkexists = checkexists( ).
  IF l_checkexists IS NOT INITIAL.
    IF overwrite IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING textid = zcx_saplink=>existing.
    ELSE.
*     delete object for new install
      deleteobject( ).
    ENDIF.
  ENDIF.

  ls_varid-mandt = sy-mandt.

* Parameter Values
  FREE: filter, iterator, rsparam_node.
  filter = xmldoc->create_filter_name( 'rsparam' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  rsparam_node ?= iterator->get_next( ).
  WHILE rsparam_node IS NOT INITIAL.
    CLEAR ls_rsparams.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = rsparam_node
      CHANGING
        structure = ls_rsparams.
    APPEND ls_rsparams TO lt_rsparams.
    rsparam_node ?= iterator->get_next( ).
  ENDWHILE.


* Get Variant texts in different languages
  FREE: filter, iterator, varit_node.
  filter = xmldoc->create_filter_name( 'varit' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  varit_node ?= iterator->get_next( ).
  WHILE varit_node IS NOT INITIAL.
    CLEAR ls_varit.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = varit_node
      CHANGING
        structure = ls_varit.

    ls_varit-mandt = sy-mandt.
    APPEND ls_varit TO lt_varit.
    varit_node ?= iterator->get_next( ).
  ENDWHILE.

* Get variants per screen
  FREE: filter, iterator, varis_node.
  filter = xmldoc->create_filter_name( 'varis' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  varis_node ?= iterator->get_next( ).
  WHILE varis_node IS NOT INITIAL.
    CLEAR ls_varit.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = varis_node
      CHANGING
        structure = ls_varis.

    CLEAR ls_rsdynnr.
    ls_rsdynnr-dynnr = ls_varis-dynnr.
    APPEND ls_rsdynnr TO lt_rsdynnr.
    varis_node ?= iterator->get_next( ).
  ENDWHILE.

  CLEAR mtext.
  CALL FUNCTION 'RS_CREATE_VARIANT'
    EXPORTING
      curr_report               = ls_varid-report
      curr_variant              = ls_varid-variant
      vari_desc                 = ls_varid
    TABLES
      vari_contents             = lt_rsparams
      vari_text                 = lt_varit
      vscreens                  = lt_rsdynnr
    EXCEPTIONS
      illegal_report_or_variant = 1
      illegal_variantname       = 2
      not_authorized            = 3
      not_executed              = 4
      report_not_existent       = 5
      report_not_supplied       = 6
      variant_exists            = 7
      variant_locked            = 8
      OTHERS                    = 9.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            INTO mtext
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING msg = mtext.
  ENDIF.


ENDMETHOD.


METHOD deleteobject.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Kareemullah Quadri
*      quadri.ks@gmail.com

  DATA:  l_program_name TYPE rsvar-report,
         l_variant_name TYPE rsvar-variant,
         mtext          TYPE string.

  CALL METHOD zsaplink_report_variants=>get_variant_name
    EXPORTING
      object_name  = objname
    IMPORTING
      program_name = l_program_name
      variant_name = l_variant_name.

  CALL FUNCTION 'RS_VARIANT_DELETE'
    EXPORTING
      report                     = l_program_name
      variant                    = l_variant_name
*   FLAG_CONFIRMSCREEN         =
*   FLAG_DELALLCLIENT          =
* IMPORTING
*   VARIANT                    =
   EXCEPTIONS
     not_authorized             = 1
     not_executed               = 2
     no_report                  = 3
     report_not_existent        = 4
     report_not_supplied        = 5
     variant_locked             = 6
     variant_not_existent       = 7
     no_corr_insert             = 8
     variant_protected          = 9
     OTHERS                     = 10
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            INTO mtext
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING msg = mtext.

  ENDIF.


ENDMETHOD.


method GETOBJECTTYPE.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Kareemullah Quadri
*      quadri.ks@gmail.com

  objecttype = 'VARI'.  "Variants
endmethod.


METHOD get_variant_name.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Kareemullah Quadri
*      quadri.ks@gmail.com

* Purpose: Splits the Object name into
*          Report and Variant name assuming  object name is
*          stored as SAP std in transports

  DATA: ls_dd03l     TYPE dd03l,
        l_objname    TYPE string,
        l_offset(2)  TYPE n.

  l_objname = object_name.

  CLEAR ls_dd03l.
  SELECT SINGLE *
    INTO ls_dd03l
    FROM dd03l
   WHERE tabname   EQ 'RSVAR'
     AND fieldname EQ 'REPORT'.

  IF sy-subrc EQ 0.
    l_offset = ls_dd03l-leng.
  ELSE.
    l_offset = '40'.
  ENDIF.
  program_name = l_objname+0(l_offset).
  SHIFT l_objname BY l_offset PLACES LEFT.

*  CLEAR ls_dd03l.
*  SELECT SINGLE *
*    INTO ls_dd03l
*    FROM dd03l
*   WHERE tabname   EQ 'RSVAR'
*     AND fieldname EQ 'VARIANT'.
*
*  IF sy-subrc EQ 0.
*    l_offset = ls_dd03l-leng.
*  ELSE.
*    l_offset = '12'.
*  ENDIF.

  variant_name = l_objname.

ENDMETHOD.
ENDCLASS.
