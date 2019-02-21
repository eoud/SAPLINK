class ZSAPLINK_WD_CHIP definition
  public
  inheriting from ZSAPLINK
  final
  create public .

public section.
*"* public components of class ZSAPLINK_WD_CHIP
*"* do not include other source files here!!!

  constants C_PORT_NODE type STRING value 'PORT'. "#EC NOTEXT
  constants C_PARAM_NODE type STRING value 'PARAM'. "#EC NOTEXT
  constants C_TAG_NODE type STRING value 'TAG'. "#EC NOTEXT

  methods CHECKEXISTS
    redefinition .
  methods CREATEIXMLDOCFROMOBJECT
    redefinition .
  methods CREATEOBJECTFROMIXMLDOC
    redefinition .
  methods VALUEHELP
    redefinition .
protected section.
*"* protected components of class ZSAPLINK_WD_CHIP
*"* do not include other source files here!!!

  methods DELETEOBJECT
    redefinition .
  methods GETOBJECTTYPE
    redefinition .
private section.
*"* private components of class ZSAPLINK_WD_CHIP
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZSAPLINK_WD_CHIP IMPLEMENTATION.


METHOD checkexists.
*/---------------------------------------------------------------------\
*| Web Dynpro ABAP CHIP plugin details                                 |
*|                                                                     |
*| Created by:  Robin Vleeschhouwer                                    |
*| Company:     RV SAP Consultancy                                     |
*| Date:        4 November 2011                                        |
*\---------------------------------------------------------------------/

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

  DATA lv_chip_name TYPE wdy_chip_name.

  "Set name to uppercase
  lv_chip_name = to_upper( objname ).

  "Check if CHIP exists
  exists = cl_wdy_md_chip_factory=>check_existency( p_name = lv_chip_name ).
ENDMETHOD.


METHOD createixmldocfromobject.
*/---------------------------------------------------------------------\
*| Web Dynpro ABAP CHIP plugin details                                 |
*|                                                                     |
*| Created by:  Robin Vleeschhouwer                                    |
*| Company:     RV SAP Consultancy                                     |
*| Date:        4 November 2011                                        |
*\---------------------------------------------------------------------/

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

  "XML nodes
  DATA lo_root_node         TYPE REF TO if_ixml_element.
  DATA lo_port_node         TYPE REF TO if_ixml_element.
  DATA lo_param_node        TYPE REF TO if_ixml_element.
  DATA lo_tag_node          TYPE REF TO if_ixml_element.
  "Chip data
  DATA lo_chip              TYPE REF TO if_wdy_md_chip.
  DATA lv_chip_name         TYPE wdy_chip_name.
  DATA ls_chip_definition   TYPE wdy_chip_def_xt.
  DATA lt_port              TYPE wdy_chip_port_ref_tab.
  DATA ls_port_definition   TYPE wdy_chip_port_xt.
  DATA lt_param             TYPE wdy_chip_param_xt_tab.
  DATA lt_tag               TYPE wdy_chip_tag_tab.
  DATA lx_error             TYPE REF TO cx_root.
  FIELD-SYMBOLS <ls_port>   TYPE wdy_chip_port_ref.
  FIELD-SYMBOLS <ls_param>  TYPE wdy_chip_param_xt.
  FIELD-SYMBOLS <ls_tag>    TYPE wdy_chip_tag.

  "Set name to uppercase
  lv_chip_name = to_upper( objname ).

  TRY.
      lo_chip = cl_wdy_md_chip_factory=>get_chip( p_name = lv_chip_name ).
    CATCH cx_wdy_md_permission_failure
          cx_wdy_md_access_exception
          cx_wdy_md_not_existing INTO lx_error.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>not_found
          msg    = lx_error->get_text( ).
  ENDTRY.

  "Get CHIP definition
  ls_chip_definition = lo_chip->get_definition( ).

  "Get CHIP ports
  lt_port = lo_chip->get_ports( ).

  "Create parent node in XML
  lo_root_node = xmldoc->create_element( getobjecttype( ) ).

  "Set CHIP definition in XML
  setattributesfromstructure( node      = lo_root_node
                              structure = ls_chip_definition ).

  "Set CHIP ports in XML
  LOOP AT lt_port ASSIGNING <ls_port>.
    "Set port definition in XML
    ls_port_definition = <ls_port>-port_ref->get_definition( ).
    lo_port_node = xmldoc->create_element( c_port_node ).
    setattributesfromstructure( node      = lo_port_node
                                structure = ls_port_definition ).
    lo_root_node->append_child( lo_port_node ).

    "Get port parameters
    lt_param = <ls_port>-port_ref->get_parameter_table( ).

    LOOP AT lt_param ASSIGNING <ls_param>.
      "Set port parameters in XML
      lo_param_node = xmldoc->create_element( c_param_node ).
      setattributesfromstructure( node      = lo_param_node
                                  structure = <ls_param> ).
      lo_port_node->append_child( lo_param_node ).

      "Get parameter tags
      lt_tag = <ls_port>-port_ref->get_tag_table( <ls_param>-field_name ).

      LOOP AT lt_tag ASSIGNING <ls_tag>.
        "Set parameter tags in XML
        lo_tag_node = xmldoc->create_element( c_tag_node ).
        setattributesfromstructure( node      = lo_tag_node
                                    structure = <ls_tag> ).
        lo_param_node->append_child( lo_tag_node ).
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

  "Set XML in SAPLink
  xmldoc->append_child( lo_root_node ).
  ixmldocument = xmldoc.
ENDMETHOD.


METHOD createobjectfromixmldoc.
*/---------------------------------------------------------------------\
*| Web Dynpro ABAP CHIP plugin details                                 |
*|                                                                     |
*| Created by:  Robin Vleeschhouwer                                    |
*| Company:     RV SAP Consultancy                                     |
*| Date:        4 November 2011                                        |
*\---------------------------------------------------------------------/

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

  "XML nodes
  DATA lo_root_node         TYPE REF TO if_ixml_element.
  DATA lo_port_node         TYPE REF TO if_ixml_element.
  DATA lo_port_nodes        TYPE REF TO if_ixml_node_list.
  DATA lo_port_iterator     TYPE REF TO if_ixml_node_iterator.
  DATA lo_param_node        TYPE REF TO if_ixml_element.
  DATA lo_param_nodes       TYPE REF TO if_ixml_node_list.
  DATA lo_param_iterator    TYPE REF TO if_ixml_node_iterator.
  DATA lo_tag_node          TYPE REF TO if_ixml_element.
  DATA lo_tag_nodes         TYPE REF TO if_ixml_node_list.
  DATA lo_tag_iterator      TYPE REF TO if_ixml_node_iterator.
  "Chip data
  DATA lo_chip              TYPE REF TO cl_wdy_md_chip.
  DATA lo_port              TYPE REF TO cl_wdy_md_port.
  DATA ls_chip_definition   TYPE wdy_chip_def_xt.
  DATA ls_port_definition   TYPE wdy_chip_port_xt.
  DATA ls_param             TYPE wdy_chip_param_xt.
  DATA ls_tag               TYPE wdy_chip_tag.
  DATA lx_error             TYPE REF TO cx_root.

  "Get XML from SAPLink
  xmldoc = ixmldocument.
  lo_root_node = xmldoc->find_from_name( getobjecttype( ) ).

  "Get CHIP definition
  getstructurefromattributes(
    EXPORTING
      node            = lo_root_node
      preserveversion = abap_true
    CHANGING
      structure       = ls_chip_definition ).

  "Set CHIP name in SAPLink
  objname = ls_chip_definition-chip_name.

  "Check if CHIP already exists
  IF checkexists( ) IS NOT INITIAL.
    IF overwrite IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>existing.
    ELSE.
      "Delete CHIP for new creation
      deleteobject( ).
    ENDIF.
  ENDIF.

  TRY.
      "Create CHIP
      lo_chip = cl_wdy_md_chip=>create_chip( p_name     = ls_chip_definition-chip_name
                                             p_devclass = devclass
                                             p_suppress_access_permission = abap_true ).
      "Set CHIP definition
      lo_chip->modify_definition( ls_chip_definition ).
    CATCH cx_wdy_md_already_existing
          cx_wdy_md_permission_failure
          cx_wdy_md_corr_cancelled
          cx_wdy_md_enqueue_failure
          cx_wdy_md_access_exception
          cx_wdy_md_create_exception INTO lx_error.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = lx_error->get_text( ).
  ENDTRY.

  "Get port nodes
  lo_port_nodes     = lo_root_node->get_children( ).
  lo_port_iterator  = lo_port_nodes->create_iterator( ).
  lo_port_node     ?= lo_port_iterator->get_next( ).
  WHILE lo_port_node IS NOT INITIAL.
    getstructurefromattributes(
      EXPORTING
        node            = lo_port_node
        preserveversion = abap_true
      CHANGING
        structure       = ls_port_definition ).

    TRY.
        "Create port
        lo_port = lo_chip->create_port( p_name = ls_port_definition-port_name
                                        p_kind = ls_port_definition-port_kind ).
        "Set port definition
        lo_port->modify_definition( ls_port_definition ).
      CATCH cx_wdy_md_already_existing
            cx_wdy_md_create_exception INTO lx_error.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = lx_error->get_text( ).
    ENDTRY.

    "Get parameter nodes
    lo_param_nodes    = lo_port_node->get_children( ).
    lo_param_iterator = lo_param_nodes->create_iterator( ).
    lo_param_node    ?= lo_param_iterator->get_next( ).
    WHILE lo_param_node IS NOT INITIAL.
      getstructurefromattributes(
        EXPORTING
          node            = lo_param_node
          preserveversion = abap_true
        CHANGING
          structure       = ls_param ).

      TRY.
          lo_port->create_parameter( ls_param-field_name ).
          lo_port->modify_parameter( ls_param ).
        CATCH cx_wdy_md_already_existing
              cx_wdy_md_create_exception
              cx_wdy_md_not_existing INTO lx_error.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = lx_error->get_text( ).
      ENDTRY.

      "Get tag nodes
      lo_tag_nodes    = lo_param_node->get_children( ).
      lo_tag_iterator = lo_tag_nodes->create_iterator( ).
      lo_tag_node    ?= lo_tag_iterator->get_next( ).
      WHILE lo_tag_node IS NOT INITIAL.
        getstructurefromattributes(
          EXPORTING
            node            = lo_tag_node
            preserveversion = abap_true
          CHANGING
            structure       = ls_tag ).
        TRY.
            lo_port->create_tag(  p_param_name = ls_tag-field_name
                                  p_tag_name   = ls_tag-tag_name ).
          CATCH cx_wdy_md_already_existing
          cx_wdy_md_create_exception INTO lx_error.
            RAISE EXCEPTION TYPE zcx_saplink
              EXPORTING
                textid = zcx_saplink=>error_message
                msg    = lx_error->get_text( ).
        ENDTRY.

        lo_tag_node ?= lo_tag_iterator->get_next( ).
      ENDWHILE.

      lo_param_node ?= lo_param_iterator->get_next( ).
    ENDWHILE.

    lo_port_node ?= lo_port_iterator->get_next( ).
  ENDWHILE.

  "Create TADIR entry
  CALL FUNCTION 'RS_CORR_INSERT'
    EXPORTING
      object          = objname
      object_class    = if_wdy_md_chip=>c_trobjtype
      mode            = 'INSERT'
      global_lock     = abap_true
      devclass        = devclass
      suppress_dialog = abap_true
    EXCEPTIONS
      OTHERS          = 1.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message.
  ENDIF.

  lo_chip->save( ).
  name = objname.
ENDMETHOD.


METHOD deleteobject.
*/---------------------------------------------------------------------\
*| Web Dynpro ABAP CHIP plugin details                                 |
*|                                                                     |
*| Created by:  Robin Vleeschhouwer                                    |
*| Company:     RV SAP Consultancy                                     |
*| Date:        4 November 2011                                        |
*\---------------------------------------------------------------------/

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

  DATA lo_chip      TYPE REF TO cl_wdy_md_chip.
  DATA lv_chip_name TYPE wdy_chip_name.
  DATA lx_error     TYPE REF TO cx_root.

  "Set name to uppercase
  lv_chip_name = to_upper( objname ).

  TRY.
      lo_chip ?= cl_wdy_md_chip_factory=>get_chip( p_name = lv_chip_name ).
    CATCH cx_wdy_md_permission_failure
          cx_wdy_md_access_exception
          cx_wdy_md_not_existing INTO lx_error.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>not_found
          msg    = lx_error->get_text( ).
  ENDTRY.

  TRY.
      lo_chip->lock( ).
    CATCH cx_wdy_md_permission_failure
          cx_wdy_md_corr_cancelled
          cx_wdy_md_enqueue_failure
          cx_wdy_md_access_exception INTO lx_error.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = lx_error->get_text( ).
  ENDTRY.

  lo_chip->delete( ).
  lo_chip->save( ).
  lo_chip->unlock( ).
ENDMETHOD.


METHOD getobjecttype.
*/---------------------------------------------------------------------\
*| Web Dynpro ABAP CHIP plugin details                                 |
*|                                                                     |
*| Created by:  Robin Vleeschhouwer                                    |
*| Company:     RV SAP Consultancy                                     |
*| Date:        4 November 2011                                        |
*\---------------------------------------------------------------------/

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

  "Web Dynpro CHIP
  objecttype = if_wdy_md_chip=>c_trobjtype.
ENDMETHOD.


METHOD valuehelp.
*/---------------------------------------------------------------------\
*| Web Dynpro ABAP CHIP plugin details                                 |
*|                                                                     |
*| Created by:  Robin Vleeschhouwer                                    |
*| Company:     RV SAP Consultancy                                     |
*| Date:        4 November 2011                                        |
*\---------------------------------------------------------------------/

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

  DATA lv_chip_name TYPE wdy_chip_name.

  "Get chip name to search for
  PERFORM get_current_screen_value IN PROGRAM zsaplink IF FOUND
                                   USING 'OBJNAME' '0110'
                                   CHANGING objname.

  "Set name to uppercase
  lv_chip_name = to_upper( objname ).

  "Display value help
  CALL FUNCTION 'F4_GENERIC'
    EXPORTING
      object_type        = 'YP'
      object             = lv_chip_name
      suppress_selection = 'X'
    IMPORTING
      result             = e_objname.

  IF e_objname IS INITIAL.
    e_objname = lv_chip_name.
  ENDIF.
ENDMETHOD.
ENDCLASS.
