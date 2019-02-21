class ZSAPLINK_PDF_INTERFACES definition
  public
  inheriting from ZSAPLINK
  final
  create public .

*"* public components of class ZSAPLINK_PDF_INTERFACES
*"* do not include other source files here!!!
public section.

  methods CHECKEXISTS
    redefinition .
  methods CREATEIXMLDOCFROMOBJECT
    redefinition .
  methods CREATEOBJECTFROMIXMLDOC
    redefinition .
protected section.
*"* protected components of class ZSAPLINK_INTERFACE_FORMS
*"* do not include other source files here!!!

  methods DELETEOBJECT
    redefinition .
  methods GETOBJECTTYPE
    redefinition .
private section.
*"* private components of class ZSAPLINK_INTERFACE_FORMS
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZSAPLINK_PDF_INTERFACES IMPLEMENTATION.


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
*      Raghavendra Kumar Alwala
*      raghavendra.alwala@gmail.com
  SELECT SINGLE name FROM fpinterface INTO objname WHERE name = objname.
  IF sy-subrc = 0.
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
*      Raghavendra Kumar Alwala
*      raghavendra.alwala@gmail.com
  DATA rootnode     TYPE REF TO if_ixml_element.
  DATA sourcenode   TYPE REF TO if_ixml_element.
  DATA rc           TYPE sysubrc.
  DATA sourcestring TYPE string.
  DATA _objtype     TYPE string.

  TYPES: t_raw(1024) TYPE x.

  DATA: l_wb_interface        TYPE REF TO   if_fp_wb_interface,
        l_interface           TYPE REF TO   if_fp_interface,
        l_interfacename       TYPE          fpname,
        l_name           TYPE          string,
        l_file_table     TYPE          filetable,
        l_filename       TYPE          string,
        l_pathname       TYPE          string,
        l_fullpath       TYPE          string,
        l_rc             TYPE          i,
        l_user_action    TYPE          i,
        l_xstring        TYPE          xstring,
        l_string         TYPE          string,
        l_binary_table   TYPE TABLE OF t_raw,
        l_binary_length  TYPE          i,
        l_node           TYPE REF TO   if_fp_node,
        l_interface_wb        TYPE REF TO   cl_fp_wb_interface,
        l_interface_wb_if     TYPE REF TO   if_fp_wb_interface.

  l_interfacename = objname.

  TRY.
      CALL METHOD cl_fp_wb_interface=>load
        EXPORTING
          i_name         = l_interfacename
        RECEIVING
          r_wb_interface = l_interface_wb_if.
    CATCH cx_fp_api_usage .
      CLEAR ixmldocument.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING textid = zcx_saplink=>not_found.
    CATCH cx_fp_api_repository .
      CLEAR ixmldocument.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING textid = zcx_saplink=>not_found.
    CATCH cx_fp_api_internal .
      CLEAR ixmldocument.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING textid = zcx_saplink=>not_found.
  ENDTRY.

  l_wb_interface ?= l_interface_wb_if.

  l_interface ?= l_wb_interface->get_object( ).

  l_name = l_wb_interface->get_name( ).

  TRY.
      l_xstring = cl_fp_helper=>convert_interface_to_xstring( l_interface ).
    CATCH cx_fp_api_internal.
      CLEAR ixmldocument.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING textid = zcx_saplink=>not_found.
  ENDTRY.

  _objtype = getobjecttype( ).
  rootnode = xmldoc->create_element( _objtype ).

  DATA: wa_fpinterface TYPE fpinterface.

  SELECT SINGLE * FROM fpinterface INTO wa_fpinterface WHERE name = objname.

  setattributesfromstructure( node = rootnode structure =  wa_fpinterface
  ).
  sourcenode = xmldoc->create_element( 'PDFinterface' ).

  l_string = l_xstring.

  rc = sourcenode->if_ixml_node~set_value( l_string ).
  rc = rootnode->append_child( sourcenode ).
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
*      Raghavendra Kumar Alwala
*      raghavendra.alwala@gmail.com

  DATA rootnode         TYPE REF TO if_ixml_element.
  DATA progattribs      TYPE trdir.
  DATA sourcenode       TYPE REF TO if_ixml_element.
  DATA l_xml_node       TYPE REF TO if_ixml_element.
  DATA source           TYPE string.
  DATA sourcetable      TYPE table_of_strings.
  DATA _objtype         TYPE string.
  DATA checkexists      TYPE flag.

  DATA: wa_fpinterface     TYPE fpinterface,
        interfacename        TYPE tdsfname,
        modif_language  TYPE sylangu.

  TYPES: t_raw(1024) TYPE x.

  DATA: l_exception_workbench  TYPE REF TO   cx_fp_ui_workbench,
        l_exception_usage      TYPE REF TO   cx_fp_api_usage,
        l_exception_repository TYPE REF TO   cx_fp_api_repository,
        l_exception_internal   TYPE REF TO   cx_fp_api_internal,
        l_message_text         TYPE          string,
        l_interface            TYPE REF TO   if_fp_interface,
        l_file_table           TYPE          filetable,
        l_filename             TYPE          string,
        l_rc                   TYPE          i,
        l_user_action          TYPE          i,
        l_xstring              TYPE          xstring,
        l_binary_table         TYPE TABLE OF t_raw,
        l_binary_length        TYPE          i,
        l_node                 TYPE REF TO   if_fp_node,
        m_wb_object            TYPE REF TO   if_fp_wb_interface.

  _objtype = getobjecttype( ).
  xmldoc = ixmldocument.
  rootnode = xmldoc->find_from_name( _objtype ).
  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = rootnode
    CHANGING
      structure = wa_fpinterface.
  objname = wa_fpinterface-name.

  checkexists = checkexists( ).
  IF checkexists IS NOT INITIAL.
    IF overwrite IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING textid = zcx_saplink=>existing.
    ELSE.
*     delete object for new install
      deleteobject( ).
    ENDIF.
  ENDIF.

  sourcenode = rootnode->find_from_name( 'PDFinterface' ).
  source = sourcenode->get_value( ).
  l_xstring = source.

  interfacename = objname.

  TRY.
      TRY.
          cl_fp_wb_helper=>interface_exists( wa_fpinterface-name ).
        CATCH cx_fp_api_usage INTO l_exception_usage.
          RAISE EXCEPTION TYPE zcx_saplink
               EXPORTING msg = 'Error creating PDF interface'.
        CATCH cx_fp_api_repository INTO l_exception_repository.

          CASE l_exception_repository->textid.

            WHEN cx_fp_api_repository=>object_already_exists.

              l_message_text = l_exception_repository->get_text( ).

              RAISE EXCEPTION TYPE zcx_saplink
               EXPORTING msg = 'Object already exists'.

          ENDCASE.

      ENDTRY.

      TRY.
          l_interface = cl_fp_helper=>convert_xstring_to_interface( i_xstring = l_xstring i_language = sy-langu ).

*          CALL METHOD cl_fp_helper=>set_new_layout_id
*            CHANGING
*              c_form = l_interface.
        CATCH cx_fp_api_internal.
          RAISE EXCEPTION TYPE zcx_saplink
           EXPORTING msg = 'Convert XML error'.
          EXIT.
      ENDTRY.

      TRY.
          m_wb_object = cl_fp_wb_interface=>create( i_name = wa_fpinterface-name i_interface = l_interface ).
        CATCH cx_fp_api_usage INTO l_exception_usage.

          RAISE EXCEPTION TYPE zcx_saplink
           EXPORTING msg = 'Error creating PDF interface'.

        CATCH cx_fp_api_repository INTO l_exception_repository.

          RAISE EXCEPTION TYPE zcx_saplink
           EXPORTING msg = 'Error creating PDF interface'.

      ENDTRY.

      TRY.
          m_wb_object->save( ).
        CATCH cx_fp_api_usage INTO l_exception_usage.

          TRY.
              m_wb_object->free( ).
            CATCH cx_fp_api.
          ENDTRY.

          RAISE EXCEPTION TYPE zcx_saplink
           EXPORTING msg = 'Error saving PDF interface'.

        CATCH cx_fp_api_repository INTO l_exception_repository.

          TRY.
              m_wb_object->free( ).
            CATCH cx_fp_api.
          ENDTRY.

          RAISE EXCEPTION TYPE zcx_saplink
           EXPORTING msg = 'Error saving PDF interface'.

        CATCH cx_fp_api_internal INTO l_exception_internal.

          TRY.
              m_wb_object->free( ).
            CATCH cx_fp_api.
          ENDTRY.

          RAISE EXCEPTION TYPE zcx_saplink
           EXPORTING msg = 'Error saving PDF interface'.

      ENDTRY.

      TRY.
          m_wb_object->free( ).
        CATCH cx_fp_api.
      ENDTRY.


    CATCH cx_fp_ui_workbench INTO l_exception_workbench.

      IF l_exception_workbench->textid <> cx_fp_ui_workbench=>error_occured.

        l_message_text = l_exception_workbench->get_text( ).
        RAISE EXCEPTION TYPE zcx_saplink
         EXPORTING msg = l_message_text.

      ENDIF.

  ENDTRY.

  name = objname.

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
*      Raghavendra Kumar Alwala
*      raghavendra.alwala@gmail.com

  DATA: l_wb_interface   TYPE REF TO cl_fp_wb_interface,
        l_interface  TYPE fpname.

  l_interface = objname.

  TRY.
      CALL METHOD cl_fp_wb_interface=>delete
        EXPORTING
          i_name = l_interface.
    CATCH cx_fp_api_usage .
    CATCH cx_fp_api_repository .
  ENDTRY.
ENDMETHOD.


METHOD getobjecttype.
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
*      Raghavendra Kumar Alwala
*      raghavendra.alwala@gmail.com
  objecttype = 'SFPI'.
ENDMETHOD.
ENDCLASS.
