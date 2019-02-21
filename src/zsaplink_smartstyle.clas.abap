class ZSAPLINK_SMARTSTYLE definition
  public
  inheriting from ZSAPLINK
  create public .

*"* public components of class ZSAPLINK_SMARTSTYLE
*"* do not include other source files here!!!
public section.

  methods CHECKEXISTS
    redefinition .
  methods CREATEIXMLDOCFROMOBJECT
    redefinition .
  methods CREATEOBJECTFROMIXMLDOC
    redefinition .
protected section.
*"* protected components of class ZSAPLINK_SMARTSTYLE
*"* do not include other source files here!!!

  methods DELETEOBJECT
    redefinition .
  methods GETOBJECTTYPE
    redefinition .
private section.
*"* private components of class ZSAPLINK_SMARTSTYLE
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZSAPLINK_SMARTSTYLE IMPLEMENTATION.


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

  SELECT SINGLE stylename FROM stxsadm
                          INTO objname WHERE stylename EQ objname.
  IF sy-subrc EQ 0.
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

  DATA: rootnode        TYPE REF TO if_ixml_element,
        sourcenode      TYPE REF TO if_ixml_element,
        headers_node    TYPE REF TO if_ixml_element,
        paragraphs_node TYPE REF TO if_ixml_element,
        strings_node    TYPE REF TO if_ixml_element,
        tabstops_node   TYPE REF TO if_ixml_element,

        paragraphs      TYPE tsfparas,
        strings         TYPE tsfstrings,
        tabstops        TYPE tsftabs,

        header          TYPE ssfcats,
        rc              TYPE sysubrc,

        _objtype        TYPE string,
        stylename       TYPE tdssname.

  FIELD-SYMBOLS:
        <fs_paragraph>  TYPE ssfparas,
        <fs_string>     TYPE ssfstrings,
        <fs_tabstop>    TYPE stxstab,
        <fs_header>     TYPE ssfcats.


  _objtype = getobjecttype( ).
  rootnode = xmldoc->create_element( _objtype ).

  stylename = objname.

  CALL FUNCTION 'SSF_READ_STYLE'
    EXPORTING
      i_style_name             = stylename
      i_style_active_flag      = 'A'
      i_style_variant          = '%MAIN'
      i_style_language         = sy-langu
    IMPORTING
      e_header                 = header
    TABLES
      e_paragraphs             = paragraphs
      e_strings                = strings
      e_tabstops               = tabstops
    EXCEPTIONS
      no_name                  = 1
      no_style                 = 2
      active_style_not_found   = 3
      inactive_style_not_found = 4
      no_variant               = 5
      no_main_variant          = 6
      cancelled                = 7
      no_access_permission     = 8
      OTHERS                   = 9.

* Read Inactive Version, If Active Version doesn't exists
  IF header-active  IS INITIAL.
    CALL FUNCTION 'SSF_READ_STYLE'
      EXPORTING
        i_style_name                  = stylename
        i_style_active_flag           = 'I'
       i_style_variant                = '%MAIN'
       i_style_language               = sy-langu
     IMPORTING
       e_header                       = header
     TABLES
       e_paragraphs                   = paragraphs
       e_strings                      = strings
       e_tabstops                     = tabstops
     EXCEPTIONS
       no_name                        = 1
       no_style                       = 2
       active_style_not_found         = 3
       inactive_style_not_found       = 4
       no_variant                     = 5
       no_main_variant                = 6
       cancelled                      = 7
       no_access_permission           = 8
       OTHERS                         = 9   .
  ENDIF.

  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1.
        RAISE EXCEPTION TYPE zcx_saplink
               EXPORTING textid = zcx_saplink=>not_found.
      WHEN 2.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg = 'no_style'.

      WHEN 3.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg = 'active_style_not_found'.
      WHEN 4.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg = 'inactive_style_not_found'.
      WHEN 5.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg = 'no_variant'.
      WHEN 6.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg = 'no_main_variant'.
      WHEN 7.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg = 'cancelled'.
      WHEN 8.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg = 'no_access_permission'.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg = 'Error'.
    ENDCASE.

  ENDIF.

*   Populate the header information
  header-stylename = stylename.
  header-active    = 'I'.
  setattributesfromstructure( node      = rootnode
                              structure = header ).


*   Populate the Paragraphs
  LOOP AT paragraphs ASSIGNING <fs_paragraph>.
    <fs_paragraph>-active = 'I'.
    paragraphs_node = xmldoc->create_element( 'paragraphs' ).
    setattributesfromstructure( node = paragraphs_node structure =
    <fs_paragraph> ).
    rc = rootnode->append_child( paragraphs_node ).
  ENDLOOP.

*   Populate the strings
  LOOP AT strings ASSIGNING <fs_string>.
    <fs_string>-active = 'I'.
    strings_node = xmldoc->create_element( 'strings' ).
    setattributesfromstructure( node = strings_node structure =
    <fs_string> ).
    rc = rootnode->append_child( strings_node ).

  ENDLOOP.

*   Populate the tabstops
  LOOP AT tabstops ASSIGNING <fs_tabstop>.
    <fs_tabstop>-active = 'I'.
    tabstops_node = xmldoc->create_element( 'tabstops' ).
    setattributesfromstructure( node = tabstops_node structure =
    <fs_tabstop> ).
    rc = rootnode->append_child( tabstops_node ).
  ENDLOOP.

*   append root node to xmldoc
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

  DATA: rootnode         TYPE REF TO if_ixml_element,
        node             TYPE REF TO if_ixml_element,
        filter           TYPE REF TO if_ixml_node_filter,
        iterator         TYPE REF TO if_ixml_node_iterator,

        paragraphs       TYPE tsfparas,
        strings          TYPE tsfstrings,
        tabstops         TYPE tsftabs,

        wa_paragraph     TYPE ssfparas,
        wa_string        TYPE ssfstrings,
        wa_tabstop       TYPE stxstab,
        header           TYPE ssfcats,

        _objtype         TYPE string,
        checkexists      TYPE flag.


  _objtype    = getobjecttype( ).

  xmldoc      = ixmldocument.
  rootnode    = xmldoc->find_from_name( _objtype ).

  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = rootnode
    CHANGING
      structure = header.


  objname = header-stylename.

* Check for the existance of the object
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

* Paragraphs
  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'paragraphs' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).

  WHILE node IS NOT INITIAL.
    CLEAR wa_paragraph.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = wa_paragraph.
    APPEND wa_paragraph TO paragraphs.
    node ?= iterator->get_next( ).
  ENDWHILE.

* Strings
  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'strings' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).

  WHILE node IS NOT INITIAL.
    CLEAR wa_paragraph.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = wa_string.
    APPEND wa_string TO strings.
    node ?= iterator->get_next( ).
  ENDWHILE.

* Tabstops
  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'tabstops' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).

  WHILE node IS NOT INITIAL.
    CLEAR wa_paragraph.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = wa_tabstop.
    APPEND wa_tabstop TO tabstops.
    node ?= iterator->get_next( ).
  ENDWHILE.

  header-devclass = devclass.

  CALL FUNCTION 'SSF_SAVE_STYLE'
    EXPORTING
      i_header     = header
    TABLES
      i_paragraphs = paragraphs
      i_strings    = strings
      i_tabstops   = tabstops.


* successful install
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

  DATA: stylename TYPE tdssname.

  stylename = objname.
  CALL FUNCTION 'SSF_DELETE_STYLE'
    EXPORTING
      i_stylename           = stylename
      i_with_dialog         = ' '
      i_with_confirm_dialog = ' '
    EXCEPTIONS
      no_name               = 1
      no_style              = 2
      style_locked          = 3
      cancelled             = 4
      no_access_permission  = 5
      illegal_language      = 6
      OTHERS                = 7.
  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1.
        RAISE EXCEPTION TYPE zcx_saplink
               EXPORTING textid = zcx_saplink=>not_found.
      WHEN 2.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg = 'no_style'.

      WHEN 3.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg = 'style_locked'.
      WHEN 4.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg = 'cancelled'.
      WHEN 5.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg = 'no_access_permission'.
      WHEN 6.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg = 'illegal_language'.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg = 'Error'.
    ENDCASE.

  ENDIF.

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

  objecttype = 'SSST'. " Smart Style
ENDMETHOD.
ENDCLASS.
