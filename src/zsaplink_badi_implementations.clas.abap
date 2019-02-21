class ZSAPLINK_BADI_IMPLEMENTATIONS definition
  public
  inheriting from ZSAPLINK
  final
  create public .

*"* public components of class ZSAPLINK_BADI_IMPLEMENTATIONS
*"* do not include other source files here!!!
public section.
  type-pools SEEX .
  type-pools SEOK .

  methods CHECKEXISTS
    redefinition .
  methods CREATEIXMLDOCFROMOBJECT
    redefinition .
  methods CREATEOBJECTFROMIXMLDOC
    redefinition .
protected section.
*"* protected components of class ZSAPLINK_BADI_IMPLEMENTATIONS
*"* do not include other source files here!!!

  methods GETOBJECTTYPE
    redefinition .
  methods DELETEOBJECT
    redefinition .
private section.
*"* private components of class ZSAPLINK_BADI_IMPLEMENTATIONS
*"* do not include other source files here!!!

  methods CREATE_IMPLEMENTATION
    importing
      value(IMPL) type IMPL_DATA
      value(ACTIVE) type SEEX_BOOLEAN default SEEX_FALSE
    changing
      value(MAST_LANGU) type SY-LANGU default SY-LANGU
      value(FILTERS) type SEEX_FILTER_TABLE optional
      value(FCODES_TO_INSERT) type SEEX_FCODE_TABLE optional
      value(COCOS_TO_INSERT) type SEEX_COCO_TABLE optional
      value(INTAS_TO_INSERT) type SEEX_TABLE_TABLE optional
      value(SSCRS_TO_INSERT) type SEEX_SCREEN_TABLE optional
      !KORRNUM type TRKORR
      !DEVCLASS type DEVCLASS
      !METHOD_IMPLEMENTS type SEEX_CLASS_IMPLEMENTATION
    exceptions
      BADI_NOT_EXISTING_OR_WRONG
      WRONG_IMP_NAME
      WRONG_IMP_CLASS_NAME
      WRONG_FILTER_TYPE
      WRONG_FILTER_VALUE
      SAVE_FAILURE
      ACTION_CANCELED
      METHOD_INCLUDE_GENERATING_FAIL
      ACTIVATION_FAILED
      ACCESS_FAILURE .
  methods SAVE_IMPLEMENTATION
    importing
      !IMPL type IMPL_DATA
      value(FLT_EXT) type RSEXSCRN-FLT_EXT
      value(FLT_TYPE) type RSEXSCRN-FLT_TYPE optional
      value(MAINT_LANGU) type SY-LANGU default SY-LANGU
      !FILTER_VAL_OBJ type ref to CL_BADI_FLT_VALUES_ALV
      value(GENFLAG) type GENFLAG default SEEX_FALSE
      value(NO_DIALOG) type SEEX_BOOLEAN default SEEX_FALSE
      !FCODES_TO_INSERT type SEEX_FCODE_TABLE optional
      !COCOS_TO_INSERT type SEEX_COCO_TABLE optional
      !INTAS_TO_INSERT type SEEX_TABLE_TABLE optional
      !SSCRS_TO_INSERT type SEEX_SCREEN_TABLE optional
    exporting
      !MAST_LANGU type SY-LANGU
    changing
      !KORRNUM type TRKORR
      !DEVCLASS type DEVCLASS
    exceptions
      SAVE_FAILURE
      ACTION_CANCELED .
ENDCLASS.



CLASS ZSAPLINK_BADI_IMPLEMENTATIONS IMPLEMENTATION.


method CHECKEXISTS.
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

 DATA: imp_name TYPE exit_imp.

 imp_name = objname.

  CALL FUNCTION 'SXV_IMP_EXISTS'
    EXPORTING
      imp_name                 = imp_name
    EXCEPTIONS
      NOT_EXISTING             = 1
      DATA_INCONSISTENCY       = 2
      OTHERS                   = 3
          .
  IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    exists = 'X'.
  ENDIF.

endmethod.


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

  DATA rootnode   TYPE REF TO if_ixml_element.
  DATA childnode  TYPE REF TO if_ixml_element.

  DATA: exit_name         TYPE exit_def,
        lv_filter_obj     TYPE REF TO cl_badi_flt_struct,
        badi_data         TYPE badi_data,
        impl_data         TYPE impl_data,
        it_methods        TYPE seex_mtd_table,
        it_filters        TYPE seex_filter_table.


  DATA: comp_method_h    TYPE seex_mtd_table,
        comp_screen_h    TYPE seex_screen_table,
        comp_fcode_h     TYPE seex_fcode_table,
        comp_coco_h      TYPE seex_coco_table,
        comp_table_h     TYPE seex_table_table,
        sxc_attrt        TYPE sxc_attrt.


  DATA rc TYPE sysubrc.
  DATA _objname(30) TYPE c.

  DATA _objtype TYPE string.
  DATA imp_name TYPE exit_imp.
  DATA impattr  TYPE sxc_attr.


  FIELD-SYMBOLS: <fs_fcode> LIKE LINE OF comp_fcode_h,
                 <fs_coco>  LIKE LINE OF comp_coco_h,
                 <fs_table> LIKE LINE OF comp_table_h,
                 <fs_screen> LIKE LINE OF comp_screen_h,
                 <fs_filter> LIKE LINE OF it_filters.

  _objtype = getobjecttype( ).
  rootnode = xmldoc->create_element( _objtype ).
  imp_name = objname.
* Check for implementation existance
  SELECT SINGLE * FROM sxc_attr INTO impattr WHERE imp_name EQ imp_name.
  IF sy-subrc EQ 0.
*   Retrieve the badi name for the implementation
    CALL FUNCTION 'SXV_EXIT_FOR_IMP'
      EXPORTING
        imp_name           = imp_name
      IMPORTING
        exit_name          = exit_name
      TABLES
        filters            = it_filters
      EXCEPTIONS
        data_inconsistency = 1
        OTHERS             = 2.
    IF sy-subrc EQ 0.
*     Read the badi definition attributes
      CALL FUNCTION 'SXO_BADI_READ'
        EXPORTING
          exit_name    = exit_name
          maint_langu  = sy-langu
        IMPORTING
          badi         = badi_data
          filter_obj   = lv_filter_obj
        TABLES
          fcodes       = comp_fcode_h
          cocos        = comp_coco_h
          intas        = comp_table_h
          scrns        = comp_screen_h
          methods      = it_methods
        EXCEPTIONS
          read_failure = 1
          OTHERS       = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING textid = zcx_saplink=>system_error.
*          RAISE dump_due_to_badi_error.
      ENDIF.
*     Read the implementation attributes
      CALL FUNCTION 'SXO_IMPL_FOR_BADI_READ'
        EXPORTING
          imp_name                    = imp_name
          exit_name                   = exit_name
          maint_langu                 = sy-langu
          inter_name                  = badi_data-inter_name
          filter_obj                  = lv_filter_obj
          no_create_filter_values_obj = 'X'
        IMPORTING
          impl                        = impl_data
          mast_langu                  = sxc_attrt-sprsl
        TABLES
          fcodes                      = comp_fcode_h
          cocos                       = comp_coco_h
          intas                       = comp_table_h
          scrns                       = comp_screen_h
        CHANGING
          methods                     = it_methods
        EXCEPTIONS
          read_failure                = 1
          OTHERS                      = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING textid = zcx_saplink=>system_error.
      ELSE.
        setattributesfromstructure( node = rootnode structure =  impl_data ).

*       Master Language
        childnode = xmldoc->create_element( 'text' ).
        setattributesfromstructure( node = childnode structure = sxc_attrt ).
        rc = rootnode->append_child( childnode ).


*       Function Codes
        LOOP AT comp_fcode_h ASSIGNING <fs_fcode>.
          childnode = xmldoc->create_element( 'fcodes' ).
          setattributesfromstructure( node = childnode structure = <fs_fcode> ).
          rc = rootnode->append_child( childnode ).
        ENDLOOP.
        UNASSIGN <fs_fcode>.

*       Control Composites
        LOOP AT comp_coco_h ASSIGNING <fs_coco>.
          childnode = xmldoc->create_element( 'cocos' ).
          setattributesfromstructure( node = childnode structure = <fs_coco> ).
          rc = rootnode->append_child( childnode ).
        ENDLOOP.
        UNASSIGN <fs_coco>.

*       Tables
        LOOP AT comp_table_h ASSIGNING <fs_table>.
          childnode = xmldoc->create_element( 'intas' ).
          setattributesfromstructure( node = childnode structure = <fs_table> ).
          rc = rootnode->append_child( childnode ).
        ENDLOOP.
        UNASSIGN <fs_table>.

*       Subscreens
        LOOP AT comp_screen_h ASSIGNING <fs_screen>.
          childnode = xmldoc->create_element( 'scrns' ).
          setattributesfromstructure( node = childnode structure = <fs_screen> ).
          rc = rootnode->append_child( childnode ).
        ENDLOOP.
        UNASSIGN <fs_screen>.

*       Populate Filter Internal table
        IF NOT lv_filter_obj->flt_type IS INITIAL.
          LOOP AT  it_filters ASSIGNING <fs_filter>.
            childnode = xmldoc->create_element( 'fltrs' ).
            setattributesfromstructure( node = childnode structure = <fs_filter> ).
            rc = rootnode->append_child( childnode ).
          ENDLOOP.
          UNASSIGN <fs_filter>.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    CLEAR ixmldocument.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING textid = zcx_saplink=>not_found.
  ENDIF.
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

  DATA rootnode    TYPE REF TO if_ixml_element.
  DATA node        TYPE REF TO if_ixml_element.
  DATA rc          TYPE sysubrc.
  DATA _devclass   TYPE devclass.
  DATA checkexists TYPE flag.
  DATA _objtype    TYPE string.
  DATA imp_data    TYPE impl_data.
  DATA filter TYPE REF TO if_ixml_node_filter.
  DATA iterator TYPE REF TO if_ixml_node_iterator.
  DATA: it_fcodes TYPE seex_fcode_table,
        it_cocos  TYPE seex_coco_table,
        it_intas  TYPE seex_table_table,
        it_sscrs  TYPE seex_screen_table,
        it_filters TYPE seex_filter_table,
        it_methods TYPE seex_class_implementation,
        ls_fcode  LIKE LINE OF it_fcodes,
        ls_cocos  LIKE LINE OF it_cocos,
        ls_intas  LIKE LINE OF it_intas,
        ls_sscrs  LIKE LINE OF it_sscrs,
        ls_filter LIKE LINE OF it_filters,
        ls_attrt  TYPE sxc_attrt,
        l_korrnum TYPE trkorr,
        lv_langu  TYPE sylangu.

  _devclass = devclass.
  _objtype = getobjecttype( ).

  xmldoc = ixmldocument.
  rootnode = xmldoc->find_from_name( _objtype ).

  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = rootnode
    CHANGING
      structure = imp_data.

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

* For Master Language
  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'text' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).
  CLEAR ls_fcode.
  CALL METHOD getstructurefromattributes
    EXPORTING
      node      = node
    CHANGING
      structure = ls_attrt.

* For Function Codes
  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'fcodes' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).

  WHILE node IS NOT INITIAL.
    CLEAR ls_fcode.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = ls_fcode.
    APPEND ls_fcode TO  it_fcodes.
    node ?= iterator->get_next( ).
  ENDWHILE.

* For Control Composites
  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'cocos' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).

  WHILE node IS NOT INITIAL.
    CLEAR ls_cocos.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = ls_cocos.
    APPEND ls_cocos TO it_cocos.
    node ?= iterator->get_next( ).
  ENDWHILE.

* For Tables
  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'intas' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).

  WHILE node IS NOT INITIAL.
    CLEAR ls_intas.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = ls_intas.
    APPEND ls_intas TO it_intas.
    node ?= iterator->get_next( ).
  ENDWHILE.

* For Subscreens
  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'scrns' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).

  WHILE node IS NOT INITIAL.
    CLEAR ls_sscrs.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = ls_sscrs.
    APPEND ls_sscrs TO it_sscrs.
    node ?= iterator->get_next( ).
  ENDWHILE.

* For Filters
  FREE: filter, iterator, node.
  filter = xmldoc->create_filter_name( 'fltrs' ).
  iterator = xmldoc->create_iterator_filtered( filter ).
  node ?= iterator->get_next( ).

  WHILE node IS NOT INITIAL.
    CLEAR ls_filter.
    CALL METHOD getstructurefromattributes
      EXPORTING
        node      = node
      CHANGING
        structure = ls_filter.
    APPEND ls_filter TO it_filters.
    node ?= iterator->get_next( ).
  ENDWHILE.

* Create a Badi Implementation
   CALL METHOD me->create_implementation
    EXPORTING
      impl                                 = imp_data
*     ACTIVE                               = SEEX_FALSE
    CHANGING
      mast_langu                           = ls_attrt-sprsl
      filters                              = it_filters
      fcodes_to_insert                     = it_fcodes
      cocos_to_insert                      = it_cocos
      intas_to_insert                      = it_intas
      sscrs_to_insert                      = it_sscrs
      korrnum                              = l_korrnum
      devclass                             = _devclass
      method_implements                    = it_methods
    EXCEPTIONS
      badi_not_existing_or_wrong           = 1
      wrong_imp_name                       = 2
      wrong_imp_class_name                 = 3
      wrong_filter_type                    = 4
      wrong_filter_value                   = 5
      save_failure                         = 6
      action_canceled                      = 7
      method_include_generating_fail       = 8
      activation_failed                    = 9
      access_failure                       = 10
      OTHERS                               = 11.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING textid = zcx_saplink=>system_error
                msg    = 'badi implementation create failure'.
  ENDIF.

* successful install
  name = objname.
ENDMETHOD.


method CREATE_IMPLEMENTATION.
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

  DATA: status TYPE seex_status,
        flt_ext TYPE sxs_attr-flt_ext,
        flt_type TYPE sxs_attr-flt_type,
        filter_obj TYPE REF TO cl_badi_flt_struct,
        ref TYPE REF TO cl_badi_flt_data_trans_and_db,
        filter_val_obj TYPE REF TO cl_badi_flt_values_alv,
        prot TYPE sprot_u_tab,
        prot_line TYPE sprot_u,
        method_implem TYPE seex_method_implementation,
        gen TYPE genflag.

* At first set the genflag
  IF devclass(1) = '$'.  " local dev. class
    gen = seex_true.
  ELSE.
    gen = seex_false.
  ENDIF.

  CALL FUNCTION 'SXV_EXIT_EXISTS'
    EXPORTING
      exit_name                   = impl-exit_name
    EXCEPTIONS
      not_existing                = 1
      exit_interface_not_existing = 2
      category_conflict           = 3
      no_sxs_inter_entry          = 4
      OTHERS                      = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING badi_not_existing_or_wrong.
  ENDIF.

  SELECT SINGLE flt_type flt_ext
           INTO (flt_type, flt_ext)
           FROM sxs_attr
           WHERE exit_name = impl-exit_name.
  CHECK sy-subrc = 0.

  CALL FUNCTION 'SXV_IMP_NAME_AVAILABLE'
    EXPORTING
      imp_name            = impl-imp_name
      exit_name           = impl-exit_name
    EXCEPTIONS
      not_available       = 1
      invalid_name        = 2
      class_not_available = 3
      already_existing    = 4
      OTHERS              = 5.
  IF sy-subrc <> 0 and sy-subrc <> 3.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING wrong_imp_name.
  ENDIF.
  CALL FUNCTION 'SXV_IMP_CLASS_NAME_AVAILABLE'
    EXPORTING
      imp_class            = impl-imp_class
      exit_name            = impl-exit_name
    EXCEPTIONS
      not_available        = 1
      class_already_exists = 2
      OTHERS               = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING wrong_imp_class_name.
  ENDIF.

  CREATE OBJECT filter_obj
      EXPORTING filter_structure = flt_type
                extend           = flt_ext
      EXCEPTIONS others = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING wrong_filter_type.
  ELSE.
    CREATE OBJECT filter_val_obj
           EXPORTING
              filter_object = filter_obj
              filter_values = filters.
    CREATE OBJECT ref
           EXPORTING
              filter_obj    = filter_obj
              filter_values = filters.
    IF not flt_type is initial.
      CALL METHOD ref->flt_val_check
        IMPORTING
          prot = prot.
      READ TABLE prot INTO prot_line INDEX 1.
      IF sy-subrc = 0.
        IF prot_line-msgnr = 468.
          prot_line-var1 = impl-imp_name.
        ENDIF.
        MESSAGE ID 'ENHANCEMENT'
                TYPE prot_line-severity
                NUMBER prot_line-msgnr
                WITH prot_line-var1
                     prot_line-var2
                     prot_line-var3
                     prot_line-var4
                RAISING wrong_filter_value.
      ENDIF.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'SXO_IMPL_ACCESS_PERMISSION'
    EXPORTING
      imp_name            = impl-imp_name
      mode                = seex_access_insert
      master_language     = mast_langu
      modify_necessary    = seex_true
      no_dialog           = seex_true
    IMPORTING
      new_master_language = mast_langu
    CHANGING
      status              = status
    EXCEPTIONS
      access_failure      = 1
      action_canceled     = 2
      OTHERS              = 3.
  IF sy-subrc = 2.
    RAISE action_canceled.
  ELSEIF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING access_failure.
  ENDIF.


   CALL METHOD me->save_implementation
    EXPORTING
      impl             = impl
      flt_ext          = flt_ext
      flt_type         = flt_type
      maint_langu      = mast_langu
      filter_val_obj   = filter_val_obj
      genflag          = gen
      no_dialog        = seex_true
      fcodes_to_insert = fcodes_to_insert
      cocos_to_insert  = cocos_to_insert
      sscrs_to_insert  = sscrs_to_insert
      intas_to_insert  = intas_to_insert
    IMPORTING
      mast_langu       = mast_langu
    CHANGING
      korrnum          = korrnum
      devclass         = devclass
    EXCEPTIONS
      save_failure     = 1
      action_canceled  = 2
      OTHERS           = 3.
  IF sy-subrc = 2.
    RAISE action_canceled.
  ELSEIF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING save_failure.
  ENDIF.

  CALL FUNCTION 'SXO_IMPL_ACCESS_PERMISSION'
    EXPORTING
      imp_name        = impl-imp_name
      mode            = seex_access_free
      no_dialog       = seex_true
    CHANGING
      status          = status
    EXCEPTIONS
      access_failure  = 1
      action_canceled = 2
      OTHERS          = 3.
  IF sy-subrc = 2.
    RAISE action_canceled.
  ELSEIF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING access_failure.
  ENDIF.

* Generate the method includes
  DATA: mtdkey TYPE seocpdkey,
        lines TYPE i.
  DESCRIBE TABLE method_implements LINES lines.
  IF lines > 0.
    mtdkey-clsname = impl-imp_class.

    IF devclass(1) ne '$'.
      CALL FUNCTION 'RS_CORR_INSERT'
        EXPORTING
          object              = impl-imp_class
          object_class        = seok_r3tr_class
          mode                = seex_access_modify
          genflag             = gen
          global_lock         = seex_true
          devclass            = devclass
          korrnum             = korrnum
        IMPORTING
          devclass            = devclass
          korrnum             = korrnum
        EXCEPTIONS
          cancelled           = 1
          permission_failure  = 2
          unknown_objectclass = 3
          OTHERS              = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                RAISING method_include_generating_fail.
      ENDIF.
    ENDIF.
    LOOP AT method_implements INTO method_implem.
      CONCATENATE impl-inter_name '~' method_implem-methodname
          INTO mtdkey-cpdname.
      CALL FUNCTION 'SEO_METHOD_GENERATE_INCLUDE'
        EXPORTING
          mtdkey                       = mtdkey
          version                      = seoc_version_inactive
          force                        = seox_true
          redefine                     = seox_false
          suppress_corr                = seox_false
          implementation_expanded      = method_implem-source
          suppress_mtdkey_check        = seox_false
          generated                    = gen
        EXCEPTIONS
          not_existing                 = 1
          model_only                   = 2
          include_existing             = 3
          method_imp_not_generated     = 4
          method_imp_not_initialised   = 5
          _internal_class_not_existing = 6
          _internal_method_overflow    = 7
          cancelled                    = 8
          OTHERS                       = 9.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                RAISING method_include_generating_fail.
      ENDIF.
    ENDLOOP.
  ENDIF.

* Active creation  ?
  IF active = seex_true.
    CALL FUNCTION 'SXO_IMPL_ACTIVE'
      EXPORTING
        imp_name                  = impl-imp_name
      EXCEPTIONS
        badi_not_existing         = 1
        imp_not_existing          = 2
        already_active            = 3
        data_inconsistency        = 4
        activation_not_admissable = 5
        action_canceled           = 6
        access_failure            = 7
        OTHERS                    = 8.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              RAISING activation_failed.
    ENDIF.
  ENDIF.

endmethod.


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

  DATA: l_suppress_dialog TYPE char1,
        imp_name          TYPE exit_imp.

  imp_name = objname.
  l_suppress_dialog = 'X'.

  CALL FUNCTION 'SXO_IMPL_DELETE'
    EXPORTING
      imp_name           = imp_name
      no_dialog          = l_suppress_dialog
    EXCEPTIONS
      imp_not_existing   = 1
      action_canceled    = 2
      access_failure     = 3
      data_inconsistency = 4
      OTHERS             = 5.
  IF sy-subrc <> 0.

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

  objecttype = 'SXCI'. "BADI Implementation
ENDMETHOD.


METHOD save_implementation.
*"----------------------------------------------------------------------
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

  DATA: mode            TYPE seex_access_mode,
        imp_class       TYPE seoclsname,
        ref             TYPE REF TO cl_badi_flt_data_trans_and_db,
        temp_korrnum    TYPE trkorr,
        mig_enhspotname TYPE enhspotname,
        err             TYPE REF TO cx_enh_root,
        filter_values_obj TYPE REF TO cl_badi_flt_values_alv,
        l_sxc_class       TYPE sxc_class,
        sxc_attr          TYPE sxc_attr,
        sxc_attrt         TYPE sxc_attrt.

  DATA: comp_method_h    TYPE seex_mtd_table.
  FIELD-SYMBOLS: <fs_method> TYPE seex_mtd_struct.

***** BAdIs und VSR *****
  DATA: imp_kind_struct TYPE sxc_impswh,
        imp_kind_table TYPE STANDARD TABLE OF sxc_impswh.


  CALL FUNCTION 'SXI_EXIT_INTF_READ'
    EXPORTING
      inter_name   = impl-inter_name
    TABLES
      method_table = comp_method_h[]
    EXCEPTIONS
      not_existing = 1
      OTHERS       = 2.

  IF NOT filter_val_obj IS INITIAL.
    filter_values_obj = filter_val_obj.
  ENDIF.

  IF impl-imp_name IS INITIAL OR
     impl-imp_class IS INITIAL OR
     impl-inter_name IS INITIAL OR
     impl-exit_name IS INITIAL.
    MESSAGE e351(enhancement) RAISING save_failure.
  ENDIF.

* Existenz-Check
  CALL FUNCTION 'SXV_IMP_EXISTS'
    EXPORTING
      imp_name     = impl-imp_name
    EXCEPTIONS
      not_existing = 1
      OTHERS       = 2.
  IF sy-subrc = 1.
    mode = seex_access_insert.
  ELSE.
    mode = seex_access_modify.
  ENDIF.

* get instance for saving the implementation
  ref = filter_values_obj->create_inst_for_save_and_trans( ).

  IF ref->filter_count = 0 AND NOT flt_type IS INITIAL.
    MESSAGE w213(enhancement) RAISING save_failure.
  ENDIF.

  IF devclass(1) NE '$' OR genflag = seex_true.
* Korrektureintrag für die Implementierung
    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = impl-imp_name
        object_class        = seex_imp_ob_class
        mode                = mode
        global_lock         = seex_true
        master_language     = mast_langu
        genflag             = genflag
        devclass            = devclass
        korrnum             = korrnum
        suppress_dialog     = no_dialog
      IMPORTING
        devclass            = devclass
        korrnum             = korrnum
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc = 1.
      IF no_dialog = seex_false.
        MESSAGE s112(enhancement).
      ENDIF.
      RAISE action_canceled.
    ELSEIF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
               RAISING save_failure.
    ENDIF.                               " if sy-subrc <> 0
  ENDIF.

* Possibly the implementation class has changed
  SELECT SINGLE imp_class INTO imp_class FROM sxc_class
         WHERE imp_name   = impl-imp_name
           AND inter_name = impl-inter_name
           AND imp_class NE impl-imp_class.
  IF sy-subrc = 0.                     " It has changed
*   Is the implementation class used in another implementation?
    SELECT COUNT(*) FROM sxc_class
            WHERE imp_class  = imp_class
              AND inter_name = impl-inter_name
              AND imp_name  NE impl-imp_name.
    IF sy-subrc = 4. " The class isn't used in another implementation
      CALL FUNCTION 'SXV_IMP_CLASS_DELETE'
        EXPORTING
          imp_name        = impl-imp_name
          inter_name      = impl-inter_name
          no_dialog       = seex_false
          class_name      = imp_class
          preserve        = seex_true
          cls_type        = seex_cls_type_normal
        CHANGING
          korrnum         = korrnum
          devclass        = devclass
        EXCEPTIONS
          failure         = 1
          action_canceled = 2
          OTHERS          = 3.
      IF sy-subrc = 2.
        IF no_dialog = seex_false.
          MESSAGE s112(enhancement).
        ENDIF.
        RAISE action_canceled.
      ELSEIF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
               RAISING save_failure.

      ENDIF.
    ENDIF.
  ENDIF.

***** BAdIs und VSR **********
** save implementation kinds
******************************
  REFRESH imp_kind_table.
  imp_kind_struct-imp_name = impl-imp_name.
  LOOP AT comp_method_h ASSIGNING <fs_method>.
    MOVE-CORRESPONDING <fs_method> TO imp_kind_struct.
    imp_kind_struct-inter_name = impl-inter_name.
    APPEND imp_kind_struct TO imp_kind_table.
  ENDLOOP.
  DELETE FROM sxc_impswh WHERE imp_name = impl-imp_name.
  INSERT sxc_impswh FROM TABLE imp_kind_table.

* Aktuelle Implementierungsklassen erzeugen
  imp_class = impl-imp_class.


* save filter values for extendable filter types
* consider the correktion request not to be initial at this moment
*  IF devclass NE '$TMP'.
  IF devclass(1) NE '$'.
    temp_korrnum = korrnum.
  ELSE.
    CLEAR temp_korrnum.
  ENDIF.
  CALL METHOD ref->save
    EXPORTING
      maint_langu     = maint_langu
      imp_name        = impl-imp_name
      devclass        = devclass
    CHANGING
      korrnum         = temp_korrnum
    EXCEPTIONS
      action_canceled = 1
      OTHERS          = 2.
  IF sy-subrc = 1.
    IF no_dialog = seex_false.
      MESSAGE s112(enhancement).
    ENDIF.
    RAISE action_canceled.
  ELSEIF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
           RAISING save_failure.

  ENDIF.

******************DATENBANKZUGRIFFE***************
  CALL METHOD cl_badi_components=>save
    EXPORTING
      role           = seex_role_imp
      fcodes         = fcodes_to_insert[]
      cocos          = cocos_to_insert[]
      intas          = intas_to_insert[]
      scrns          = sscrs_to_insert[]
    EXCEPTIONS
      write_conflict = 1
      db_error       = 2
      OTHERS         = 3.
  IF sy-subrc = 1.
    IF no_dialog = seex_false.
      MESSAGE i351(enhancement).
    ENDIF.
    RAISE save_failure.
  ELSEIF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
           RAISING save_failure.
  ENDIF.

* Tabelle SMODILOG
  DATA: smodilog_object TYPE REF TO if_clm_tool_log,
        smodi_struct TYPE smodi_tool_log_struct,
        smodi_table TYPE smodi_tool_log_tab,
        trkey TYPE trkey,
        eu_lname TYPE eu_lname, eu_aname TYPE eu_aname,
        lines TYPE i.

  eu_aname = eu_lname = impl-imp_name.

  trkey-obj_type = seex_imp_ob_class.
  trkey-obj_name = eu_aname.
  trkey-sub_type = seex_imp_ob_class.
  trkey-sub_name = eu_lname.
  CALL FUNCTION 'CLM_CREATE_TOOL_LOG_OBJECT'
    EXPORTING
      p_trkey      = trkey
      p_state      = smodi_c_state_active
    CHANGING
      p_log_object = smodilog_object.

  CALL METHOD smodilog_object->get_entries
    IMPORTING
      p_entries_tab = smodi_table.

  DESCRIBE TABLE smodi_table LINES lines.
  IF lines = 0.
    smodi_struct-operation = smodi_c_op_badi_imp.
    APPEND smodi_struct TO smodi_table.
  ELSE.
    LOOP AT smodi_table INTO smodi_struct.
      smodi_struct-operation = smodi_c_op_badi_imp.
      MODIFY smodi_table FROM smodi_struct.
    ENDLOOP.
  ENDIF.

  CALL METHOD smodilog_object->modify_entries
    EXPORTING
      p_entries_tab = smodi_table.
  smodilog_object->main_prog = impl-exit_name.
  smodilog_object->mod_langu = mast_langu.
  CALL METHOD smodilog_object->save
    EXPORTING
      p_state  = smodi_c_state_active
      p_trkorr = korrnum.

* Tabelle SXC_CLASS
* There can be only one(!) interface
  DELETE FROM sxc_class WHERE imp_name   = impl-imp_name.
  MOVE-CORRESPONDING impl TO l_sxc_class.
  INSERT sxc_class FROM l_sxc_class.

* Tabelle SXC_EXIT
  CALL METHOD ref->save_sxc_exit
    EXPORTING
      exit_name = impl-exit_name
      imp_name  = impl-imp_name.

* Tabelle SXC_ATTR
  SELECT SINGLE * FROM sxc_attr
                  INTO sxc_attr WHERE imp_name = impl-imp_name.
  IF sy-subrc NE 0.
    CLEAR sxc_attr.
*  ELSEIF sxc_attr-active  = seex_false.
*    CLEAR: sxc_attr-aname, sxc_attr-adate, sxc_attr-atime.
  ENDIF.

  SELECT SINGLE masterlang
    INTO mast_langu
    FROM tadir WHERE pgmid = seex_pgmid AND
                     object = seex_imp_ob_class AND
                     obj_name = impl-imp_name.

  MOVE: impl-imp_name TO sxc_attr-imp_name,
        impl-uname TO sxc_attr-uname,
        impl-udate TO sxc_attr-udate,
        impl-utime TO sxc_attr-utime.

  IF sxc_attr-uname IS INITIAL. sxc_attr-uname = sy-uname. ENDIF.
  IF sxc_attr-udate IS INITIAL. sxc_attr-udate = sy-datum. ENDIF.
  IF sxc_attr-utime IS INITIAL. sxc_attr-utime = sy-uzeit. ENDIF.

  sxc_attr-layer = impl-layer.

  sxc_attr-mst_lang = mast_langu.
  MODIFY sxc_attr FROM sxc_attr.

* Tabelle SXC_ATTRT
  MOVE-CORRESPONDING impl TO sxc_attrt.
  sxc_attrt-sprsl = maint_langu.
  MODIFY sxc_attrt FROM sxc_attrt.

  CLEAR mig_enhspotname.
  SELECT SINGLE mig_enhspotname FROM sxs_attr INTO mig_enhspotname
    WHERE exit_name = impl-exit_name.

  IF mig_enhspotname IS NOT INITIAL OR sxc_attr-mig_enhname IS NOT INITIAL.
    TRY.
        CALL METHOD cl_enh_classic_badi_migration=>update_badi_implementation
          EXPORTING
            imp_name = impl-imp_name.
      CATCH cx_enh_root INTO err.
        MESSAGE err TYPE 'E'.
    ENDTRY.
  ENDIF.

  CALL FUNCTION 'DB_COMMIT'.
ENDMETHOD.
ENDCLASS.
