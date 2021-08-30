CLASS zma0_cl_send_html_email DEFINITION PUBLIC FINAL CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_receiver,
            receiver TYPE so_address,
           END OF ty_receiver.

    TYPES: tty_receiver TYPE TABLE OF ty_receiver
            WITH NON-UNIQUE KEY receiver,

           tty_receivers TYPE SORTED TABLE OF sodlienti1
            WITH UNIQUE KEY member_adr.

    METHODS constructor
        IMPORTING
            VALUE(im_dt_list) TYPE soobjinfi1-obj_name
            VALUE(im_sender)  TYPE string.

    METHODS set_body
        IMPORTING
            VALUE(im_body) TYPE TLINET.

    METHODS get_body
        RETURNING
            VALUE(re_body) TYPE TLINET.

    METHODS get_so10_text
        IMPORTING
            VALUE(im_name)     TYPE thead-tdname
            VALUE(im_language) TYPE thead-tdspras
        RETURNING
            VALUE(re_text)     TYPE TLINET.

    METHODS set_sender
        IMPORTING
            VALUE(im_sender) TYPE string.

    METHODS get_sender
        RETURNING
            VALUE(re_sender) TYPE string.

    METHODS set_receiver
        IMPORTING
            VALUE(im_receiver) TYPE
                  zma0_cl_send_html_email=>tty_receivers.

    METHODS get_receiver
        RETURNING
            VALUE(re_receiver) TYPE
                  zma0_cl_send_html_email=>tty_receivers.

    METHODS set_subject
        IMPORTING
            VALUE(im_subject) TYPE string.

    METHODS get_subject
        RETURNING
            VALUE(re_subject) TYPE string.

    METHODS send_email.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS retrive_distribution_list
        IMPORTING
            VALUE(im_dt_list) TYPE soobjinfi1-obj_name
        RETURNING
            VALUE(re_receivers) TYPE
             zma0_cl_send_html_email=>tty_receivers.

    DATA lo_mime_repository TYPE REF TO if_mr_api.

    DATA lo_mime_helper TYPE REF TO cl_gbt_multirelated_service.

    DATA t_body TYPE soli_tab.

    DATA receivers TYPE zma0_cl_send_html_email=>tty_receivers.

    DATA sender TYPE string.

    DATA lo_doc_bcs TYPE REF TO cl_document_bcs.

    DATA lo_bcs TYPE REF TO cl_bcs.

    DATA subject TYPE string.

    DATA lo_sender TYPE REF TO if_sender_bcs.

    DATA lo_recipient TYPE REF TO if_recipient_bcs.

ENDCLASS.



CLASS zma0_cl_send_html_email IMPLEMENTATION.

  METHOD constructor.

    TRY.
      lo_bcs = cl_bcs=>create_persistent( ).
    CATCH cx_send_req_bcs.
      MESSAGE e002(zm_vdrship).
    ENDTRY.

    me->set_sender( im_sender ).

    me->set_receiver( me->retrive_distribution_list( im_dt_list ) ).

    IF lo_mime_repository IS INITIAL.
      lo_mime_repository = cl_mime_repository_api=>if_mr_api~get_api( ).
    ENDIF.

    CREATE OBJECT lo_mime_helper.


  ENDMETHOD.

  METHOD set_body.

    CHECK im_body IS NOT INITIAL.

    LOOP AT im_body INTO DATA(lw_body).

      CASE lw_body-tdformat.
        WHEN ' '.
            APPEND lw_body-tdline TO t_body.
        WHEN 'B'.
            IF t_body IS INITIAL.
              APPEND lw_body-tdline TO t_body.
            ELSE.
              APPEND '<br>' TO t_body.
              APPEND lw_body-tdline TO t_body.
            ENDIF.
        WHEN '*'.
            IF t_body IS INITIAL.
              APPEND lw_body-tdline TO t_body.
            ELSE.
              APPEND '<br>' TO t_body.
              APPEND lw_body-tdline TO t_body.
            ENDIF.
        WHEN OTHERS.
            APPEND lw_body-tdline TO t_body.
      ENDCASE.

    ENDLOOP.

* Set the HTML with description
    CALL METHOD lo_mime_helper->set_main_html
      EXPORTING
        content     = t_body
        filename    = 'message.htm'     "filename for HMTL form
        description = 'Email message'.  "Title

  ENDMETHOD.

  METHOD get_body.

    re_body = t_body.

  ENDMETHOD.

  METHOD get_so10_text.

    DATA lt_text TYPE STANDARD TABLE OF tline.

    DATA lw_body TYPE soli.

* Get the header text in SO10
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt    " Client
        id                      = 'ST'    " Text ID of text to be read
        language                = im_language    " Language of text to be read
        name                    = im_name " Name of text to be read
        object                  = 'TEXT'   " Object of text to be read
      TABLES
        lines                   = re_text    " Lines of text read
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
      OTHERS                    = 8.

    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD set_sender.

    sender = im_sender.

    TRY.
      lo_sender = cl_cam_address_bcs=>create_internet_address(
       CONV #( sender ) ).
    CATCH cx_address_bcs.
      MESSAGE e002(zm_vdrship).
    ENDTRY.

    TRY.
      lo_bcs->set_sender(
      EXPORTING
      i_sender = lo_sender ).
    CATCH cx_send_req_bcs.
      MESSAGE e002(zm_vdrship).
    ENDTRY.

  ENDMETHOD.

  METHOD get_sender.

    re_sender = sender.

  ENDMETHOD.

  METHOD set_receiver.

    receivers = im_receiver.

    LOOP AT im_receiver INTO DATA(lw_receiver).

      TRY.
        lo_recipient = cl_cam_address_bcs=>create_internet_address(
            i_address_string = CONV #( lw_receiver-member_adr )
            ).
      CATCH cx_address_bcs.
        MESSAGE e002(zm_vdrship).
      ENDTRY.

      TRY.
        lo_bcs->add_recipient( i_recipient = lo_recipient ).
      CATCH cx_send_req_bcs.
        MESSAGE e002(zm_vdrship).
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_receiver.

    re_receiver = receivers.

  ENDMETHOD.

  METHOD retrive_distribution_list.

    DATA lt_receivers TYPE TABLE OF SODLIENTI1.

    CHECK im_dt_list IS NOT INITIAL.

* Get the mail address from Distribution List
    CALL FUNCTION 'SO_DLI_READ_API1'
      EXPORTING
        dli_name                   = im_dt_list
        shared_dli                 = 'X'
      TABLES
        dli_entries                = lt_receivers
      EXCEPTIONS
        dli_not_exist              = 1
        operation_no_authorization = 2
        parameter_error            = 3
        x_error                    = 4
      OTHERS                       = 5.

    IF sy-subrc <> 0.
      MESSAGE e001(zmm_vdrship).
    ENDIF.

    re_receivers = lt_receivers.

  ENDMETHOD.

  METHOD set_subject.

    CHECK im_subject IS NOT INITIAL.

  "Create HTML using BCS class and attach html and image part to it.
  "lv_subject = lc_subject_email.  "subject
    TRY.
      lo_doc_bcs = cl_document_bcs=>create_from_multirelated(
        i_subject          = CONV #( im_subject )
        i_multirel_service = lo_mime_helper ).
    CATCH cx_document_bcs cx_gbt_mime cx_bcom_mime.
      MESSAGE e002(zm_vdrship).
    ENDTRY.


"Create HTML Document
    TRY.
      "lo_bcs = cl_bcs=>create_persistent( ).
      lo_bcs->set_document( i_document = lo_doc_bcs ).
    CATCH cx_send_req_bcs.
      MESSAGE e002(zm_vdrship).
    ENDTRY.

  ENDMETHOD.

  METHOD get_subject.

    re_subject = subject.

  ENDMETHOD.

  METHOD send_email.

* Send email
    TRY.
      lo_bcs->send( ).
    CATCH cx_send_req_bcs.
      MESSAGE e002(zm_vdrship).
    ENDTRY.
    COMMIT WORK AND WAIT.

  ENDMETHOD.

ENDCLASS.
