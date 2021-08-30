REPORT zma0_send_email_model.

DATA: lo_send_email TYPE REF TO ZMA0_CL_SEND_HTML_EMAIL.

lo_send_email =  NEW #(
    im_dt_list = 'LIST_TEST'
    im_sender  = 'silvapitterr@johndeere.com'
).


DATA(lt_text) = lo_send_email->get_so10_text(
    im_name = 'ZM_TEXT_TEST'
    im_language = sy-langu
).

lo_send_email->set_body( lt_text ).

lo_send_email->set_subject( 'TESTING ZMA0_SEND_HTML_EMAIL CLASS' ).

lo_send_email->send_email( ).
