CLASS z_insertdatatest DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
    METHODS insert_paymentlog_entry.
*      METHODS: ZPayment IMPORTING iv_xml_document TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS Z_INSERTDATATEST IMPLEMENTATION.


METHOD if_oo_adt_classrun~main.
    insert_paymentlog_entry( ).
  ENDMETHOD.


  METHOD insert_paymentlog_entry.
 DATA: lo_http_destination       TYPE REF TO if_http_destination,
          lo_web_http_client        TYPE REF TO if_web_http_client,
          lo_web_http_get_request   TYPE REF TO if_web_http_request,
          lo_web_http_get_response  TYPE REF TO if_web_http_response,
          lo_web_http_post_request  TYPE REF TO if_web_http_request,
          lo_web_http_post_response TYPE REF TO if_web_http_response,
          lv_response               TYPE string,
          lv_response_code          TYPE string,
          lv_csrf_token             TYPE string.
DATA(lv_message_id) = 'Test123' .
DATA(lv_client_id) = 'THRSTRAVEL'.
DATA(lv_msgsrc_id) = 'THRSTRAVEL'.

        DATA jso TYPE string.
*    jso = '{"ZPaymentFileReference": "' && lv_message_id && '","ZClientcode": "' && lv_client_id && '","ZMessagesourcecode": "' && lv_msgsrc_id && '","ZFileContent": "' && xml_data && '" ,"HasActiveEntity": false,"IsActiveEntity": true}'.
     jso = '{"ZPaymentFileReference": "' && lv_message_id && '","ZClientcode": "' && lv_client_id && '","ZMessagesourcecode": "' && lv_msgsrc_id && '","HasActiveEntity": false,"IsActiveEntity": true}'.
    TRY.
        lo_http_destination = cl_http_destination_provider=>create_by_comm_arrangement( comm_scenario = 'ZOUTGOINGPAYMENT_V1' ).

        lo_web_http_client = cl_web_http_client_manager=>create_by_http_destination( lo_http_destination ).

        lo_web_http_get_request = lo_web_http_client->get_http_request( ).

        lo_web_http_get_request->set_header_fields( VALUE #( ( name = 'x-csrf-token' value = 'fetch' ) ) ).

        lo_web_http_get_response = lo_web_http_client->execute( if_web_http_client=>get ).
        lv_response = lo_web_http_get_response->get_text( ).
        lv_response_code = lo_web_http_get_response->get_status( )-code.

        lv_csrf_token = lo_web_http_get_response->get_header_field( i_name = 'x-csrf-token' ).

        IF lv_csrf_token IS NOT INITIAL.

          lo_web_http_post_request = lo_web_http_client->get_http_request( ).

          lo_web_http_post_request->set_header_fields( VALUE #( ( name = 'x-csrf-token' value = lv_csrf_token ) ) ).
          lo_web_http_post_request->set_content_type( content_type = 'application/json' ).
          IF jso IS NOT INITIAL.
            lo_web_http_post_request->set_text( jso ).
            lo_web_http_post_response = lo_web_http_client->execute( if_web_http_client=>post ).
            lv_response = lo_web_http_post_response->get_text( ).
            lv_response_code = lo_web_http_post_response->get_status( )-code.


          ENDIF.
        ENDIF.

      "CATCH cx_http_dest_provider_error.
      CATCH cx_http_dest_provider_error INTO DATA(lx_dest_err).
         DATA(lv_text1) = lx_dest_err->get_text( ).
     " CATCH cx_web_http_client_error.
      CATCH cx_web_http_client_error INTO DATA(lx_http_err).
         DATA(lv_text2) = lx_http_err->get_text( ).
     " CATCH cx_web_message_error.
     CATCH cx_web_message_error INTO DATA(lx_web_msg_err).
       DATA(lv_error_text) = lx_web_msg_err->get_text( ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
