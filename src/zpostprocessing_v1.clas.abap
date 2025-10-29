CLASS zpostprocessing_v1 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_badi_interface .
    INTERFACES if_dmee_output_postprocessing .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZPOSTPROCESSING_V1 IMPLEMENTATION.


  METHOD if_dmee_output_postprocessing~modify_output.



DATA hash TYPE string.

DATA(file_string) = cl_dmee_output_file=>to_string(
output_mode = output_mode
output_file_lines = output_file_lines
string = string
xml_document = xml_document ).

" 2) Hash Generation
TRY.
cl_abap_message_digest=>calculate_hash_for_char(
EXPORTING
if_algorithm = 'SHA256'
if_data = file_string
IMPORTING
ef_hashstring = hash ).

TEST-SEAM hash_generation.
END-TEST-SEAM.
CATCH cx_abap_message_digest INTO DATA(cx).
MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(dummy).

APPEND CORRESPONDING #( sy ) TO log.
ENDTRY.

*" removing byte order mark
*cl_dmee_output_file=>remove_bom( CHANGING file_string = file_string ).
*
*" removing XML header from file_string
*DATA(xml_header) = cl_dmee_output_file=>get_xml_header( file_string ).
*cl_dmee_output_file=>remove_xml_header( CHANGING file_string = file_string ).
*
*" 3) Adjusting of the XML
*file_string =
*xml_header && " add the removed header to the beginning of the file
*'<Envelope>' &&
*'<Hash>' &&
*hash &&
*'</Hash>' &&
*'<Payload>' &&
*file_string &&
*'</Payload>' &&
*'</Envelope>'.

" 4) Prepare the table content of the file

cl_dmee_output_file=>to_file(
EXPORTING
file_string = file_string
CHANGING
output_file_lines = output_file_lines ).



*DATA(lo_insert_data) = NEW z_insertdatatest( ).
*lo_insert_data->insert_paymentlog_entry( ).

DATA strin TYPE string.
    strin = file_string.




    DATA: lo_payment_class TYPE REF TO zpaymentclass_v1.


    DATA lv_xml_document TYPE string.

    lv_xml_document = strin.


    IF lv_xml_document IS NOT INITIAL.

      CREATE OBJECT lo_payment_class.

      IF lo_payment_class IS BOUND.
        CALL METHOD lo_payment_class->zpayment_v1
          EXPORTING
            iv_xml_document = lv_xml_document.
      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
