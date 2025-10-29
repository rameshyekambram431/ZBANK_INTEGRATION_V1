@EndUserText.label: 'Instrument List'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define view entity ZC_INSTRUMENTLIST_V1 as projection on ZR_INSTRUMENTLIST_V1
{
    key ZInstrumentUuid,
    ZPaymentUuid,
    ZMessageid,
    ZStatuscode,
    ZStatusremark,
    ZStatusDescription,
    ZCreatedby,
    ZCreatedate,
    ZLastchangedby,
    ZLastchangedate,
    ZLocallastchangedate,
    /* Associations */
     _Details: redirected to parent ZC_PAYMENTDETAILS_V1
}
