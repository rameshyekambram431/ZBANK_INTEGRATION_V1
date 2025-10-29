@EndUserText.label: 'Status Log'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define view entity ZC_STATUSLOG_V1 as projection on ZR_STATUSLOG_V1
{
    key ZStatuslogUuid,
    ZPaymentUuid,
    ZResponsecode,
    ZResponse,
    ZCreatedby,
    ZCreatedate,
    ZLastchangedby,
    ZLastchangedate, 
    ZLocallastchangedate,
    /* Associations */
    _Details: redirected to parent ZC_PAYMENTDETAILS_V1
}
