@EndUserText.label: 'Payment Log V1'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define view entity ZC_PAYMENTLOG_V1 as projection on ZR_PAYMENTLOG_V1
{
    key ZPaymentLogUuid,
    @EndUserText.label: 'Payment UUID'
    ZPaymentUuid,
    @EndUserText.label: 'Resonse Code'
    ZResponsecode,
    @EndUserText.label: 'Resposne'
    ZResponse,
    ZCreatedby,
    ZCreatedate,
    ZLastchangedby,
    ZLastchangedate,
    ZLocallastchangedate,
    /* Associations */
    _Details: redirected to parent ZC_PAYMENTDETAILS_V1
}
