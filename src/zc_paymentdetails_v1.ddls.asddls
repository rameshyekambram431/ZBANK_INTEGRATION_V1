@EndUserText.label: 'Payment Details'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZC_PAYMENTDETAILS_V1
provider contract transactional_query as 
projection on ZR_PAYMENTDETAILS_V1
{
    key ZPaymentUuid,
    @EndUserText.label: 'Payment File Reference'
    ZPaymentFileReference,
    @EndUserText.label: 'File Content'
    ZFileContent,
    @EndUserText.label: 'Encrypted File'
    ZEncryptedfile,
    @EndUserText.label: 'Payment Status'
    ZPaymentstatus,
    @EndUserText.label: 'Bank Processed Status'
    ZBankProcessedStatus,
    @EndUserText.label: 'Payment Status Code'
    ZPaymentstatuscode,
    @EndUserText.label: 'Payment Message Id'
    ZPaymentmessageid,
    @EndUserText.label: 'Payment Sync'
    ZPaymentsync,
    @EndUserText.label: 'Status Sync'
    ZStatussync,
    @EndUserText.label: 'Client Code'
    ZClientcode,
    @EndUserText.label: 'Message Source Code'
    ZMessagesourcecode,
    ZCreatedby,
    ZCreatedate,
    ZLastchangedby,
    ZLastchangedate,
    ZLocallastchangedate,
    /* Associations */
    _Log:redirected to composition child ZC_PAYMENTLOG_V1,
    _StatusLog: redirected to composition child ZC_STATUSLOG_V1,
    _InstrumentLog:redirected to composition child ZC_INSTRUMENTLIST_V1
}
