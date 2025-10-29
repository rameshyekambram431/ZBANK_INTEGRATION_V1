@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Payment Status Details V1'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZPAYMENTSTATUSDETAILS_V1 as select from DDCDS_CUSTOMER_DOMAIN_VALUE_T( p_domain_name: 'ZPAYMENTSTATUS_V1')
{
      @UI.hidden: true
  key domain_name,
      @UI.hidden: true
  key value_position,
      @UI.hidden: true
      @Semantics.language: true
  key language,
      value_low,
      @Semantics.text: true
      text
}
