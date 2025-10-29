@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@EndUserText.label: 'Projection View for ZR_CONFIGURATION'
define root view entity ZC_CONFIGURATION_V1
  provider contract transactional_query
  as projection on ZR_CONFIGURATION_V1
{
  key ZConfigurationUUID,
  @EndUserText.label: 'ID'
  ZID,
  @EndUserText.label: 'Grant Type'
  ZGrantType,
  @EndUserText.label: 'Client ID'
  ZClientID,
  @EndUserText.label: 'Client Secret'
  ZClientSecret,
  ZLastchangedate
  
}
